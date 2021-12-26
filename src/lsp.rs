use crate::error::Location;
use lsp_server::{Connection, IoThreads, Message};
use lsp_types::{Diagnostic, Url};

pub fn run() {
    let (connection, io_threads) = init();
    for msg in &connection.receiver {
        if let Message::Request(request) = &msg {
            if connection.handle_shutdown(request).unwrap() {
                break;
            }
        }
        handle_msg(msg, &connection);
    }
    io_threads.join().unwrap();
}

fn init() -> (Connection, IoThreads) {
    let (connection, io_threads) = Connection::stdio();
    use lsp_types::{
        InitializeParams, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
        TextDocumentSyncOptions, TextDocumentSyncSaveOptions,
    };
    let mut text_document_sync_options = TextDocumentSyncOptions::default();
    text_document_sync_options.open_close = Some(true);
    text_document_sync_options.change = Some(TextDocumentSyncKind::FULL);
    text_document_sync_options.save = Some(TextDocumentSyncSaveOptions::Supported(true));

    let mut server_capabilities = ServerCapabilities::default();
    server_capabilities.text_document_sync = Some(TextDocumentSyncCapability::Options(
        text_document_sync_options,
    ));
    let server_capabilities = serde_json::to_value(server_capabilities)
        .expect("failed to convert server capabilities to json value..");
    let initialization_params: InitializeParams = serde_json::from_value(
        connection
            .initialize(server_capabilities)
            .expect("failed to initialize.."),
    )
    .expect("failed to convert json value to initialization_params..");

    let root_dir = initialization_params
        .root_uri
        .unwrap()
        .to_file_path()
        .unwrap();

    info!("starting yil language server at {}", root_dir.display());
    (connection, io_threads)
}

fn handle_msg(msg: Message, connection: &Connection) {
    match msg {
        Message::Notification(notification) => handle_notification(connection, notification),
        Message::Request(request) => handle_request(request),
        Message::Response(response) => handle_response(response),
    }
}

fn handle_notification(connection: &Connection, notif: lsp_server::Notification) {
    let _ = handle_notification_impl(connection, notif);
}

fn handle_notification_impl(
    connection: &Connection,
    notif: lsp_server::Notification,
) -> Result<(), ()> {
    use lsp_types::notification::{
        Cancel, DidChangeTextDocument, DidOpenTextDocument, DidSaveTextDocument, Exit, Initialized,
    };

    fn extract<T: lsp_types::notification::Notification>(
        connection: &Connection,
        notification: lsp_server::Notification,
        action: fn(connection: &Connection, T::Params),
    ) -> Result<lsp_server::Notification, ()> {
        use lsp_server::ExtractError;
        match notification.extract(T::METHOD) {
            Ok(params) => {
                action(connection, params);
                Err(())
            }
            Err(ExtractError::MethodMismatch(notif)) => return Ok(notif),
            Err(ExtractError::JsonError { .. }) => unreachable!(),
        }
    }

    let notif = extract::<Cancel>(connection, notif, |_, cancel| {
        warn!(
            "request {:?} was canceled, but currently cancellation is not implemented",
            cancel.id
        );
    })?;

    let notif = extract::<Initialized>(connection, notif, |_, _| info!("initialized!"))?;

    let notif = extract::<Exit>(connection, notif, |_, _| info!("exited!"))?;

    let notif = extract::<DidOpenTextDocument>(
        connection,
        notif,
        |connection, open_text_document_param| {
            let uri = open_text_document_param.text_document.uri;
            let text = open_text_document_param.text_document.text;
            info!("opened: {}", uri);

            send_publish_diagnostics(connection, uri.clone(), vec![]);
            let diagnostics = calc_diagnostic_notifications(text);
            send_publish_diagnostics(connection, uri, diagnostics)
        },
    )?;

    let notif = extract::<DidChangeTextDocument>(
        connection,
        notif,
        |connection, change_text_document_param| {
            let uri = change_text_document_param.text_document.uri;
            let text = change_text_document_param
                .content_changes
                .into_iter()
                .nth(0)
                .unwrap()
                .text;
            info!("changed: {}", uri);

            send_publish_diagnostics(connection, uri.clone(), vec![]);
            let diagnostics = calc_diagnostic_notifications(text);
            send_publish_diagnostics(connection, uri, diagnostics)
        },
    )?;

    let notif =
        extract::<DidSaveTextDocument>(connection, notif, |_, save_text_document_param| {
            let uri = save_text_document_param.text_document.uri;
            info!("saved: {}", uri);
        })?;

    warn!("unhangled notification: {:?}", notif);
    Ok(())
}

fn handle_request(req: lsp_server::Request) {
    warn!("unhandled request: {:?}", req);
}

fn handle_response(response: lsp_server::Response) {
    debug!("receive response: {:?}", response);
}

fn send_publish_diagnostics(connection: &Connection, uri: Url, diagnostics: Vec<Diagnostic>) {
    use lsp_types::{notification::PublishDiagnostics, PublishDiagnosticsParams};

    let params = PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    };
    let notif = lsp_server::Notification {
        method: <PublishDiagnostics as lsp_types::notification::Notification>::METHOD.to_string(),
        params: serde_json::value::to_value(params).expect("unreachable"),
    };
    if let Err(e) = connection.sender.send(Message::Notification(notif)) {
        warn!("failed to send notification: {}", e)
    }
}

use crate::env::NameEnv;
fn calc_diagnostic_notifications(text: String) -> Vec<Diagnostic> {
    use crate::error::Error;
    let mut diagnostics = vec![];

    fn to_diagnostic<E: Error>(err: E, name_env: &NameEnv) -> Diagnostic {
        let diag = err.to_diagnostic(name_env);
        let range = range_from_location(diag.loc);
        let msg = format!("[{}] {}", diag.kind, diag.msg);
        info!("msg: {}", msg);
        Diagnostic::new_simple(range, msg)
    }

    match crate::parse::program(&text) {
        Ok((program, name_env)) => {
            if let Err(e) = crate::typecheck::typecheck_program(&program) {
                diagnostics.push(to_diagnostic(e, &name_env));
            }
        }
        Err(e) => diagnostics.push(to_diagnostic(e, &NameEnv::empty())),
    }
    diagnostics
}

fn range_from_location(location: Location) -> lsp_types::Range {
    match location {
        Location::Position(pos) => lsp_types::Range {
            start: lsp_types::Position {
                line: pos.line as u32 - 1,
                character: pos.col as u32 - 1,
            },
            end: lsp_types::Position {
                line: pos.line as u32 - 1,
                character: pos.col as u32 - 1,
            },
        },
        Location::Range(range) => lsp_types::Range {
            start: lsp_types::Position {
                line: range.start.line as u32 - 1,
                character: range.start.col as u32 - 1,
            },
            end: lsp_types::Position {
                line: range.end.line as u32 - 1,
                character: range.end.col as u32 - 1,
            },
        },
    }
}
