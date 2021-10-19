use std::io::{BufWriter, Write};

use crate::ast::Pos;

pub fn write_lines_in_range(
    out: &mut BufWriter<std::io::StderrLock>,
    src: &Vec<&str>,
    range: &(Pos, Pos),
) -> Result<(), std::io::Error> {
    // clamp the range within src
    let mut range = range.clone();
    if src.len() < range.1.line {
        range.1.line = src.len();
        range.1.col = src[src.len() - 1].len() + 1;
    }

    assert!(range.0.line <= range.1.line);

    if range.0.line == range.1.line {
        write_single_line_in_range(out, src, range.0.line, range.0.col, range.1.col)
    } else {
        write_multi_lines_in_range(out, src, &range)
    }
}

fn write_single_line_in_range(
    out: &mut BufWriter<std::io::StderrLock>,
    src: &Vec<&str>,
    line: usize,
    start_col: usize,
    end_col: usize,
) -> Result<(), std::io::Error> {
    assert!(1 <= line && line <= src.len());
    assert!(start_col <= end_col);
    assert!(end_col <= src[line - 1].len() + 1);

    let line_number_width = line.to_string().len();

    writeln!(
        out,
        "{} | ",
        std::iter::repeat(' ')
            .take(line_number_width)
            .collect::<String>()
    )?;
    writeln!(out, "{} | {}", line, src[line - 1])?;
    write!(
        out,
        "{} | {}{}",
        std::iter::repeat(' ')
            .take(line_number_width)
            .collect::<String>(),
        std::iter::repeat(' ')
            .take(start_col - 1)
            .collect::<String>(),
        std::iter::repeat('-')
            .take(end_col - start_col)
            .collect::<String>(),
    )
}

fn write_multi_lines_in_range(
    out: &mut BufWriter<std::io::StderrLock>,
    src: &Vec<&str>,
    range: &(Pos, Pos),
) -> Result<(), std::io::Error> {
    assert!(range.0.line < range.1.line);
    assert!(range.1.line <= src.len());

    let line_number_width = std::cmp::max(
        range.0.line.to_string().len(),
        range.1.line.to_string().len(),
    );

    writeln!(
        out,
        "{} | ",
        std::iter::repeat(' ')
            .take(line_number_width)
            .collect::<String>()
    )?;
    for line_number in range.0.line..(range.1.line + 1) {
        let line = src[line_number - 1];
        writeln!(
            out,
            "{:<width$} | {}",
            line_number,
            line,
            width = line_number_width
        )?;
        write!(
            out,
            "{} | ",
            std::iter::repeat(' ')
                .take(line_number_width)
                .collect::<String>()
        )?;
        if line_number == range.0.line {
            writeln!(
                out,
                "{}{}",
                std::iter::repeat(' ')
                    .take(range.0.col - 1)
                    .collect::<String>(),
                std::iter::repeat('-')
                    .take(line.len() + 1 - range.0.col)
                    .collect::<String>(),
            )?;
        } else if line_number == range.1.line {
            write!(
                out,
                "{}",
                std::iter::repeat('-')
                    .take(range.1.col - 1)
                    .collect::<String>()
            )?;
        } else {
            writeln!(
                out,
                "{}",
                std::iter::repeat('-').take(line.len()).collect::<String>()
            )?;
        }
    }
    write!(out, "")
}

pub fn write_line_at_pos(
    out: &mut BufWriter<std::io::StderrLock>,
    src: &Vec<&str>,
    pos: &Pos,
) -> Result<(), std::io::Error> {
    write_single_line_in_range(out, src, pos.line, pos.col, pos.col)
}
