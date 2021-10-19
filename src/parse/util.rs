use super::*;

pub fn rule_to_str(rule: &Rule) -> &'static str {
    match rule {
        Rule::program => "program",
        Rule::func => "function",
        Rule::refine_type => "refinement type",
        Rule::int_refine_type => "refinement int type",
        Rule::func_refine_type => "refinement func type",
        Rule::term => "logical term",
        Rule::and_term => "logical and-term",
        Rule::imply_term => "logical imply-term",
        Rule::binop_term => "logical binary term",
        Rule::additive_term => "logical additive term",
        Rule::multive_term => "logical multive term",
        Rule::primary_term => "logical primary term",
        Rule::expr => "expression",
        Rule::let_expr => "let-expression",
        Rule::or_expr => "or-expression",
        Rule::and_expr => "and-expression",
        Rule::comp_expr => "comp-expression",
        Rule::additive_expr => "additive expression",
        Rule::multive_expr => "multive expression",
        Rule::apply_expr => "func-apply-expression",
        Rule::primary_expr => "primary expression",
        Rule::ifz_expr => "ifz-expression",
        Rule::constant => "constant",
        Rule::variable => "variable",
        Rule::paren_expr => "parened-expression",
        Rule::name => "identifier",

        Rule::left_paren => "'('",
        Rule::right_paren => "')'",
        Rule::left_brace => "'{'",
        Rule::right_brace => "'}'",
        Rule::plus => "'+'",
        Rule::minus => "'-'",
        Rule::ast => "'*'",
        Rule::slash => "'/'",
        Rule::percent => "'%'",
        Rule::or => "'||'",
        Rule::and => "'&&'",
        Rule::equal => "'='",
        Rule::eq => "'='",
        Rule::neq => "'!='",
        Rule::lt => "'<'",
        Rule::leq => "'<='",
        Rule::gt => "'>'",
        Rule::geq => "'>='",
        Rule::colon => "':'",
        Rule::comma => "','",
        Rule::bar => "'|'",
        Rule::arrow => "'->'",
        Rule::fat_arrow => "'=>'",

        Rule::kw_ifz => "'ifz'",
        Rule::kw_else => "'else'",
        Rule::kw_let => "'let'",
        Rule::kw_in => "'in'",
        Rule::kw_rec => "'rec'",
        Rule::kw_func => "'func'",
        Rule::kw_int => "'int'",
        Rule::kw_true => "'true'",
        Rule::kw_false => "'false'",
        Rule::EOI => "<eof>",

        rule => {
            eprintln!("{:?}", rule);
            unreachable!()
        }
    }
}

use pest::iterators::Pair;
pub fn pair_to_info<R: pest::RuleType>(pair: &Pair<R>) -> Info {
    let (start_line, start_col) = pair.as_span().start_pos().line_col();
    let (end_line, end_col) = pair.as_span().end_pos().line_col();
    Info::Range(
        Pos {
            line: start_line,
            col: start_col,
        },
        Pos {
            line: end_line,
            col: end_col,
        },
    )
}
