use super::*;

pub fn rule_to_str(rule: &Rule) -> &'static str {
    match rule {
        Rule::program => "program",
        Rule::func => "function",
        Rule::refine_type => "refinement type",
        Rule::term => "logical term",
        Rule::primary_term => "logical primary term",
        Rule::expr => "expression",
        Rule::ifz_expr => "if-expression",
        Rule::let_expr => "let-expression",
        Rule::apply_expr => "func-apply-expression",
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
        Rule::eq => "'=='",
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

        _ => unreachable!(),
    }
}

pub fn line_of<'a>(src: &'a str, mut line_num: usize) -> String {
    let mut line = String::new();
    for c in src.chars() {
        if line_num == 0 && c == '\n' {
            return line;
        }
        if line_num == 0 {
            line.push(c);
        }
        if c == '\n' {
            line_num -= 1;
        }
    }
    line
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
