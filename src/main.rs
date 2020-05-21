#[macro_use]
extern crate lalrpop_util;

mod ast;
mod lexer;

lalrpop_mod!(pub java);

fn main() {
}

#[test]
fn test() {
    let input = " /* blah */\n package";
    let lexer = lexer::JavaLexer::new(input);
    println!("Parse result is {:?}",
        java::CompilationUnitParser::new().parse(input, lexer).unwrap());
    assert!(false);
//    println!("Parse result is {:?}", integer::DecimalIntegerLiteralParser::new().parse("12 L"));
//    assert!(integer::DecimalIntegerLiteralParser::new().parse("1234L").is_ok());
//    assert!(!integer::DecimalIntegerLiteralParser::new().parse("1234L").is_ok());
}
