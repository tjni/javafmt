use lazy_static::lazy_static;
use regex::{Match, Regex};

#[derive(Clone, Copy, Debug)]
pub struct Position {
    pub line   : usize,
    pub column : usize,
}

#[derive(Clone, Copy, Debug)]
pub enum Token<'input> {
    //
    // § 3.6. White Space
    //
    EndOfLine(&'input str),

    //
    // § 3.7. Comments
    //
    BlockComment(&'input str),
    LineComment(&'input str),

    //
    // § 3.8. Identifiers
    //
    Identifier(&'input str),

    //
    // § 3.9. Keywords
    //
    AbstractKeyword,
    AssertKeyword,
    BooleanKeyword,
    BreakKeyword,
    ByteKeyword,
    CaseKeyword,
    CatchKeyword,
    CharKeyword,
    ClassKeyword,
    ConstKeyword,
    ContinueKeyword,
    DefaultKeyword,
    DoKeyword,
    DoubleKeyword,
    ElseKeyword,
    EnumKeyword,
    ExtendsKeyword,
    FinalKeyword,
    FinallyKeyword,
    FloatKeyword,
    ForKeyword,
    IfKeyword,
    GotoKeyword,
    ImplementsKeyword,
    ImportKeyword,
    InstanceofKeyword,
    IntKeyword,
    InterfaceKeyword,
    LongKeyword,
    NativeKeyword,
    NewKeyword,
    PackageKeyword,
    PrivateKeyword,
    ProtectedKeyword,
    PublicKeyword,
    ReturnKeyword,
    ShortKeyword,
    StaticKeyword,
    StrictfpKeyword,
    SuperKeyword,
    SwitchKeyword,
    SynchronizedKeyword,
    ThisKeyword,
    ThrowKeyword,
    ThrowsKeyword,
    TransientKeyword,
    TryKeyword,
    VoidKeyword,
    VolatileKeyword,
    WhileKeyword,

    //
    // § 3.9. Restricted Keywords
    //
    ExportsKeyword,
    ModuleKeyword,
    OpenKeyword,
    OpensKeyword,
    ProvidesKeyword,
    RequiresKeyword,
    ToKeyword,
    TransitiveKeyword,
    UsesKeyword,
    WithKeyword,

    //
    // § 3.10. Literals
    //
    DecimalIntegerLiteral(&'input str),
    HexIntegerLiteral(&'input str),
    OctalIntegerLiteral(&'input str),
    BinaryIntegerLiteral(&'input str),
    DecimalFloatingPointLiteral(&'input str),
    HexFloatingPointLiteral(&'input str),
    BooleanLiteral(&'input str),
    CharacterLiteral(&'input str),
    StringLiteral(&'input str),
    NullLiteral,

    // 
    // § 3.11. Separators
    //
    LeftParenSeparator,
    RightParenSeparator,
    LeftBraceSeparator,
    RightBraceSeparator,
    LeftBracketSeparator,
    RightBracketSeparator,
    SemicolonSeparator,
    CommaSeparator,
    EllipsisSeparator,
    DotSeparator,
    AtSeparator,
    DoubleColonSeparator,

    //
    // § 3.12. Operators
    //
    EqualOperator,
    AssignOperator,
    NotEqualOperator,
    NotOperator,
    UnsignedRightShiftAssignOperator,
    UnsignedRightShiftOperator,
    RightShiftAssignOperator,
    RightShiftOperator,
    GreaterEqualOperator,
    GreaterOperator,
    LeftShiftAssignOperator,
    LeftShiftOperator,
    LessEqualOperator,
    LessOperator,
    IncrementOperator,
    PlusAssignOperator,
    PlusOperator,
    ArrowOperator,
    DecrementOperator,
    MinusAssignOperator,
    MinusOperator,
    TimesAssignOperator,
    TimesOperator,
    DivideAssignOperator,
    DivideOperator,
    ModAssignOperator,
    ModOperator,
    ComplementOperator,
    ConditionalAndOperator,
    BitAndAssignOperator,
    BitAndOperator,
    ConditionalOrOperator,
    BitOrAssignOperator,
    BitOrOperator,
    BitXorAssignOperator,
    BitXorOperator,
    TernaryIfOperator,
    TernaryElseOperator,
}

#[derive(Clone, Copy, Debug)]
pub struct LocatableToken<'input>(pub Position, pub Token<'input>);

#[derive(Clone, Copy, Debug)]
pub struct LexicalError {
    pub position : Position
}

pub struct JavaLexer<'input> {
    input  : &'input str,
    offset : usize,
    line   : usize,
    column : usize,
}

// 
// § 3.6. White Space
//
lazy_static! {
    //
    // Spaces that we ignore when lexing.
    //
    //   the ASCII SP character, also known as "space"
    //   the ASCII HT character, also known as "horizontal tab"
    //   the ASCII FF character, also known as "form feed"
    //
    static ref SPACES: Regex = Regex::new(r"^[ \t\f]+").unwrap();

    //
    // This regular expression matches the following productions.
    //
    // LineTerminator:
    //   the ASCII LF character, also known as "newline"
    //   the ASCII CR character, also known as "return"
    //   the ASCII CR character followed by the ASCII LF character
    //
    static ref END_OF_LINE: Regex = Regex::new(r"^(?:\n|\r|\r\n)").unwrap();
}

// 
// § 3.7. Comments
//
lazy_static! {
    //
    // This regular expression matches the following productions.
    //
    // TraditionalComment:
    //   / * CommentTail
    //
    // CommentTail:
    //   * CommentTailStar
    //   NotStar CommentTail
    //
    // CommentTailStar:
    //   /
    //   * CommentTailStar
    //   NotStarNotSlash CommentTail
    //
    // NotStar:
    //   InputCharacter but not *
    //   LineTerminator
    //
    // NotStarNotSlash:
    //   InputCharacter but not * or /
    //   LineTerminator
    //
    static ref BLOCK_COMMENT: Regex = Regex::new(r"^/\*[.\n]*?\*/").unwrap();

    //
    // This regular expression matches the following productions.
    //
    // EndOfLineComment:
    //   / / {InputCharacter}
    //
    static ref LINE_COMMENT: Regex = Regex::new("^//.*").unwrap();
}

//
// § 3.8. Identifiers
//
lazy_static! {
    //
    // This regular expression matches the following productions.
    //
    // Identifier:
    //   IdentifierChars but not a Keyword or BooleanLiteral or NullLiteral
    //
    // IdentifierChars:
    //   JavaLetter {JavaLetterOrDigit}
    //
    // JavaLetter:
    //   any Unicode character that is a "Java letter"
    //
    // JavaLetterOrDigit:
    //   any Unicode character that is a "Java letter-or-digit"
    //
    // We do not exclude keywords and literals. Instead, we will make sure to
    // look for those before looking for an identifier in the input stream.
    //
    // TODO: Handle Unicode characters that are valid identifier characters.
    //
    static ref IDENTIFIER: Regex = Regex::new(
        r"^[a-zA-Z\$_][a-zA-Z\$_0-9]*").unwrap();
}

// 
// § 3.9. Keywords
//
lazy_static! {

    static ref ABSTRACT    : Regex = Regex::new(r"^abstract\b"    ).unwrap();
    static ref ASSERT      : Regex = Regex::new(r"^assert\b"      ).unwrap();
    static ref BOOLEAN     : Regex = Regex::new(r"^boolean\b"     ).unwrap();
    static ref BREAK       : Regex = Regex::new(r"^break\b"       ).unwrap();
    static ref BYTE        : Regex = Regex::new(r"^byte\b"        ).unwrap();
    static ref CASE        : Regex = Regex::new(r"^case\b"        ).unwrap();
    static ref CATCH       : Regex = Regex::new(r"^catch\b"       ).unwrap();
    static ref CHAR        : Regex = Regex::new(r"^char\b"        ).unwrap();
    static ref CLASS       : Regex = Regex::new(r"^class\b"       ).unwrap();
    static ref CONST       : Regex = Regex::new(r"^const\b"       ).unwrap();
    static ref CONTINUE    : Regex = Regex::new(r"^continue\b"    ).unwrap();
    static ref DEFAULT     : Regex = Regex::new(r"^default\b"     ).unwrap();
    static ref DO          : Regex = Regex::new(r"^do\b"          ).unwrap();
    static ref DOUBLE      : Regex = Regex::new(r"^double\b"      ).unwrap();
    static ref ELSE        : Regex = Regex::new(r"^else\b"        ).unwrap();
    static ref ENUM        : Regex = Regex::new(r"^enum\b"        ).unwrap();
    static ref EXTENDS     : Regex = Regex::new(r"^extends\b"     ).unwrap();
    static ref FINAL       : Regex = Regex::new(r"^final\b"       ).unwrap();
    static ref FINALLY     : Regex = Regex::new(r"^finally\b"     ).unwrap();
    static ref FLOAT       : Regex = Regex::new(r"^float\b"       ).unwrap();
    static ref FOR         : Regex = Regex::new(r"^for\b"         ).unwrap();
    static ref IF          : Regex = Regex::new(r"^if\b"          ).unwrap();
    static ref GOTO        : Regex = Regex::new(r"^goto\b"        ).unwrap();
    static ref IMPLEMENTS  : Regex = Regex::new(r"^implements\b"  ).unwrap();
    static ref IMPORT      : Regex = Regex::new(r"^import\b"      ).unwrap();
    static ref INSTANCEOF  : Regex = Regex::new(r"^instanceof\b"  ).unwrap();
    static ref INT         : Regex = Regex::new(r"^int\b"         ).unwrap();
    static ref INTERFACE   : Regex = Regex::new(r"^interface\b"   ).unwrap();
    static ref LONG        : Regex = Regex::new(r"^long\b"        ).unwrap();
    static ref NATIVE      : Regex = Regex::new(r"^native\b"      ).unwrap();
    static ref NEW         : Regex = Regex::new(r"^new\b"         ).unwrap();
    static ref PACKAGE     : Regex = Regex::new(r"^package\b"     ).unwrap();
    static ref PRIVATE     : Regex = Regex::new(r"^private\b"     ).unwrap();
    static ref PROTECTED   : Regex = Regex::new(r"^protected\b"   ).unwrap();
    static ref PUBLIC      : Regex = Regex::new(r"^public\b"      ).unwrap();
    static ref RETURN      : Regex = Regex::new(r"^return\b"      ).unwrap();
    static ref SHORT       : Regex = Regex::new(r"^short\b"       ).unwrap();
    static ref STATIC      : Regex = Regex::new(r"^static\b"      ).unwrap();
    static ref STRICTFP    : Regex = Regex::new(r"^strictfp\b"    ).unwrap();
    static ref SUPER       : Regex = Regex::new(r"^super\b"       ).unwrap();
    static ref SWITCH      : Regex = Regex::new(r"^switch\b"      ).unwrap();
    static ref SYNCHRONIZED: Regex = Regex::new(r"^synchronized\b").unwrap();
    static ref THIS        : Regex = Regex::new(r"^this\b"        ).unwrap();
    static ref THROW       : Regex = Regex::new(r"^throw\b"       ).unwrap();
    static ref THROWS      : Regex = Regex::new(r"^throws\b"      ).unwrap();
    static ref TRANSIENT   : Regex = Regex::new(r"^transient\b"   ).unwrap();
    static ref TRY         : Regex = Regex::new(r"^try\b"         ).unwrap();
    static ref VOID        : Regex = Regex::new(r"^void\b"        ).unwrap();
    static ref VOLATILE    : Regex = Regex::new(r"^volatile\b"    ).unwrap();
    static ref WHILE       : Regex = Regex::new(r"^while\b"       ).unwrap();

    //
    // Restricted Keywords
    //
    static ref EXPORTS     : Regex = Regex::new(r"^exports\b"     ).unwrap();
    static ref MODULE      : Regex = Regex::new(r"^module\b"      ).unwrap();
    static ref OPEN        : Regex = Regex::new(r"^open\b"        ).unwrap();
    static ref OPENS       : Regex = Regex::new(r"^opens\b"       ).unwrap();
    static ref PROVIDES    : Regex = Regex::new(r"^provides\b"    ).unwrap();
    static ref REQUIRES    : Regex = Regex::new(r"^requires\b"    ).unwrap();
    static ref TO          : Regex = Regex::new(r"^to\b"          ).unwrap();
    static ref TRANSITIVE  : Regex = Regex::new(r"^transitive\b"  ).unwrap();
    static ref USES        : Regex = Regex::new(r"^uses\b"        ).unwrap();
    static ref WITH        : Regex = Regex::new(r"^with\b"        ).unwrap();
}

// 
// § 3.11. Separators
//
lazy_static! {

    static ref LEFT_PAREN   : Regex = Regex::new(r"\("   ).unwrap();
    static ref RIGHT_PAREN  : Regex = Regex::new(r"\)"   ).unwrap();
    static ref LEFT_BRACE   : Regex = Regex::new(r"\{"   ).unwrap();
    static ref RIGHT_BRACE  : Regex = Regex::new(r"\}"   ).unwrap();
    static ref LEFT_BRACKET : Regex = Regex::new(r"\["   ).unwrap();
    static ref RIGHT_BRACKET: Regex = Regex::new(r"\]"   ).unwrap();
    static ref SEMICOLON    : Regex = Regex::new(r";"    ).unwrap();
    static ref COMMA        : Regex = Regex::new(r","    ).unwrap();
    static ref ELLIPSIS     : Regex = Regex::new(r"\.{3}").unwrap();
    static ref DOT          : Regex = Regex::new(r"\."   ).unwrap();
    static ref AT           : Regex = Regex::new(r"@"    ).unwrap();
    static ref DOUBLE_COLON : Regex = Regex::new(r"::"   ).unwrap();
}

//
// § 3.12. Operators
//
lazy_static! {

    static ref EQUAL              : Regex = Regex::new("^=="   ).unwrap();
    static ref ASSIGN             : Regex = Regex::new("^="    ).unwrap();
    static ref NOT_EQUAL          : Regex = Regex::new("^!="   ).unwrap();
    static ref NOT                : Regex = Regex::new("^!"    ).unwrap();
    static ref URIGHT_SHIFT_ASSIGN: Regex = Regex::new("^>>>=" ).unwrap();
    static ref URIGHT_SHIFT       : Regex = Regex::new("^>>>"  ).unwrap();
    static ref RIGHT_SHIFT_ASSIGN : Regex = Regex::new("^>>="  ).unwrap();
    static ref RIGHT_SHIFT        : Regex = Regex::new("^>>"   ).unwrap();
    static ref GREATER_EQUAL      : Regex = Regex::new("^>="   ).unwrap();
    static ref GREATER            : Regex = Regex::new("^>"    ).unwrap();
    static ref LEFT_SHIFT_ASSIGN  : Regex = Regex::new("^<<="  ).unwrap();
    static ref LEFT_SHIFT         : Regex = Regex::new("^<<"   ).unwrap();
    static ref LESS_EQUAL         : Regex = Regex::new("^<="   ).unwrap();
    static ref LESS               : Regex = Regex::new("^<"    ).unwrap();
    static ref INCREMENT          : Regex = Regex::new(r"^\+\+").unwrap();
    static ref PLUS_ASSIGN        : Regex = Regex::new(r"^\+=" ).unwrap();
    static ref PLUS               : Regex = Regex::new(r"^\+"  ).unwrap();
    static ref ARROW              : Regex = Regex::new("^->"   ).unwrap();
    static ref DECREMENT          : Regex = Regex::new("^--"   ).unwrap();
    static ref MINUS_ASSIGN       : Regex = Regex::new("^-="   ).unwrap();
    static ref MINUS              : Regex = Regex::new("^-"    ).unwrap();
    static ref TIMES_ASSIGN       : Regex = Regex::new(r"^\*=" ).unwrap();
    static ref TIMES              : Regex = Regex::new(r"^\*"  ).unwrap();
    static ref DIVIDE_ASSIGN      : Regex = Regex::new("^/="   ).unwrap();
    static ref DIVIDE             : Regex = Regex::new("^/"    ).unwrap();
    static ref MOD_ASSIGN         : Regex = Regex::new("^%="   ).unwrap();
    static ref MOD                : Regex = Regex::new("^%"    ).unwrap();
    static ref COMPLEMENT         : Regex = Regex::new("^~"    ).unwrap();
    static ref CONDITIONAL_AND    : Regex = Regex::new("^&&"   ).unwrap();
    static ref BIT_AND_ASSIGN     : Regex = Regex::new("^&="   ).unwrap();
    static ref BIT_AND            : Regex = Regex::new("^&"    ).unwrap();
    static ref CONDITIONAL_OR     : Regex = Regex::new(r"^\|\|").unwrap();
    static ref BIT_OR_ASSIGN      : Regex = Regex::new(r"^\|=" ).unwrap();
    static ref BIT_OR             : Regex = Regex::new(r"^\|"  ).unwrap();
    static ref BIT_XOR_ASSIGN     : Regex = Regex::new(r"^\^=" ).unwrap();
    static ref BIT_XOR            : Regex = Regex::new(r"^\^"  ).unwrap();
    static ref TERNARY_IF         : Regex = Regex::new(r"^\?"  ).unwrap();
    static ref TERNARY_ELSE       : Regex = Regex::new("^:"    ).unwrap();
}

// 
// § 3.10.1. Integer Literals
//

//
// This regular expression matches the following productions.
//
// NonZeroDigit:
//   (one of)
//   1 2 3 4 5 6 7 8 9
//
// Digits:
//   Digit
//   Digit [DigitsAndUnderscores] Digit
//
// Digit:
//   0
//   NonZeroDigit
//
// DigitsAndUnderscores:
//   DigitOrUnderscore {DigitOrUnderscore}
//
// DigitOrUnderscore:
//   Digit
//   _
//
const DIGITS: &str = "(?:[0-9](?:[0-9_]*[0-9])?)";

//
// This regular expression matches the following productions.
//
// HexDigits:
//   HexDigit
//   HexDigit [HexDigitsAndUnderscores] HexDigit
//
// HexDigit:
//   (one of)
//   0 1 2 3 4 5 6 7 8 9 a b c d e f A B C D E F
//
// HexDigitsAndUnderscores:
//   HexDigitOrUnderscore {HexDigitOrUnderscore}
//
// HexDigitOrUnderscore:
//   HexDigit
//   _
//
const HEX_DIGITS: &str = "(?:[0-9a-fA-F](?:[0-9a-fA-F_]*[0-9a-fA-F])?)";

lazy_static! {
    //
    // This regular expression matches the following productions.
    //
    // DecimalIntegerLiteral:
    //   DecimalNumeral [IntegerTypeSuffix]
    //
    // IntegerTypeSuffix:
    //   (one of)
    //   l L
    //
    // DecimalNumeral:
    //   0
    //   NonZeroDigit [Digits]
    //   NonZeroDigit Underscores Digits
    //
    // Underscores:
    //   _ {_}
    //
    static ref DECIMAL_INTEGER_LITERAL: Regex = Regex::new(
        r"^(?:0|[1-9](?:[0-9_]*[0-9])?[lL]?)\b").unwrap();

    //
    // This regular expression matches the following productions.
    //
    // HexIntegerLiteral:
    //   HexNumeral [IntegerTypeSuffix]
    //
    // HexNumeral:
    //   0 x HexDigits
    //   0 X HexDigits
    //
    static ref HEX_INTEGER_LITERAL: Regex = Regex::new(&format!(
        r"^0[xX]{HEX_DIGITS}[lL]?\b", HEX_DIGITS = HEX_DIGITS)[..]).unwrap();
        
    //
    // This regular expression matches the following productions.
    //
    // OctalIntegerLiteral:
    //   OctalNumeral [IntegerTypeSuffix]
    //
    // OctalNumeral:
    //   0 OctalDigits
    //   0 Underscores OctalDigits
    //
    // OctalDigits:
    //   OctalDigit
    //   OctalDigit [OctalDigitsAndUnderscores] OctalDigit
    //
    // OctalDigit:
    //   (one of)
    //   0 1 2 3 4 5 6 7
    //
    // OctalDigitsAndUnderscores:
    //   OctalDigitOrUnderscore {OctalDigitOrUnderscore}
    //
    // OctalDigitOrUnderscore:
    //   OctalDigit
    //   _
    //
    static ref OCTAL_INTEGER_LITERAL: Regex = Regex::new(
        r"^0(?:[0-7_]*[0-7])?[lL]?\b").unwrap();

    //
    // This regular expression matches the following productions.
    //
    // BinaryIntegerLiteral:
    //   BinaryNumeral [IntegerTypeSuffix]
    //
    // BinaryNumeral:
    //   0 b BinaryDigits
    //   0 B BinaryDigits
    //
    // BinaryDigits:
    //   BinaryDigit
    //   BinaryDigit [BinaryDigitsAndUnderscores] BinaryDigit
    //
    // BinaryDigit:
    //   (one of)
    //   0 1
    //
    // BinaryDigitsAndUnderscores:
    //   BinaryDigitOrUnderscore {BinaryDigitOrUnderscore}
    //
    // BinaryDigitOrUnderscore:
    //   BinaryDigit
    //   _
    //
    static ref BINARY_INTEGER_LITERAL: Regex = Regex::new(
        r"^0[bB][01](?:[01_]*[01])?[lL]?\b").unwrap();
}

// 
// § 3.10.2. Floating-Point Literals
//
lazy_static! {
    //
    // This regular expression matches the following productions.
    //
    // ExponentPart:
    //   ExponentIndicator SignedInteger
    //
    // ExponentIndicator:
    //   (one of)
    //   e E
    //
    // SignedInteger:
    //   [Sign] Digits
    //
    // Sign:
    //   (one of)
    //   + -
    //
    static ref EXPONENT_PART: String = format!(
        r"(?:[eE][\+\-]?{DIGITS})",
        DIGITS = DIGITS);

    //
    // This regular expression matches the following productions.
    //
    // BinaryExponent:
    //   BinaryExponentIndicator SignedInteger
    //
    // BinaryExponentIndicator:
    //   (one of)
    //   p P
    // 
    static ref BINARY_EXPONENT: String = format!(
        r"(?:[pP][\+\-]?{DIGITS})",
        DIGITS = DIGITS);


    //
    // This regular expression matches the following productions.
    //
    // DecimalFloatingPointLiteral:
    //   Digits . [Digits] [ExponentPart] [FloatTypeSuffix]
    //   . Digits [ExponentPart] [FloatTypeSuffix]
    //   Digits ExponentPart [FloatTypeSuffix]
    //   Digits [ExponentPart] FloatTypeSuffix
    //
    // FloatTypeSuffix:
    //   (one of)
    //   f F d D
    //
    static ref DECIMAL_FLOATING_POINT_LITERAL: Regex = Regex::new(&format!(concat!(
        r"^(?:",
        r"{DIGITS}\.{DIGITS}?{EXPONENT_PART}?[fFdD]?|",
        r"\.{DIGITS}{EXPONENT_PART}?[fFdD]?|",
        r"{DIGITS}{EXPONENT_PART}[fFdD]?|",
        r"{DIGITS}[fFdD]",
        r")\b"),
        DIGITS = DIGITS,
        EXPONENT_PART = *EXPONENT_PART)[..]).unwrap();

    //
    // This regular expression matches the following productions.
    //
    // HexadecimalFloatingPointLiteral:
    //   HexSignificand BinaryExponent [FloatTypeSuffix]
    //
    // HexSignificand:
    //   HexNumeral [.]
    //   0 x [HexDigits] . HexDigits
    //   0 X [HexDigits] . HexDigits
    //
    static ref HEX_FLOATING_POINT_LITERAL: Regex = Regex::new(&format!(concat!(
        r"^0[xX](?:{HEX_DIGITS}\.?|{HEX_DIGITS}?\.{HEX_DIGITS})",
        r"{BINARY_EXPONENT}[fFdD]?\b"),
        HEX_DIGITS = HEX_DIGITS,
        BINARY_EXPONENT = *BINARY_EXPONENT)[..]).unwrap();
}

// 
// § 3.10.3. Boolean Literals
//
lazy_static! {
    //
// This regular expression matches the following production.
    //
    // BooleanLiteral:
    //   (one of)
    //   true false
    //
    static ref BOOLEAN_LITERAL: Regex = Regex::new(
        r"^(?:true|false)\b").unwrap();
}

// 
// § 3.10.6. Escape Sequences for Character and String Literals
//

//
// This regular expression matches the following productions.
//
// EscapeSequence:
//   \ b (backspace BS, Unicode \u0008)
//   \ t (horizontal tab HT, Unicode \u0009)
//   \ n (linefeed LF, Unicode \u000a)
//   \ f (form feed FF, Unicode \u000c)
//   \ r (carriage return CR, Unicode \u000d)
//   \ " (double quote ", Unicode \u0022)
//   \ ' (single quote ', Unicode \u0027)
//   \ \ (backslash \, Unicode \u005c)
//   OctalEscape (octal value, Unicode \u0000 to \u00ff)
//
// OctalEscape:
//   \ OctalDigit
//   \ OctalDigit OctalDigit
//   \ ZeroToThree OctalDigit OctalDigit
//
// ZeroToThree:
//   (one of)
//   0 1 2 3
//
const ESCAPE_SEQUENCE: &str = r#"\\(?:[bfnrt"'\\]|[0-7][0-7]?|[0-3][0-7]{2})"#;

// 
// § 3.10.4. Character Literals
//
lazy_static! {
    //
    // This regular expression matches the following productions.
    //
    // CharacterLiteral:
    //   ' SingleCharacter '
    //   ' EscapeSequence '
    //
    // SingleCharacter:
    //   InputCharacter but not ' or \
    //
    static ref CHARACTER_LITERAL: Regex = Regex::new(&format!(
        r"^'(?:[^'\\\r\n]|{ESCAPE_SEQUENCE})'",
        ESCAPE_SEQUENCE = ESCAPE_SEQUENCE)[..]).unwrap();
}

// 
// § 3.10.5. String Literals
//
lazy_static! {
    //
    // This regular expression matches the following productions.
    //
    // StringLiteral:
    //   " {StringCharacter} "
    //
    // StringCharacter:
    //   InputCharacter but not " or \
    //   EscapeSequence
    //
    static ref STRING_LITERAL: Regex = Regex::new(&format!(
        r#"^"(?:[^"\\\r\n]|{ESCAPE_SEQUENCE}")+""#,
        ESCAPE_SEQUENCE = ESCAPE_SEQUENCE)[..]).unwrap();
}

// 
// § 3.10.7. The Null Literal
//
lazy_static! {
    //
    // This regular expression matches the following productions.
    //
    // NullLiteral:
    //   null
    //
    static ref NULL_LITERAL: Regex = Regex::new(r"^null\b").unwrap();
}

//
// Miscellaneous
//
lazy_static! {
    //
    // This regular expression matches line terminator combinations, but it does
    // not require them to be at the start of the input.
    //
    static ref END_OF_LINE_CHARS: Regex = Regex::new(r"\r?\n?").unwrap();
}

impl<'input> JavaLexer<'input> {
    pub fn new(input: &'input str) -> JavaLexer<'input> {
        JavaLexer {
            input,
            offset : 0,
            line   : 1,
            column : 0,
        }
    }

    fn next_token(&mut self) -> Option<Token<'input>> {
        if let Some(mat) = self.consume(&END_OF_LINE) {
            Some(self.handle_end_of_line(&mat))
        } else if let Some(mat) = self.consume(&BLOCK_COMMENT) {
            Some(self.handle_block_comment(&mat))
        } else if let Some(mat) = self.consume_without_eols(&LINE_COMMENT) {
            Some(Token::LineComment(mat.as_str()))
        } else if let Some(_) = self.consume_without_eols(&ABSTRACT) {
            Some(Token::AbstractKeyword)
        } else if let Some(_) = self.consume_without_eols(&ASSERT) {
            Some(Token::AssertKeyword)
        } else if let Some(_) = self.consume_without_eols(&BOOLEAN) {
            Some(Token::BooleanKeyword)
        } else if let Some(_) = self.consume_without_eols(&BREAK) {
            Some(Token::BreakKeyword)
        } else if let Some(_) = self.consume_without_eols(&BYTE) {
            Some(Token::ByteKeyword)
        } else if let Some(_) = self.consume_without_eols(&CASE) {
            Some(Token::CaseKeyword)
        } else if let Some(_) = self.consume_without_eols(&CATCH) {
            Some(Token::CatchKeyword)
        } else if let Some(_) = self.consume_without_eols(&CHAR) {
            Some(Token::CharKeyword)
        } else if let Some(_) = self.consume_without_eols(&CLASS) {
            Some(Token::ClassKeyword)
        } else if let Some(_) = self.consume_without_eols(&CONST) {
            Some(Token::ConstKeyword)
        } else if let Some(_) = self.consume_without_eols(&CONTINUE) {
            Some(Token::ContinueKeyword)
        } else if let Some(_) = self.consume_without_eols(&DEFAULT) {
            Some(Token::DefaultKeyword)
        } else if let Some(_) = self.consume_without_eols(&DO) {
            Some(Token::DoKeyword)
        } else if let Some(_) = self.consume_without_eols(&DOUBLE) {
            Some(Token::DoubleKeyword)
        } else if let Some(_) = self.consume_without_eols(&ELSE) {
            Some(Token::ElseKeyword)
        } else if let Some(_) = self.consume_without_eols(&ENUM) {
            Some(Token::EnumKeyword)
        } else if let Some(_) = self.consume_without_eols(&EXTENDS) {
            Some(Token::ExtendsKeyword)
        } else if let Some(_) = self.consume_without_eols(&FINAL) {
            Some(Token::FinalKeyword)
        } else if let Some(_) = self.consume_without_eols(&FINALLY) {
            Some(Token::FinallyKeyword)
        } else if let Some(_) = self.consume_without_eols(&FLOAT) {
            Some(Token::FloatKeyword)
        } else if let Some(_) = self.consume_without_eols(&FOR) {
            Some(Token::ForKeyword)
        } else if let Some(_) = self.consume_without_eols(&IF) {
            Some(Token::IfKeyword)
        } else if let Some(_) = self.consume_without_eols(&GOTO) {
            Some(Token::GotoKeyword)
        } else if let Some(_) = self.consume_without_eols(&IMPORT) {
            Some(Token::ImportKeyword)
        } else if let Some(_) = self.consume_without_eols(&INSTANCEOF) {
            Some(Token::InstanceofKeyword)
        } else if let Some(_) = self.consume_without_eols(&INT) {
            Some(Token::IntKeyword)
        } else if let Some(_) = self.consume_without_eols(&INTERFACE) {
            Some(Token::InterfaceKeyword)
        } else if let Some(_) = self.consume_without_eols(&LONG) {
            Some(Token::LongKeyword)
        } else if let Some(_) = self.consume_without_eols(&NATIVE) {
            Some(Token::NativeKeyword)
        } else if let Some(_) = self.consume_without_eols(&NEW) {
            Some(Token::NewKeyword)
        } else if let Some(_) = self.consume_without_eols(&PACKAGE) {
            Some(Token::PackageKeyword)
        } else if let Some(_) = self.consume_without_eols(&PRIVATE) {
            Some(Token::PrivateKeyword)
        } else if let Some(_) = self.consume_without_eols(&PROTECTED) {
            Some(Token::ProtectedKeyword)
        } else if let Some(_) = self.consume_without_eols(&PUBLIC) {
            Some(Token::PublicKeyword)
        } else if let Some(_) = self.consume_without_eols(&RETURN) {
            Some(Token::ReturnKeyword)
        } else if let Some(_) = self.consume_without_eols(&SHORT) {
            Some(Token::ShortKeyword)
        } else if let Some(_) = self.consume_without_eols(&STATIC) {
            Some(Token::StaticKeyword)
        } else if let Some(_) = self.consume_without_eols(&STRICTFP) {
            Some(Token::StrictfpKeyword)
        } else if let Some(_) = self.consume_without_eols(&SUPER) {
            Some(Token::SuperKeyword)
        } else if let Some(_) = self.consume_without_eols(&SWITCH) {
            Some(Token::SwitchKeyword)
        } else if let Some(_) = self.consume_without_eols(&SYNCHRONIZED) {
            Some(Token::SynchronizedKeyword)
        } else if let Some(_) = self.consume_without_eols(&THIS) {
            Some(Token::ThisKeyword)
        } else if let Some(_) = self.consume_without_eols(&THROW) {
            Some(Token::ThrowKeyword)
        } else if let Some(_) = self.consume_without_eols(&THROWS) {
            Some(Token::ThrowsKeyword)
        } else if let Some(_) = self.consume_without_eols(&TRANSIENT) {
            Some(Token::TransientKeyword)
        } else if let Some(_) = self.consume_without_eols(&TRY) {
            Some(Token::TryKeyword)
        } else if let Some(_) = self.consume_without_eols(&VOID) {
            Some(Token::VoidKeyword)
        } else if let Some(_) = self.consume_without_eols(&VOLATILE) {
            Some(Token::VolatileKeyword)
        } else if let Some(_) = self.consume_without_eols(&WHILE) {
            Some(Token::WhileKeyword)
        } else if let Some(_) = self.consume_without_eols(&EXPORTS) {
            Some(Token::ExportsKeyword)
        } else if let Some(_) = self.consume_without_eols(&MODULE) {
            Some(Token::ModuleKeyword)
        } else if let Some(_) = self.consume_without_eols(&OPEN) {
            Some(Token::OpenKeyword)
        } else if let Some(_) = self.consume_without_eols(&OPENS) {
            Some(Token::OpensKeyword)
        } else if let Some(_) = self.consume_without_eols(&PROVIDES) {
            Some(Token::ProvidesKeyword)
        } else if let Some(_) = self.consume_without_eols(&REQUIRES) {
            Some(Token::RequiresKeyword)
        } else if let Some(_) = self.consume_without_eols(&TO) {
            Some(Token::ToKeyword)
        } else if let Some(_) = self.consume_without_eols(&TRANSITIVE) {
            Some(Token::TransitiveKeyword)
        } else if let Some(_) = self.consume_without_eols(&USES) {
            Some(Token::UsesKeyword)
        } else if let Some(_) = self.consume_without_eols(&WITH) {
            Some(Token::WithKeyword)
        } else if let Some(_) = self.consume_without_eols(&LEFT_PAREN) {
            Some(Token::LeftParenSeparator)
        } else if let Some(_) = self.consume_without_eols(&RIGHT_PAREN) {
            Some(Token::RightParenSeparator)
        } else if let Some(_) = self.consume_without_eols(&LEFT_BRACE) {
            Some(Token::LeftBraceSeparator)
        } else if let Some(_) = self.consume_without_eols(&RIGHT_BRACE) {
            Some(Token::RightBraceSeparator)
        } else if let Some(_) = self.consume_without_eols(&LEFT_BRACKET) {
            Some(Token::LeftBracketSeparator)
        } else if let Some(_) = self.consume_without_eols(&RIGHT_BRACKET) {
            Some(Token::RightBracketSeparator)
        } else if let Some(_) = self.consume_without_eols(&SEMICOLON) {
            Some(Token::SemicolonSeparator)
        } else if let Some(_) = self.consume_without_eols(&COMMA) {
            Some(Token::CommaSeparator)
        } else if let Some(_) = self.consume_without_eols(&ELLIPSIS) {
            Some(Token::EllipsisSeparator)
        } else if let Some(_) = self.consume_without_eols(&DOT) {
            Some(Token::DotSeparator)
        } else if let Some(_) = self.consume_without_eols(&AT) {
            Some(Token::AtSeparator)
        } else if let Some(_) = self.consume_without_eols(&DOUBLE_COLON) {
            Some(Token::DoubleColonSeparator)
        } else if let Some(_) = self.consume_without_eols(&EQUAL) {
            Some(Token::EqualOperator)
        } else if let Some(_) = self.consume_without_eols(&ASSIGN) {
            Some(Token::AssignOperator)
        } else if let Some(_) = self.consume_without_eols(&NOT_EQUAL) {
            Some(Token::NotEqualOperator)
        } else if let Some(_) = self.consume_without_eols(&NOT) {
            Some(Token::NotOperator)
        } else if let Some(_) = self.consume_without_eols(&URIGHT_SHIFT_ASSIGN) {
            Some(Token::UnsignedRightShiftAssignOperator)
        } else if let Some(_) = self.consume_without_eols(&URIGHT_SHIFT) {
            Some(Token::UnsignedRightShiftOperator)
        } else if let Some(_) = self.consume_without_eols(&RIGHT_SHIFT_ASSIGN) {
            Some(Token::RightShiftAssignOperator)
        } else if let Some(_) = self.consume_without_eols(&RIGHT_SHIFT) {
            Some(Token::RightShiftOperator)
        } else if let Some(_) = self.consume_without_eols(&GREATER_EQUAL) {
            Some(Token::GreaterEqualOperator)
        } else if let Some(_) = self.consume_without_eols(&GREATER) {
            Some(Token::GreaterOperator)
        } else if let Some(_) = self.consume_without_eols(&LEFT_SHIFT_ASSIGN) {
            Some(Token::LeftShiftAssignOperator)
        } else if let Some(_) = self.consume_without_eols(&LEFT_SHIFT) {
            Some(Token::LeftShiftOperator)
        } else if let Some(_) = self.consume_without_eols(&LESS_EQUAL) {
            Some(Token::LessEqualOperator)
        } else if let Some(_) = self.consume_without_eols(&LESS) {
            Some(Token::LessOperator)
        } else if let Some(_) = self.consume_without_eols(&INCREMENT) {
            Some(Token::IncrementOperator)
        } else if let Some(_) = self.consume_without_eols(&PLUS_ASSIGN) {
            Some(Token::PlusAssignOperator)
        } else if let Some(_) = self.consume_without_eols(&PLUS) {
            Some(Token::PlusOperator)
        } else if let Some(_) = self.consume_without_eols(&ARROW) {
            Some(Token::ArrowOperator)
        } else if let Some(_) = self.consume_without_eols(&DECREMENT) {
            Some(Token::DecrementOperator)
        } else if let Some(_) = self.consume_without_eols(&MINUS_ASSIGN) {
            Some(Token::MinusAssignOperator)
        } else if let Some(_) = self.consume_without_eols(&MINUS) {
            Some(Token::MinusOperator)
        } else if let Some(_) = self.consume_without_eols(&TIMES_ASSIGN) {
            Some(Token::TimesAssignOperator)
        } else if let Some(_) = self.consume_without_eols(&TIMES) {
            Some(Token::TimesOperator)
        } else if let Some(_) = self.consume_without_eols(&DIVIDE_ASSIGN) {
            Some(Token::DivideAssignOperator)
        } else if let Some(_) = self.consume_without_eols(&DIVIDE) {
            Some(Token::DivideOperator)
        } else if let Some(_) = self.consume_without_eols(&MOD_ASSIGN) {
            Some(Token::ModAssignOperator)
        } else if let Some(_) = self.consume_without_eols(&MOD) {
            Some(Token::ModOperator)
        } else if let Some(_) = self.consume_without_eols(&COMPLEMENT) {
            Some(Token::ComplementOperator)
        } else if let Some(_) = self.consume_without_eols(&CONDITIONAL_AND) {
            Some(Token::ConditionalAndOperator)
        } else if let Some(_) = self.consume_without_eols(&BIT_AND_ASSIGN) {
            Some(Token::BitAndAssignOperator)
        } else if let Some(_) = self.consume_without_eols(&BIT_AND) {
            Some(Token::BitAndOperator)
        } else if let Some(_) = self.consume_without_eols(&CONDITIONAL_OR) {
            Some(Token::ConditionalOrOperator)
        } else if let Some(_) = self.consume_without_eols(&BIT_OR_ASSIGN) {
            Some(Token::BitOrAssignOperator)
        } else if let Some(_) = self.consume_without_eols(&BIT_OR) {
            Some(Token::BitOrOperator)
        } else if let Some(_) = self.consume_without_eols(&BIT_XOR_ASSIGN) {
            Some(Token::BitXorAssignOperator)
        } else if let Some(_) = self.consume_without_eols(&BIT_XOR) {
            Some(Token::BitXorOperator)
        } else if let Some(_) = self.consume_without_eols(&TERNARY_IF) {
            Some(Token::TernaryIfOperator)
        } else if let Some(_) = self.consume_without_eols(&TERNARY_ELSE) {
            Some(Token::TernaryElseOperator)
        } else if let Some(mat) = self.consume_without_eols(&STRING_LITERAL) {
            Some(Token::StringLiteral(mat.as_str()))
        } else if let Some(mat) = self.consume_without_eols(&CHARACTER_LITERAL) {
            Some(Token::CharacterLiteral(mat.as_str()))
        } else if let Some(mat) = self.consume_without_eols(&BOOLEAN_LITERAL) {
            Some(Token::BooleanLiteral(mat.as_str()))
        } else if let Some(_) = self.consume_without_eols(&NULL_LITERAL) {
            Some(Token::NullLiteral)
        } else if let Some(mat) = self.consume_without_eols(&DECIMAL_INTEGER_LITERAL) {
            Some(Token::DecimalIntegerLiteral(mat.as_str()))
        } else if let Some(mat) = self.consume_without_eols(&DECIMAL_FLOATING_POINT_LITERAL) {
            Some(Token::DecimalFloatingPointLiteral(mat.as_str()))
        } else if let Some(mat) = self.consume_without_eols(&HEX_INTEGER_LITERAL) {
            Some(Token::HexIntegerLiteral(mat.as_str()))
        } else if let Some(mat) = self.consume_without_eols(&HEX_FLOATING_POINT_LITERAL) {
            Some(Token::HexFloatingPointLiteral(mat.as_str()))
        } else if let Some(mat) = self.consume_without_eols(&OCTAL_INTEGER_LITERAL) {
            Some(Token::OctalIntegerLiteral(mat.as_str()))
        } else if let Some(mat) = self.consume_without_eols(&BINARY_INTEGER_LITERAL) {
            Some(Token::BinaryIntegerLiteral(mat.as_str()))
        } else if let Some(mat) = self.consume_without_eols(&IDENTIFIER) {
            Some(Token::Identifier(mat.as_str()))
        } else {
            None
        }
    }

    fn handle_end_of_line(&mut self, mat: &Match<'input>) -> Token<'input> {
        self.offset += mat.end();
        self.line   += 1;
        self.column  = 0;
        Token::EndOfLine(mat.as_str())
    }

    fn handle_block_comment(&mut self, mat: &Match<'input>) -> Token<'input> {
        self.offset += mat.end();

        let comment = mat.as_str();

        let eol_mats = END_OF_LINE_CHARS.find_iter(comment);
        if let Some((mat_index, eol_mat)) = eol_mats.enumerate().last() {
            let num_eols = mat_index + 1;
            self.line   += num_eols;
            self.column  = mat.end() - eol_mat.end();
        } else {
            self.column += mat.end();
        }

        Token::BlockComment(comment)
    }

    fn consume(&mut self, regex: &Regex) -> Option<Match<'input>> {
        regex.find(self.input).map(|mat| {
            self.input = &self.input[mat.end()..];
            mat
        })
    }

    fn consume_without_eols(&mut self, regex: &Regex) -> Option<Match<'input>> {
        self.consume(regex).map(|mat| {
            self.offset += mat.end();
            self.column += mat.end();
            mat
        })
    }
}

impl<'input> Iterator for JavaLexer<'input> {
    type Item = Result<(usize, LocatableToken<'input>, usize), LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {

        self.consume_without_eols(&SPACES);

        if self.input.is_empty() {
            return None
        }

        let start_offset = self.offset;

        let start_position = Position {
            line   : self.line,
            column : self.column,
        };

        if let Some(token) = self.next_token() {
            let end_offset = self.offset;
            let locatable_token = LocatableToken(start_position, token);
            Some(Ok((start_offset, locatable_token, end_offset)))
        } else {
            Some(Err(LexicalError { position: start_position }))
        }
    }
}
