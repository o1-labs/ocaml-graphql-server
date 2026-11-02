{
open Lexing
open Parser

exception Error of string

let code_point h = int_of_string ("0x" ^ h)

let add_code_point buf n = Buffer.add_utf_8_uchar buf (Uchar.of_int n)
}

let ignored = [' ' '\t' ',']+
let comment = '#' [^ '\n' '\r']*
let newline = '\r' | '\n' | "\r\n"

let digit = ['0'-'9']
let int = '-'? '0' | '-'? ['1'-'9'] digit*
let fract_part = '.' digit+
let exp_part = ['e' 'E'] ['+' '-']? digit+
let float = int fract_part | int exp_part | int fract_part exp_part

let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let high_surrogate = ['d' 'D'] ['8' '9' 'a' 'b' 'A' 'B'] hex hex
let low_surrogate = ['d' 'D'] ['c'-'f' 'C'-'F'] hex hex

let name = ['_' 'A'-'Z' 'a'-'z'] ['_' '0'-'9' 'A'-'Z' 'a'-'z']*

rule token = parse
  | ignored { token lexbuf }
  | comment { token lexbuf }
  | newline { new_line lexbuf; token lexbuf }

  | int   { INT (int_of_string (lexeme lexbuf)) }
  | float { FLOAT (float_of_string (lexeme lexbuf)) }
  | '"'   { read_string (Buffer.create 17) lexbuf }

  | "false"        { BOOL false }
  | "fragment"     { FRAGMENT }
  | "mutation"     { MUTATION }
  | "null"         { NULL }
  | "on"           { ON }
  | "query"        { QUERY }
  | "subscription" { SUBSCRIPTION }
  | "true"         { BOOL true }
  | name           { NAME (lexeme lexbuf) }

  | '!'   { BANG }
  | '$'   { DOLLAR }
  | '('   { LPAREN }
  | ')'   { RPAREN }
  | "..." { ELLIPSIS }
  | ':'   { COLON }
  | '='   { EQUAL }
  | '@'   { AT }
  | '['   { LBRACK }
  | ']'   { RBRACK }
  | '{'   { LBRACE }
  | '}'   { RBRACE }
  | _ { raise (Error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof   { EOF }

and read_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' '"'  { Buffer.add_char buf    '"'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf   '\\'; read_string buf lexbuf }
  | '\\' '/'  { Buffer.add_char buf    '/'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf   '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf   '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf   '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf   '\t'; read_string buf lexbuf }
  | "\\u" (high_surrogate as hi) "\\u" (low_surrogate as lo)
    {
      add_code_point buf
        (0x10000 + ((code_point hi - 0xD800) lsl 10) + (code_point lo - 0xDC00));
      read_string buf lexbuf
    }
  | "\\u" (high_surrogate | low_surrogate)
    { raise (Error ("Unpaired surrogate: " ^ lexeme lexbuf)) }
  | "\\u" (hex hex hex hex as h)
    { add_code_point buf (code_point h); read_string buf lexbuf }
  | '\\' _? { raise (Error ("Invalid escape: " ^ lexeme lexbuf)) }
  | ['\n' '\r'] | eof { raise (Error "Unterminated string") }
  | [^ '"' '\\' '\n' '\r']+
    {
      Buffer.add_string buf (lexeme lexbuf);
      read_string buf lexbuf
    }
