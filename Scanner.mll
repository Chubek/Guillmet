let upper = [ 'A' - 'Z' ]
let lower = [ 'a' - 'z' ]

let bdigit = '0' | '1'
let odigit = [ '0' - '7' ]
let ndigit = [ '1' - '9' ]
let zdigit = [ '0' - '9' ]
let xdigit = zdigit | [ 'a' - 'f' 'A' - 'F' ]

let letter = upper | lower

let idsym = ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '_' '~' '^']

let idhead = idsym | letter
let idtail = idhead | zdigit

let sign = [ '+' '-' ]

let name = idhead idtail*

let intlit = sign? ndigit zdigit*

let reallit = sign? zdigit* '.' zdigit+
let fractlit = sign? zdigit+ '/' zdigit+
let scilit = sign? zdigit+ [ 'e' 'E' ] sign? zdigit+

let binlit = '#' [ 'b' 'B' ] bdigit+
let octlit = '#' [ 'o' 'O' ] odigit+
let hexlit = '#' [ 'x' 'X' ] xdigit+

let charlit = "\\#" [^ '\\' '#' ]+

let dqstrlit = '"' [^ '"' ]* '"'
let sqstrlit = '\'' [^ '\'' ]* '\''

rule token = parse
| [ '\t' ' ' '\r' '\n' ] { token lexbuf }
| name as id { (`IDENT, id) }
| intlit as i { (`INTLIT, i) }
| reallit as r { (`REALLIT, r) }
| fractlit as f { (`FRACTLIT, f) }
| scilit as s { (`SCILIT, s) }
| binlit as b { (`BINLIT, b) }
| octlit as o { (`OCTLIT, o) }
| hexlit as h { (`HEXLIT, h) }
| charlit as c { (`CHARLIT, c) }
| dqstrlit as s { (`STRLIT, s) }
| sqstrlit as s { (`STRLIT, s) }
| '{' | '(' | '[' as ldelim { (`LEFTDELIM, ldelim) }
| '}' | ')' | ']' as rdelim { (`RIGHTDELIM, rdelim) }
| _ { }
