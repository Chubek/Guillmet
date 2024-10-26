{
type token =
        | TOKEN_QueryName of string
        | TOKEN_ImperativeName of string
        | TOKEN_CarrierName of string
        | TOKEN_CasualName of string
        | TOKEN_Integer of string
        | TOKEN_HexInteger of string
        | TOKEN_BinInteger of string
        | TOKEN_OctInteger of string
        | TOKEN_Real of string
        | TOKEN_ExactReal of string
        | TOKEN_RoundedReal of string
        | TOKEN_Quasiquote
        | TOKEN_Quote
        | TOKEN_Expn
        | TOKEN_Splice

module Make (M : sig
        type 'a t
        val return : 'a -> 'a t
        val bind : 'a t -> ('a -> 'b t) -> 'b t
        val fail : string -> 'a t

        val on_refill : Lexing.lexbuf -> unit t
       end)
= struct

let refill_handler k lexbuf =
   M.bind (M.on_refill lexbuf) (fun () -> k lexbuf)


}

let upper = [ 'A' - 'Z' ]
let lower = [ 'a' - 'z' ]

let bdigit = '0' | '1'
let odigit = [ '0' - '7' ]
let ndigit = [ '1' - '9' ]
let zdigit = [ '0' - '9' ]
let xdigit = zdigit | [ 'a' - 'f' 'A' - 'F' ]

let letter = upper | lower

let idsym = ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '_' '~' '^']



let id = idhead idtail*

let intlit = sign? ndigit zdigit*
let binlit = '#' [ 'b' 'B' ] bdigit+
let octlit = '#' [ 'o' 'O' ] odigit+
let hexlit = '#' [ 'x' 'X' ] xdigit+

let realbase = ( "nan" | "inf" | zdigit+ )
let sign = [ '+' '-' ]
let decimallit = realbase ( '.' realbase )?
let fractlit = realbase '/' realbase
let scilit = realbase [ 'e' 'E' ] sign? intlit
let reallit = sign? ( decimallit | fractlit | scilit )

let roundlit = reallit ( 's' | 'f' | 'd' | 'l' | 'S' | 'F' | 'D' | 'L' ) '0'
let exactlit = "e#" reallit

let complexlit = decimallit sign decimallit [ 'a' - 'z' ]

let charlit = "\\#" [^ '\\' '#' ]+

let dqstrlit = '"' [^ '"' ]* '"'
let sqstrlit = '\'' [^ '\'' ]* '\''

let ldelim = [ '(' '[' '}' ]
let rdelim = [ ')' ']' '}' ]

let quote = '`' | '\''
let expand = '@'
let splice = ",@"

refill {refill_handler}

rule token = parse
| [ '\t' ' ' '\r' '\n' ] 
        { token lexbuf }
| id as name
        { M.return TOKEN_Name name }
| intlit as i 
        { M.return TOKEN_Integer ()  }
| reallit as r 
        {  }
| fractlit as f 
        {  }
| scilit as s 
        {  }
| binlit as b 
        {  }
| octlit as o 
        {  }
| hexlit as h 
        {  }
| charlit as c 
        {  }
| dqstrlit as s 
        {  }
| sqstrlit as s 
        {  }
| ldelim as ld 
        {  }
| rdelim as rd 
        {  }
| _ { }

{
end
}
