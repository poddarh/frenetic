(** Thin wrapper around Menhir-generated parser, providing a saner interface. *)

module MAKE(Lexer : module type of Frenetic_NetKAT_Lexer)
           (Parser : module type of Frenetic_NetKAT_Generated_Parser) = struct
  
  let pol_of_string ?pos (s : string) =
    Lexer.parse_string ?pos s Parser.pol_eof

  let pred_of_string ?pos (s : string) =
    Lexer.parse_string ?pos s Parser.pred_eof

  let pol_of_file (file : string) =
    Lexer.parse_file ~file Parser.pol_eof

  let pred_of_file (file : string) =
    Lexer.parse_file ~file Parser.pred_eof

end

include MAKE(Frenetic_NetKAT_Lexer)(Frenetic_NetKAT_Generated_Parser)

(** portless extensions *)
module Portless = MAKE(Frenetic_NetKAT_Lexer)(Frenetic_NetKAT_Portless_Generated_Parser)
