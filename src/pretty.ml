open Ast
open Lib
open Format
open Tast

let binop = function
  | Ast.Badd -> "+"
  | Bsub -> "-"
  | Bmul -> "+"
  | Bdiv -> "/"
  | Bmod -> "%"
  | Beq -> "=="
  | Bne -> "!="
  | Blt -> "<"
  | Ble -> "<="
  | Bgt -> ">"
  | Bge -> ">="
  | Band -> "&&"
  | Bor -> "||"
let unop = function
  | Ast.Uneg -> "-"
  | Unot -> "!"
  | Uamp -> "&"
  | Ustar -> "*"

(* TODO afficher ast non type *)
(* let ast_file fmt dl = ... *)

let draw_constant : constant -> string = function
   |Cbool(x) -> string_of_bool x
   |Cint(x) -> Int64.to_string x
   |Cstring(x) -> "\""^x^"\""


let draw_unop fmt  = function
   | Uneg -> pp_print_char fmt  '-'
   | Unot -> pp_print_char fmt  '!'
   | Uamp -> pp_print_char fmt  '&'
   | Ustar-> pp_print_char fmt  '*'

let draw_binop fmt = function
   | Badd -> pp_print_string fmt "+"
   | Bsub -> pp_print_string fmt "-"
   | Bmul -> pp_print_string fmt "*"
   | Bdiv -> pp_print_string fmt "/"
   | Bmod -> pp_print_string fmt "%"
   | Beq  -> pp_print_string fmt "="
   | Bne  -> pp_print_string fmt "!="
   | Blt  -> pp_print_string fmt "<" 
   | Ble  -> pp_print_string fmt "<="
   | Bgt  -> pp_print_string fmt ">"
   | Bge  -> pp_print_string fmt ">="
   | Band -> pp_print_string fmt "&&"
   | Bor  -> pp_print_string fmt "||"

let rec draw_pexpr (fmt : Format.formatter) e = 
   match e.pexpr_desc with
   | PEskip -> ()
   | PEconstant(x) -> pp_print_string fmt (draw_constant x)
   | PEbinop(op,e1,e2) -> 
      draw_pexpr fmt e1;
      draw_binop fmt op;
      draw_pexpr fmt e2
   | PEunop(op,e1) -> 
      draw_unop fmt op;
      draw_pexpr fmt e1
   | PEnil -> pp_print_string fmt "NIL"
(* | PEcall(_,pexprs) -> *) (*aucune idée de ce que c'est*)
   | PEident(_) -> () 
   | PEdot(e1,_) -> 
      pp_print_char fmt '.';
      draw_pexpr fmt e1;
      pp_print_char fmt ' '

   (*| PEassign of pexpr list * pexpr list
   | PEvars of ident list * ptyp option * pexpr list*)

   | PEif(cond,yes,no) -> 
      pp_print_string fmt "if ("; 
      draw_pexpr fmt cond;
      pp_print_string fmt ")";
      draw_pexpr fmt yes;
      pp_print_string fmt "else";
      draw_pexpr fmt no     

   | PEreturn(pexprs) -> 
      List.iter 
         (fun e1 -> 
            draw_pexpr fmt e1;
            pp_print_char fmt ';') pexprs
   | PEblock(pexprs) ->
      pp_open_box fmt 5;
      List.iter 
         (fun e1 ->
            draw_pexpr fmt e1)
         pexprs;
      pp_close_box fmt ()
   | PEfor(loop,e1) -> 
      pp_print_string fmt "for (";
      draw_pexpr fmt loop;
      pp_print_string fmt ") ";
      draw_pexpr fmt e1
   | PEincdec(e1,incdec) -> 
      draw_pexpr fmt e1;
      pp_print_string fmt
         (if incdec = Inc then "++" else "--")
   |_ -> ()

let draw_pfunc (fmt : Format.formatter) (f : Ast.pfunc)  = 
   pp_print_string fmt f.pf_name.id;
   pp_print_char fmt '(';

   pp_print_char fmt ')'



   

(*let rec ast_file (fmt : Format.formatter) (dl: Ast.pdecl list) = 
   match dl with
      |[] -> ()
      |PDfunction(f)::t -> pp_open_box fmt 5;
                           draw_pfunc fmt f;
                           pp_close_box fmt ();
                           pp_print_char fmt ';';
                           ast_file fmt t
      |PDstruct(s)::t -> ()

*)

let rec typ fmt = function
  | Tint -> fprintf fmt "int"
  | Tbool -> fprintf fmt "bool"
  | Tstring -> fprintf fmt "string"
  | Tstruct s -> fprintf fmt "%s" s.s_name
  | Tptr ty -> fprintf fmt "*%a" typ ty


  (* TODO autres types utilises par l'analyse semantique *)

let rec expr fmt e = match e.expr_desc with
  | TEskip -> fprintf fmt ";"
  | TEnil -> fprintf fmt "ni"
  | TEconstant (Cint n) -> fprintf fmt "%Ld" n
  | TEconstant (Cbool b) -> fprintf fmt "%b" b
  | TEconstant (Cstring s) -> fprintf fmt "%S" s
  | TEbinop (op, e1, e2) ->
     fprintf fmt "@[(%a %s@ %a)@]" expr e1 (binop op) expr e2
  | TEunop (op, e1) ->
     fprintf fmt "@[(%s@ %a)@]" (unop op) expr e1
  | TEnew ty ->
     fprintf fmt "new(%a)" typ ty
  | TEcall (f, el) ->
     fprintf fmt "%s(%a)" f.fn_name list el
  | TEident v ->
     fprintf fmt "%s" v.v_name
  | TEdot (e1, f) ->
     fprintf fmt "%a.%s" expr e1 f.f_name
  | TEassign ([], _) | TEassign (_, []) ->
     assert false
  | TEassign ([lvl], [e]) ->
     fprintf fmt "%a = %a" expr lvl expr e
  | TEassign (lvl, el) ->
     fprintf fmt "%a = %a" list lvl list el
  | TEif (e1, e2, e3) ->
     fprintf fmt "if %a@ %a@ %a" expr e1 expr e2 expr e3
  | TEreturn el ->
     fprintf fmt "return %a" list el
  | TEblock bl ->
     block fmt bl
  | TEfor (e1, e2) ->
     fprintf fmt "for %a %a" expr e1 expr e2
  | TEprint el ->
     fprintf fmt "fmt.Print(%a)" list el
  | TEincdec (e1, op) ->
     fprintf fmt "%a%s" expr e1 (match op with Inc -> "++" | Dec -> "--")
  | TEvars vl ->
     fprintf fmt "var %a" (print_list comma var) vl

and var fmt v =
  fprintf fmt "%s" v.v_name

and block fmt bl =
  fprintf fmt "{@\n%a}" (print_list newline expr) bl

and list fmt el =
  print_list comma expr fmt el

let decl fmt = function
  | TDfunction (f, e) ->
     fprintf fmt "@[<hov 2>func %s(%a) %a@]@\n@\n"
       f.fn_name (print_list comma var) f.fn_params expr e
  | TDstruct s ->
     fprintf fmt "type %s struct { ... }@\n" s.s_name

let file fmt dl =
  fprintf fmt "---------@\n";
  List.iter (decl fmt) dl;
  fprintf fmt "---------@\n"

