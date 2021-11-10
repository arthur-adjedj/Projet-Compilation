open Ast
open Lib
open Format
open Tast

let binop = function
  | Ast.Badd -> " + "
  | Bsub -> " - "
  | Bmul -> " * "
  | Bdiv -> " / "
  | Bmod -> " % "
  | Beq -> " == "
  | Bne -> " != "
  | Blt -> " < "
  | Ble -> " <= "
  | Bgt -> " > "
  | Bge -> " >= "
  | Band -> " && "
  | Bor -> " || "
let unop = function
  | Ast.Uneg -> "-"
  | Unot -> "!"
  | Uamp -> "&"
  | Ustar -> "*"

let print_constant fmt = function
   |Cbool(x) -> pp_print_string fmt (string_of_bool x)
   |Cint(x) -> pp_print_string fmt  (Int64.to_string x)
   |Cstring(x) -> pp_print_string fmt ("\""^x^"\"")

let rec print_ptyp fmt = function
   |PTident(ident) -> 
      if ident.id <> "bool" && ident.id <> "int" then
      pp_print_string fmt ident.id
   |PTptr(ptyp) ->
      pp_print_char fmt '*';
      print_ptyp fmt ptyp

      
let rec print_pfields fmt : pfield list -> unit= function
   |[] -> ()
   |(ident,ptyp)::r ->       
      pp_print_string fmt ident.id;
      pp_print_char fmt ' ';
      print_ptyp fmt ptyp;
      pp_print_cut fmt ();
      print_pfields fmt r


let rec print_pparam fmt :pparam list -> unit = function
   |[] -> ()
   |[(ident,ptyp)] -> 
      pp_print_string fmt ident.id;
      pp_print_char fmt ' ';
      print_ptyp fmt ptyp
   |(ident,ptyp)::r ->       
      pp_print_string fmt ident.id;
      pp_print_char fmt ' ';
      print_ptyp fmt ptyp;
      pp_print_char fmt ',';
      print_pparam fmt r      

let rec print_pexpr (fmt : Format.formatter) e = 
   match e.pexpr_desc with
   | PEskip -> ()
   | PEconstant(x) -> print_constant fmt x
   | PEbinop(op,e1,e2) -> 
      print_pexpr fmt e1;
      pp_print_string fmt (binop op);
      print_pexpr fmt e2
   | PEunop(op,e1) -> 
      pp_print_string fmt (unop op);
      print_pexpr fmt e1
   | PEnil -> pp_print_string fmt "NIL"
   | PEcall(ident,pexprs) -> 
      pp_print_string fmt ident.id;
      pp_print_char fmt '(';
      print_pexpr_list fmt "," pexprs  ;
      pp_print_char fmt ')'

   | PEident(ident) -> pp_print_string fmt ident.id 
   | PEdot(e1,ident) -> 
      print_pexpr fmt e1;
      pp_print_string fmt ("."^ident.id);
   | PEassign(pexprs1,pexprs2) ->
      print_pexpr_list fmt "," pexprs1;
      pp_print_string fmt " = ";
      print_pexpr_list fmt "," pexprs2
   | PEvars(idents,typeo,pexprs) -> 
      pp_print_string fmt "var ";
      print_pexpr_list fmt "," (List.map (fun x -> { pexpr_desc = PEident(x);pexpr_loc = x.loc}) idents);
      if pexprs <> [] then (
         pp_print_string fmt " = ";
         print_pexpr_list fmt " , " pexprs 
      )
   | PEif(cond,yes,no) -> 
      pp_print_string fmt "if ("; 
      print_pexpr fmt cond;
      pp_print_string fmt ")";
      print_pexpr fmt yes;
      pp_print_string fmt "else";
      print_pexpr fmt no     
   | PEreturn(pexprs) -> 
      print_pexpr_list fmt "," pexprs
   | PEblock(pexprs) ->
      pp_print_string fmt "{";
      pp_print_cut fmt ();
      pp_open_vbox fmt 2;
      pp_print_string fmt "  ";
      print_pexpr_list ~newline:true fmt "" pexprs;
      pp_close_box fmt ();
      pp_print_cut fmt ();
      pp_print_string fmt "}";
   | PEfor(loop,e1) -> 
      pp_print_string fmt "for (";
      print_pexpr fmt loop;
      pp_print_string fmt ") ";
      print_pexpr fmt e1
   
   | PEincdec(e1,incdec) -> 
      print_pexpr fmt e1;
      pp_print_string fmt
         (if incdec = Inc then "++" else "--")

and print_pexpr_list ?newline:(newline = false) fmt c =
      function
      |[] -> ()
      |[x] -> print_pexpr fmt x
      |h::l -> 
         print_pexpr fmt h;
         pp_print_string fmt c;
         if newline then pp_print_cut fmt ();
         print_pexpr_list ~newline:newline fmt c l


let print_pfunc (fmt : Format.formatter) (f : Ast.pfunc)  = 
   pp_print_string fmt "func ";
   pp_print_string fmt f.pf_name.id;
   pp_print_char fmt '(';
   print_pparam fmt f.pf_params;
   pp_print_string fmt ") ";
   print_pexpr fmt f.pf_body

let print_pstruct fmt e = 
   pp_print_string fmt ("struct "^e.ps_name.id^" { ");
   pp_print_cut fmt ();
   pp_open_vbox fmt 2;
   pp_print_string fmt "  ";
   print_pfields fmt e.ps_fields;
   pp_close_box fmt ();
   pp_print_cut fmt ();
   pp_print_string fmt "}"

let rec ast_file (fmt : Format.formatter) (dl: Ast.pdecl list) = 
   pp_open_vbox fmt 0;
   match dl with
      |[] -> ()
      |PDfunction(f)::t -> 
         print_pfunc fmt f;
         pp_print_cut fmt ();
         pp_print_cut fmt ();
         pp_close_box fmt ();
         ast_file fmt t
      |PDstruct(s)::t -> 
         print_pstruct fmt s;
         pp_print_cut fmt ();
         pp_print_cut fmt ();
         pp_close_box fmt ();
         ast_file fmt t

let rec typ fmt = function
  | Tint -> fprintf fmt "int"
  | Tbool -> fprintf fmt "bool"
  | Tstring -> fprintf fmt "string"
  | Tstruct s -> fprintf fmt "%s" s.s_name
  | Tptr ty -> fprintf fmt "*%a" typ ty
  |_ -> ()


(* TODO autres types utilises par l'analyse semantique *)

let rec expr fmt e = match e.expr_desc with
  | TEskip -> ()
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

