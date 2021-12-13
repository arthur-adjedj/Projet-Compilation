(* étiquettes
     F_function      entrée fonction
     E_function      sortie fonction
     L_xxx           sauts
     S_xxx           chaîne

   expression calculée avec la pile si besoin, résultat final dans %rdi

   fonction : arguments sur la pile, résultat dans %rax ou sur la pile

            res k
            ...
            res 1
            arg n
            ...
            arg 1
            adr. retour
   rbp ---> ancien rbp
            ...
            var locales
            ...
            calculs
   rsp ---> ...

*)

open Format
open Ast
open Tast
open X86_64
open Typing

let id x = x 
let debug = ref false

let strings = Hashtbl.create 32
let alloc_string =
  let r = ref 0 in
  fun s ->
    incr r;
    let l = "S_" ^ string_of_int !r in
    Hashtbl.add strings l s;
    l

let malloc n = movq (imm n) (reg rdi) ++ call "malloc"
let allocz n = movq (imm n) (reg rdi) ++ call "allocz"

let sizeof = Typing.sizeof

let new_label =
  let r = ref 0 in fun () -> incr r; "L_" ^ string_of_int !r

type env = {
  exit_label: string;
  ofs_this: int;
  nb_locals: int ref; (* maximum *)
  next_local: int; (* 0, 1, ... *)
}




let empty_env =
  { exit_label = ""; ofs_this = -1; nb_locals = ref 0; next_local = 0 }

let mk_bool d = { expr_desc = d; expr_typ = Tbool }

(* f reçoit le label correspondant à ``renvoyer vrai'' *)
let compile_bool f =
  let l_true = new_label () and l_end = new_label () in
  f l_true ++
  movq (imm 0) (reg rdi) ++ 
  jmp l_end ++
  label l_true ++
  movq (imm 1) (reg rdi) ++
  label l_end
 
let fields h = Hashtbl.fold (fun _ x y -> x::y) h []

let htbl_to_list e h = (*très fier de cette horreur*)
  Hashtbl.fold (fun x y z-> 
    [make (TEconstant (Cstring (x ^" : ")))Tstring;
    make (TEdot(e,y)) y.f_typ]@z) h []


let adress f var = 
  -8*(List.length f.fn_params + 2 + var.v_id)

let rec expr (env: env) e = match e.expr_desc with
  | TEskip ->
      nop
  | TEconstant (Cbool true) ->
      movq (imm 1) !%rdi
  | TEconstant (Cbool false) ->
      movq (imm 0) !%rdi
  | TEconstant (Cint x) ->
      movq (imm64 x) !%rdi
  | TEnil ->
      xorq (reg rdi) !%rdi
  | TEconstant (Cstring s) ->
      movq (ilab (alloc_string s)) !%rdi
  | TEbinop (Band, e1, e2) ->
      expr env 
        (mk_bool
          (TEif(
            mk_bool (TEunop(Unot,e1)),
            mk_bool (TEconstant (Cbool false)) ,
            e2)))

  | TEbinop (Bor, e1, e2) ->
    expr env 
    (mk_bool 
      (TEif(
        e1,
        mk_bool (TEconstant (Cbool true)),        
        e2
        )))

  | TEbinop (Blt | Ble | Bgt | Bge as op, e1, e2) -> 
    expr env e1 ++
    movq !%rdi !%rax ++
    expr env e2 ++
    cmpq !%rdi !%rax ++
    compile_bool (
      match op with
        |Blt -> jl
        |Ble -> jle
        |Bgt -> jg
        |Bge -> jge
        |_ -> failwith "tptc"
    )

  | TEbinop (Badd | Bsub | Bmul | Bdiv | Bmod as op, e1, e2) ->
      expr env e1 ++
      movq !%rdi !%rax ++
      expr env e2 ++
      (match op with
        | Badd -> addq !%rax !%rdi
        | Bsub -> subq !%rax !%rdi
        | Bmul -> imulq !%rax !%rdi
        | Bdiv -> movq (imm64 0L) !%rdx ++
                  idivq !%rdi  ++
                  movq !%rax !%rdi
        | Bmod -> movq (imm64 0L) !%rdx ++
                  idivq !%rdi  ++
                  movq !%rdx !%rdi
        |_ -> failwith "tptc")

  | TEbinop (Beq | Bne as op, e1, e2) ->
      expr env e1 ++
      movq !%rdi !%rax ++
      expr env e2 ++
      cmpq !%rdi !%rax ++
      compile_bool (match op with |Beq -> je | Bne -> jne |_ -> failwith "tptc")

  | TEunop (Uneg, e1) ->
      expr env e1 ++
      negq !%rdi

  | TEunop (Unot, e1) ->
      expr env e1 ++
      cmpq (imm64 0L) !%rdi ++
      compile_bool jz

  | TEunop (Uamp, e1) ->
    (* TODO code pour & *) assert false 
  | TEunop (Ustar, e1) ->
    (* TODO code pour * *) assert false 
  | TEprint el ->
      let rec aux l = match l with 
        |[] -> nop
        |e::t ->
          expr env e ++ (
          match e.expr_typ with
            | Tint | Tptr _ -> call "print_int"
            | Tbool -> call "print_bool"
            | Tstring -> call "print_string"
            | Tstruct s -> aux (htbl_to_list e s.s_fields)
            |_ -> failwith "tptc"
        ) ++ (if t <> [] then (++) (call "print_space") else id)  (aux t)
      in aux el

  | TEident x ->
      let a = (ind ~ofs:(-8*( x.v_id + env.ofs_this +1 ))  rbp) in
      print_string ("trying to find "^x.v_name^" in address : ");
      (convert a) std_formatter ();
      print_newline ();
      movq a !%rdi
    (* TODO code pour x *) 
  | TEassign ([{expr_desc = TEident x}], [e1]) ->
      let ofse = x.v_id + env.ofs_this+1  in 
      let a = (ind ~ofs:(-8*ofse) rbp) in
      print_string ("id of "^x.v_name^" : ");
      print_int x.v_id;
      print_string "\noffset of env : ";
      print_int env.ofs_this;
      print_string ("\nplace of value "^x.v_name^" in mem: ");
      (X86_64.convert a) std_formatter ();
      print_newline ();
      expr env e1 ++
      if x.v_name = "_" then nop else movq !%rdi a

  | TEassign ([lv], [e1]) ->
    (* TODO code pour x1,... := e1,... *) assert false 
  | TEassign (_, _) ->
      assert false
  | TEblock el ->
      let rec parcours el = (match el with
        |[] -> nop
        |{expr_desc = TEvars(vrs)}::t -> 
          List.fold_left (fun res v ->(
            if v.v_name = "_" then nop else
            (pushq (imm64 0L)) ) ++
            res
            ) nop vrs ++ parcours t
        |h::t -> expr env h ++ (parcours t )) in
      parcours el
  | TEif (e1, e2, e3) ->
      let thena = new_label () in
      let elsea = new_label () in
      expr env e1 ++
      cmpq (imm64 0L) !%rdi  ++
      je elsea ++
      expr env e2 ++
      jmp thena ++
      label elsea ++
      expr env e3 ++
      label thena

  | TEfor (e1, e2) ->
     (* TODO code pour for *) assert false
  | TEnew ty ->
     (* TODO code pour new S *) assert false
  | TEcall (f, el) ->
     (* TODO code pour appel fonction *) assert false
  | TEdot (e1, {f_ofs=ofs}) ->
     (* TODO code pour e.f *) assert false
  | TEvars _ ->
      nop
  | TEreturn [] ->
    (* TODO code pour return e *) assert false
  | TEreturn [e1] ->
    (* TODO code pour return e1,... *) assert false
  | TEreturn _ ->
     assert false
  | TEincdec (e1, op) ->
    (* TODO code pour return e++, e-- *) assert false



let maxalloc f e =
  let rec nb_vars = function
    |[] -> 0
    |TEvars(vs) :: t -> List.length vs + (nb_vars t)
    |TEblock(l)::t2 -> List.fold_left (fun a x -> a + nb_vars [x.expr_desc] ) 0 l + nb_vars t2
    |_::t -> nb_vars t
  in List.length f.fn_params + nb_vars e
  
let function_ f e =
  if !debug then eprintf "function %s:@." f.fn_name;
  (* TODO code pour fonction *) 
  let s = f.fn_name in 
  label ("F_" ^ s) ++ 
  (*rajouter adresses arguments/retours*)
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  (expr empty_env e) ++
  label ("E_" ^ s) ++
  movq !%rbp !%rsp ++
  popq rbp ++
  ret

let decl code = function
  | TDfunction (f, e) -> code ++ function_ f e
  | TDstruct _ -> code


let file ?debug:(b=false) dl =
  debug := b;
  (* TODO calcul offset champs *)
  (* TODO code fonctions *) let funs = List.fold_left decl nop dl in
  { text =
      globl "main" ++ label "main" ++
      call "F_main" ++
      xorq !%rax !%rax ++
      ret ++
      funs ++
      inline "
print_int_or_nil:
      test    %rdi, %rdi
      jz      print_nil
      movq    (%rdi), %rdi
print_int:
      movq    %rdi, %rsi
      movq    $S_int, %rdi
      xorq    %rax, %rax
      call    printf
      ret
print_string:
      test    %rdi, %rdi
      jz      print_nil
      mov     %rdi, %rsi
      mov     $S_string, %rdi
      xorq    %rax, %rax
      call    printf
      ret
print_nil:
      mov     $S_nil, %rdi
      xorq    %rax, %rax
      call    printf
      ret
print_space:
      mov     $S_space, %rdi
      xorq    %rax, %rax
      call    printf
      ret
print_bool:
      xorq    %rax, %rax
      test    %rdi, %rdi
      jz      1f
      mov     $S_true, %rdi
      call    printf
      ret
1:    mov     $S_false, %rdi
      call    printf
      ret
";
   (* TODO appel malloc de stdlib *)
    data =
      label "S_int" ++ string "%ld" ++
      label "S_string" ++ string "%s" ++
      label "S_true" ++ string "true" ++
      label "S_false" ++ string "false" ++
      label "S_nil" ++ string "<nil>" ++
      label "S_space" ++ string " " ++
      label "S_empty" ++ string "" ++
      (Hashtbl.fold (fun l s d -> label l ++ string s ++ d) strings nop)
    ;
  }
