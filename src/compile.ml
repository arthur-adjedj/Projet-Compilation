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
    [make (TEconstant (Cstring (x ^" :"))) Tstring;
    make (TEdot(e,y)) y.f_typ;make (TEconstant (Cstring ("\n"))) Tstring]@z) h []


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
          |_ -> error dummy_loc "tptc comp"
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
        |_ -> error dummy_loc "tptc op")

  | TEbinop (Beq | Bne as op, e1, e2) ->
      expr env e1 ++
      movq !%rdi !%rax ++
      expr env e2 ++
      cmpq !%rdi !%rax ++
      compile_bool (match op with |Beq -> je | Bne -> jne |_ -> error dummy_loc "tptc eq")

  | TEunop (Uneg, e1) ->
      expr env e1 ++
      negq !%rdi

  | TEunop (Unot, e1) ->
      expr env e1 ++
      cmpq (imm64 0L) !%rdi ++
      compile_bool jz

  | TEunop (Uamp, {expr_desc = TEident x}) ->
      comment ("Uamp of "^x.v_name) ++
      let ofse = x.v_id + env.ofs_this+1  in 
      movq !%rbp !%rdi ++
      addq (imm (-8*ofse)) !%rdi

  | TEunop (Uamp, {expr_desc = TEdot (e1, {f_name;f_ofs})}) ->
      expr env (make (TEunop (Uamp,e1)) (Tptr e1.expr_typ)) ++
      movq (ind rdi) !%rdi ++
      addq (imm f_ofs) !%rdi
  | TEunop (Uamp, {expr_desc = TEunop(Ustar,e1)}) ->
      comment "amp of *" ++
      expr env e1 
    
  | TEunop (Uamp,e) -> Pretty.expr std_formatter e; assert false

  | TEunop (Ustar, e1) ->
      expr env e1 ++
      movq (ind rdi) !%rdi

  | TEprint el ->
      let rec aux l = match l with 
        |[] -> nop
        |e::t ->
          expr env e ++ (
          match e.expr_typ with
            | Tint | Tptr _ -> call "print_int"
            | Tbool ->  call "print_bool"
            | Tstring -> call "print_string"
            | Tstruct s -> aux (htbl_to_list e s.s_fields)
            |_ -> error dummy_loc "tptc print"
        ) ++ (if t <> [] then (++) (call "print_space") else id)  (aux t)
      in comment "begin print" ++ aux el ++ comment "end print"

  | TEident x ->
      let a = (ind ~ofs:(-8*( x.v_id + env.ofs_this - env.next_local + 1))  rbp) in
      comment ("ident of "^x.v_name) ++
      movq a !%rdi    

  | TEassign ([{expr_desc = TEident x}], [e1]) ->
    comment ("start assign of ident : "^x.v_name) ++
      let ofse = x.v_id + env.ofs_this +1  in 
      let a = ind ~ofs:(-8*ofse) rbp in
      expr env e1 ++
      (if x.v_name = "_" then nop else movq !%rdi a)
      ++ comment ("end assign of ident : "^x.v_name)


  | TEassign ([lv], [e1]) ->
    comment "start assign" ++
      expr env e1 ++
      movq !%rdi !%rax ++
      expr env (make (TEunop(Uamp,lv)) (Tptr lv.expr_typ)) ++
      movq (ind rdi) !%rax ++
    comment "end assign"

  | TEassign (_, _) ->
      assert false

  | TEblock el ->
    comment "start block" ++

      let rec parcours el = (match el with
        |[] -> nop
        |{expr_desc = TEvars(vrs)}::t -> 
          List.fold_left (fun res v ->(
            if v.v_name = "_" then nop else
            (incr env.nb_locals;(pushq (imm64 0L))) ) ++
            res
            ) nop vrs ++ parcours t
        |h::t -> expr env h ++ (parcours t )) in
      parcours el ++

      comment "end block"

  | TEif (e1, e2, e3) ->
      let thena = new_label () in
      let elsea = new_label () in
      comment "start if" ++
      expr env e1 ++
      cmpq (imm64 0L) !%rdi  ++
      je elsea ++
      expr env e2 ++
      jmp thena ++
      label elsea ++
      expr env e3 ++
      label thena ++
      comment "end if"


  | TEfor (e1, e2) ->
      let l1 = new_label () in
      let l2 = new_label () in 
      comment "start for" ++
      label l1 ++
      expr env e1 ++
      testq !%rdi !%rdi++
      jz l2 ++
      expr env e2 ++
      jmp l1 ++
      label l2 ++
      comment "end for"


  | TEnew ty ->
      let s = sizeof ty in
      movq (imm s) (reg rdi) ++ 
      call "allocz" ++ 
      movq !%rax !%rdi

  | TEcall (f, el) ->

    (*when f called with args el, arguments need to be pushed on the stack, and rbp need to be moved at the right place.
    after execution of the call, rbp/rsp need to be reset to their place before all happened*)
      let n = List.length el in
      print_int n;
      print_newline ();
      List.fold_left (fun x y -> (expr env y) ++ x) nop el ++
      call ("F_"^f.fn_name)
      (* TODO code pour appel fonction *) 

  | TEdot ({expr_desc = TEident x} as e1, {f_name;f_ofs}) ->
      expr env e1 ++
      movq (ind ~ofs:f_ofs rdi) !%rdi

  | TEdot ({expr_desc = TEunop(Ustar,e1) },{f_name;f_ofs}) -> 
      expr env e1 ++  
      movq (ind ~ofs:f_ofs rdi) !%rdi

  |TEdot _ as e -> Pretty.expr std_formatter (make e tvoid)  ; error dummy_loc "tptc dot"
  | TEvars _ ->
      nop
  | TEreturn [] ->
    (* TODO code pour return e *) assert false
  | TEreturn [e1] ->
    (* TODO code pour return e1,... *) assert false
  | TEreturn _ ->
      assert false
  | TEincdec (e1, Inc) ->
      expr env (make (TEunop(Uamp,e1)) (Tptr e1.expr_typ)) ++
      addq (imm 1) (ind rdi)
  | TEincdec (e1, Dec) ->
      expr env (make (TEunop(Uamp,e1)) (Tptr e1.expr_typ)) ++
      subq (imm 1) (ind rdi)




let maxalloc f e =
  let rec nb_vars = function
    |[] -> 0
    |TEvars(vs) :: t -> List.length vs + (nb_vars t)
    |TEblock(l)::t2 -> List.fold_left (fun a x -> a + nb_vars [x.expr_desc] ) 0 l + nb_vars t2
    |_::t -> nb_vars t
  in List.length f.fn_params + nb_vars e
  

let rec n_vars {expr_desc} = match expr_desc with
  |TEvars vrs -> List.length (List.filter (fun x -> x.v_name <> "_") vrs)
  |TEblock b -> List.fold_left (fun x y -> x + (n_vars y)) 0 b
  |_ -> 0

let function_ f e envg =
  if !debug then eprintf "function %s:@." f.fn_name;
  let n =  n_vars e in
  (* TODO code pour fonction *) 
  let s = f.fn_name in 
  let n_params = List.length f.fn_params in
  let env = { exit_label = ""; ofs_this = n_params - 1; nb_locals = ref 0; next_local = !envg.next_local } in
  envg := {!envg with next_local = env.next_local + n};
  label ("F_" ^ s) ++ 
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  expr env e ++
  addq (imm64  (Int64.of_int (8*n))) !%rsp ++
  label ("E_" ^ s) ++
  movq !%rbp !%rsp ++
  popq rbp ++
  ret

let decl env code = 
  function
  | TDfunction (f, e) -> code ++ function_ f e env
  | TDstruct _ -> code


let file ?debug:(b=false) dl =
  debug := b;
  let env = ref empty_env in

  (* TODO calcul offset champs *)
  (* TODO code fonctions *) let funs = List.fold_left (decl env) nop dl in
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
      mov     $S_false, %rdi
      call    printf
      ret
allocz:
      movq    %rdi, %rbx     # callee-save
        call    malloc
        testq   %rbx, %rbx
        jnz     1f
        ret
1:      movb    $0, (%rax, %rbx)
        decq    %rbx
        jnz     1b
        ret
";
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
