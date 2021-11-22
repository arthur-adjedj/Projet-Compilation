
open Format
open Lib
open Ast
open Tast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string

let error loc e = raise (Error (loc, e))


module Structs = struct

  module Struct = struct
    type t = pstruct
    let compare = compare
  end

  module M = Set.Make(Struct)
  type t = M.t
  let empty = M.empty
  let find = M.find
  let all_structs = ref empty
  let is_defed s = M.exists (fun x -> x.ps_name.id = s.ps_name.id) !all_structs
  let add s = 
    if is_defed s then 
      error s.ps_name.loc ("struct "^s.ps_name.id^"is defined twice")
  else
    all_structs := M.add s !all_structs
end

module Funcs = struct
  module Func = struct
    type t = pfunc
    let compare = compare
  end

  module M = Set.Make(Func)
  type t = M.t
  let empty = M.empty
  let find = M.find
  let all_funcs = ref empty
  let is_defed f = M.exists (fun x -> x.pf_name.id = f.pf_name.id) !all_funcs
  let add f = 
    if is_defed f then 
      error f.pf_name.loc ("function "^f.pf_name.id^"is defined twice")
  else
      all_funcs := M.add f !all_funcs
  let are_vars_unique f = 
    let table = Hashtbl.create 15 in
    let rec add_table = function
      |[] -> ()
      |h::t -> 
        if Hashtbl.mem table (fst h).id then
          error (fst h).loc ("the variable"^(fst h).id^"is already defined twice")
        else (
          Hashtbl.add table (fst h).id ();
          add_table t
        )
    in try add_table f.pf_params;true with _ -> false
end


let rec type_type = function
  | PTident { id = "int" } -> Tint
  | PTident { id = "bool" } -> Tbool
  | PTident { id = "string" } -> Tstring
  | PTptr ty -> Tptr (type_type ty)
  | PTident {id = s } -> error dummy_loc ("unknown struct ") (* TODO type structure *)

let rec eq_type ty1 ty2 = match ty1, ty2 with
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring -> true
  | Tstruct s1, Tstruct s2 -> s1 == s2
  | Tptr ty1, Tptr ty2 -> eq_type ty1 ty2
  | Tmany man1, Tmany man2 -> List.filter (fun x -> not (List.mem x man1 )) man2 = []
  | _ -> false
    (* TODO autres types *)

let fmt_used = ref false
let fmt_imported = ref false

let evar v = { expr_desc = TEident v; expr_typ = v.v_typ }

let new_var =
  let id = ref 0 in
  fun x loc ?(used=false) ty ->
    incr id;
    { v_name = x; v_id = !id; v_loc = loc; v_typ = ty; v_used = used; v_addr = false }

module Env = struct
  module M = Map.Make(String)
  type t = var M.t
  let empty = M.empty
  let find = M.find
  let add env v = M.add v.v_name v env

  let all_vars = ref []
  let check_unused () =
    let check v =
      if v.v_name <> "_" && not v.v_used then error v.v_loc "unused variable" in
    List.iter check !all_vars


  let var x loc ?used ty env =
    let v = new_var x loc ?used ty in
    all_vars := v :: !all_vars;
    add env v, v

  (* TODO type () et vecteur de types *)
end

let tvoid = Tmany []
let make d ty = { expr_desc = d; expr_typ = ty }
let stmt d = make d tvoid

let rec expr env e =
 let e, ty, rt = expr_desc env e.pexpr_loc e.pexpr_desc in
  { expr_desc = e; expr_typ = ty }, rt

and expr_desc env loc = function
  | PEskip ->
     TEskip, tvoid, false
  | PEconstant c ->
      TEconstant c, 
      (match c with
        |Cbool _ -> Tbool
        |Cint _ -> Tint
        |Cstring _ -> Tstring)
      , false
  | PEbinop (op, e1, e2) -> 
      let te1,te2 = (expr env e1),(expr env e2) in
      (match op with
        | Badd | Bsub | Bmul | Bdiv | Bmod -> 
          if (fst te1).expr_typ <> Tint then
            error e1.pexpr_loc "type int expected";
          if (fst te2).expr_typ <> Tint then
            error e2.pexpr_loc "type int expected";
          TEbinop(op,fst te1, fst te2), Tint,false
        | Beq | Bne | Blt | Ble | Bgt | Bge -> 
          if (fst te1).expr_typ <> Tint then
            error e1.pexpr_loc "type int expected";
          if (fst te2).expr_typ <> Tint then
            error e2.pexpr_loc "type int expected";
          TEbinop(op,fst te1, fst te2), Tbool,false
        | Band | Bor -> 
          if (fst te1).expr_typ <> Tbool then
            error e1.pexpr_loc "type bool expected";
          if (fst te2).expr_typ <> Tint then
            error e2.pexpr_loc "type bool expected";
          TEbinop(op,fst te1, fst te2), Tbool,false)
  | PEunop (Uamp, e1) ->
      let son = fst (expr env e1) in
      TEunop(Uamp, son),Tptr(son.expr_typ),false
  | PEunop (Uneg | Unot | Ustar as op, e1) -> 
      let son = fst (expr env e1) in
      (match son.expr_typ with
        |Tint -> if op <> Uneg then 
          error e1.pexpr_loc "this operator can't be applied to type int" 
        |Tbool -> if op <> Unot then 
          error e1.pexpr_loc "this operator can't be applied to type bool" 
        |Tptr(_) -> if op <> Ustar then 
          error e1.pexpr_loc "this operator can't be applied to pointer" 
        |_ -> error e1.pexpr_loc "can't apply unary operation to this expression!"
      );
      TEunop(op,son),son.expr_typ,false
  | PEcall ({id = "fmt.Print"}, el) ->
    (* TODO *) TEprint [], tvoid, false
  | PEcall ({id="new"}, [{pexpr_desc=PEident {id}}]) ->
     let ty = match id with
       | "int" -> Tint | "bool" -> Tbool | "string" -> Tstring
       | _ -> (* TODO *) error loc ("no such type " ^ id) in
     TEnew ty, Tptr ty, false
  | PEcall ({id="new"}, _) ->
     error loc "new expects a type"
  | PEcall (id, el) ->
     (* TODO *) assert false
  | PEfor (e, b) ->
     (* TODO *) assert false
  | PEif (e1, e2, e3) ->
     (* TODO *) assert false
  | PEnil ->
     (* TODO *) assert false
  | PEident {id=id} ->
     (* TODO *) (try let v = Env.find id env in TEident v, v.v_typ, false
      with Not_found -> error loc ("unbound variable " ^ id))
  | PEdot (e, id) ->
     (* TODO *) assert false
  | PEassign (lvl, el) ->
     (* TODO *) TEassign ([], []), tvoid, false 
  | PEreturn el ->
      let sons = List.map (fun x -> fst (expr env x)) el in
      TEreturn sons , Tmany(List.map (fun x -> x.expr_typ) sons), true
  | PEblock el ->
     (* TODO *) TEblock [], tvoid, false
  | PEincdec (e, op) ->
     (* TODO *) assert false
  | PEvars _ ->
     (* TODO *) assert false 

let found_main = ref false

(* 1. declare structures *)
(*A TESTER*)
let phase1 = function
  | PDstruct ({ ps_name = { id = id; loc = loc }} as s) -> 
      if Structs.is_defed s then 
        error loc "structure déjà définie"
      else Structs.add s
  | PDfunction _ -> ()


(*A TESTER*)
let rec sizeof = function
  | Tint | Tbool | Tstring | Tptr _ -> 8
  | Tstruct s -> Hashtbl.fold (fun _ b c -> c + sizeof (b.f_typ)) s.s_fields 0
  | Tmany l -> List.fold_left (fun i x -> i + sizeof (x)) 0 l

(* 2. declare functions and type fields *)

let rec is_well_formed = function
  | PTident { id = "int" } 
  | PTident { id = "bool" } 
  | PTident { id = "string" } -> true
  | PTptr ty -> is_well_formed ty
  | PTident ({id = s } as i)-> Structs.is_defed {ps_name = i;ps_fields = []}

(*RAJOUTER QUELS OBJETS DE LA FONCTION SONT MAL FORMES*)
let phase2 = function
  | PDfunction ({ pf_name={id; loc}; pf_params=pl; pf_typ=tyl; } as f) ->
     if List.for_all is_well_formed (List.map snd pl) && List.for_all is_well_formed tyl then 
      Funcs.add f
    else error loc ("types mal formés dans la fonction :"^id)
  | PDstruct ({ ps_name = {id; loc}; ps_fields = fl } as s)->
    if List.for_all is_well_formed (List.map snd fl) then 
      Structs.add s
    else error loc ("types mal formés dans la structure :"^id)

(* 3. type check function bodies *)
let decl = function
  | PDfunction { pf_name={id; loc}; pf_body = e; pf_typ=tyl } ->
    (* TODO check name and type *) 
    let f = { fn_name = id; fn_params = []; fn_typ = []} in
    let e, rt = expr Env.empty e in
    TDfunction (f, e)
  | PDstruct {ps_name={id}} ->
    (* TODO *) let s = { s_name = id; s_fields = Hashtbl.create 5 } in
     TDstruct s

let file ~debug:b (imp, dl) =
  debug := b;
  (* fmt_imported := imp; *)
  List.iter phase1 dl;
  List.iter phase2 dl;
  if not !found_main then error dummy_loc "missing method main";
  let dl = List.map decl dl in
  Env.check_unused (); (* TODO variables non utilisees *)
  if imp && not !fmt_used then error dummy_loc "fmt imported but not used";
  dl
