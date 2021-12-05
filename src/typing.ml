
open Format
open Lib
open Ast
open Tast

(*A supprimer*)
open Lexing
let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string

let report_loc (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol in
  let lc = e.pos_cnum - b.pos_bol in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" "" l fc lc


let error loc e = raise (Error (loc, e))

module Pstructs = struct
  module M = Map.Make(String)
  type t = pstruct M.t

  let empty = M.empty
  let all_pstructs = ref empty
  let empty_struct = {ps_name = {id = "";loc = dummy_loc};ps_fields = []}
  let find = fun x -> M.find x !all_pstructs
  let is_defed s = M.mem s !all_pstructs
  let add_name s = 
    if is_defed s.ps_name.id then 
      error s.ps_name.loc ("struct "^s.ps_name.id^" is defined twice")
  else
    all_pstructs := M.add s.ps_name.id empty_struct !all_pstructs
  let add s = all_pstructs := M.add s.ps_name.id s !all_pstructs     
  let are_fields_unique (s : pstruct) = 
    let table = Hashtbl.create 15 in
    let rec add_table = function
      |[] -> ()
      |h::t -> 
        if Hashtbl.mem table (fst h).id then
          error (fst h).loc ("the field "^(fst h).id^" is already defined twice")
        else (
          Hashtbl.add table (fst h).id ();
          add_table t
        )
    in try add_table s.ps_fields;true with _ -> false  


    let iter f = M.iter f !all_pstructs

end

module Structs = struct
  module M = Map.Make(String)
  type t = structure M.t

  let (empty : t) = M.empty
  let all_structs = ref empty
  let find = fun x -> M.find x !all_structs
  let add s = all_structs := M.add s.s_name s !all_structs     
  let iter f = M.iter f !all_structs

  let card () = M.cardinal !all_structs

end



(*all_funcs contient une map qui à un string associe la pfunc associée*)
module Funcs = struct
  module M = Map.Make(String)
  type t = pfunc M.t
  let empty = M.empty
  let all_funcs = ref empty
  let find = fun x -> M.find x !all_funcs
  let is_defed f = M.mem f.pf_name.id !all_funcs
  let add f = 
    if is_defed f then 
      error f.pf_name.loc ("function "^f.pf_name.id^" is defined twice")
  else
      all_funcs := M.add f.pf_name.id f !all_funcs
  let are_vars_unique f = 
    let table = Hashtbl.create 15 in
    let rec add_table = function
      |[] -> ()
      |h::t -> 
        if Hashtbl.mem table (fst h).id then
          error (fst h).loc ("the variable "^(fst h).id^" is already defined twice")
        else (
          Hashtbl.add table (fst h).id ();
          add_table t
        )
    in try add_table f.pf_params;true with _ -> false
end


let rec pstruct_to_struct s = 
  try Structs.find s with _ ->
  if not (Pstructs.is_defed s) then
    error dummy_loc ("unknown struct "); 
  let struc = Pstructs.find s in
  let n = List.length struc.ps_fields in
  let ns = {
    s_name = s;
    s_fields = Hashtbl.create n
  } in
  Structs.add ns;
  let h = (Structs.find s).s_fields in
  let rec aux = function
  |[] -> ()
  |(p,t)::r -> Hashtbl.add h p.id 
    {
      f_name = p.id;
      f_typ =  type_type t;
      f_ofs = 0 
    };
    aux r
  in aux struc.ps_fields;
  ns

and type_type a = 
  match a with
  | PTident { id = "int" } -> Tint
  | PTident { id = "bool" } -> Tbool
  | PTident { id = "string" } -> Tstring
  | PTptr ty -> Tptr (type_type ty)
  | PTident {id = s } -> Tstruct (pstruct_to_struct s)
  
let rec typstr = function
  | Tint -> "int"
  | Tbool ->  "bool"
  | Tstring -> "string"
  | Tstruct s -> s.s_name
  | Tptr ty -> "*"^(typstr ty)
  | Tmany [x] -> typstr x
  | Tmany (h::t) -> (typstr h)^" , "^typstr (Tmany t)
  | Tmany [] -> "" 

let rec eq_type ty1 ty2 = match ty1, ty2 with
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring -> true
  | Tstruct s1, Tstruct s2 -> s1.s_name = s2.s_name
  | Tptr ty1, Tptr ty2 -> eq_type ty1 ty2
  | Tmany man1, Tmany man2 -> List.filter (fun x -> not (List.mem x man1 )) man2 = []
  | _ -> false
    (* TODO autres types *)



let is_recursive (s : structure) =
  let n = s.s_name in
  let res = ref false in
  let rec aux f = 
    match f.f_typ with
      |Tstruct s' ->  
        if s'.s_name = n then res := true
        else Hashtbl.iter (fun _ -> fun x -> aux x) s'.s_fields
      |_ -> ()
    in Hashtbl.iter (fun _ -> fun x -> aux x) s.s_fields;
    !res

    
let fmt_used = ref false
let fmt_imported = ref false

let evar v = { expr_desc = TEident v; expr_typ = v.v_typ }

let new_var =
  let id = ref 0 in
  fun x loc ?(used=false) ty ->
    incr id;
    { v_name = x; v_id = !id; v_loc = loc; v_typ = ty; v_used = used; v_addr = false }


let tvoid = Tmany []


module Env = struct
  module M = Map.Make(String)
  type t = var M.t
  let empty = M.empty
  let find = M.find
  let add env v = M.add v.v_name v env
  let all_vars = ref []
  let check_unused () =
    let check v =
      if v.v_name <> "_" && not v.v_used then error v.v_loc ("unused variable "^v.v_name) in
    List.iter check !all_vars


  let var x loc ?used ty env =
    let v = new_var x loc ?used ty in
    all_vars := v :: !all_vars;
    add env v, v

  let new_env = fst (var "_" dummy_loc tvoid empty)
  
  (* TODO type () et vecteur de types *)
end

let l_to_typ = function
  |[x] -> x
  |_ as a -> Tmany a

let typ_to_l = function
  |Tmany a -> a
  |_ as a -> [a]

let make d ty = { expr_desc = d; expr_typ = ty }
let stmt d = make d tvoid

let rec is_l_value e = match e.expr_desc with
  |TEident(_) -> true
  |TEdot(el,_) -> is_l_value el
  |TEunop(Ustar,el) -> (
    match el.expr_typ with 
      |Tptr _ -> el.expr_desc <> TEnil
      |_ -> false
    ) 
  |_ -> false

let rec flatten = function
  |[] -> []
  |(Tmany a)::t -> (flatten a)@(flatten t)
  |h::t -> h::(flatten t)
  

let errtyp loc exp real =
  error loc ("type "^(typstr exp)^" expected, found "^(typstr real)^" instead") 




let correct_assign loc nlvl nel =
  let f2 = flatten (List.map (fun x -> x.expr_typ) nel) in
  let rec aux (a,b) = match a,b with
    |[],[] -> ()
    |{expr_desc = TEident {v_name = "_"}}::t1,_::t2 -> aux (t1,t2)
    |{expr_typ = Tptr(_)}::t1,Tptr(tvoid)::t2 -> aux (t1,t2)
    |{expr_typ = Tptr(a)}::t1,b::t2 -> 
      if not (eq_type a b || eq_type (Tptr a) b) then  
        error loc ("wrong type assignation : "^(typstr (Tptr a))^" not compatible with "^typstr b);
        aux (t1,t2) 
    |{expr_typ = a}::t1,b::t2 -> 
      if not (eq_type a b) then 
        error loc ("wrong type assignation : "^(typstr a)^" not compatible with "^typstr b);
       aux (t1,t2) 
    |l,[] -> error loc ("not enough assignations, missing "^(string_of_int (List.length l))^" r-values")
    |[],l -> error loc ("not enough assignations, missing "^(string_of_int (List.length l))^" l-values")
in aux (nlvl,f2)

let ret_type = ref tvoid

let rec expr env e =
 let e, ty, rt = expr_desc env e.pexpr_loc e.pexpr_desc in
  make e ty, rt

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
            errtyp loc Tint (fst te1).expr_typ;
          if (fst te2).expr_typ <> Tint then
            errtyp loc Tint (fst te2).expr_typ;
          TEbinop(op,fst te1, fst te2), Tint,false
        | Beq | Bne -> 
          if (fst te1).expr_desc = TEnil then 
            error loc "nil present in left-hand side of comparison";
          if (fst te2).expr_desc = TEnil then 
            error loc "nil present in right-hand side of comparison";
          TEbinop(op,fst te1,fst te2),Tbool,false
        | Blt | Ble | Bgt | Bge -> 
          if (fst te1).expr_typ <> Tint  then
            errtyp loc Tint (fst te1).expr_typ;
          if (fst te2).expr_typ <> Tint  then
            errtyp loc Tint (fst te2).expr_typ;
          TEbinop(op,fst te1, fst te2), Tbool,false
        | Band | Bor -> 
          if (fst te1).expr_typ <> Tbool then
            errtyp loc Tbool (fst te1).expr_typ;
          if (fst te2).expr_typ <> Tbool then
            errtyp loc Tbool (fst te2).expr_typ;
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
        |_ -> error e1.pexpr_loc "can't apply unary operation to this expression!");
      TEunop(op,son),son.expr_typ,false
  | PEcall ({id = "fmt.Print"}, el) ->
      if not !fmt_imported then 
        error loc "fmt used but not imported";
      fmt_used := true;
      let l = List.map (fun x -> fst (expr env x)) el in
      TEprint l, tvoid, false
  | PEcall ({id="new"}, [{pexpr_desc=PEident {id;loc}}])->
      let ty =  try Tstruct (Structs.find id) with _ -> error loc ("no such type " ^ id)
      in TEnew ty, Tptr ty, false
  | PEcall ({id="new"}, _) ->
      error loc "new expects a type"
  | PEcall (id, el) ->
      (*tests possiblement nécessaires 
      pour vérifier le bon typage des fonctions*)
      let af = Funcs.find id.id in
      let f = 
        {fn_name = af.pf_name.id;
        fn_params = List.map 
          (fun (p,t) ->
            new_var 
              p.id 
              p.loc 
              (type_type t)
          ) af.pf_params;
        fn_typ =  List.map type_type af.pf_typ;
        } in 
        let exprs = List.map (fun x -> fst (expr env x)) el in
        let ty = l_to_typ f.fn_typ in
      TEcall(f,exprs),ty,false
  | PEfor (e, b) ->
      let (ne,r1) = expr env e
      and (nb,r2) =  expr env b in
      if ne.expr_typ <> Tbool then
        errtyp loc Tbool ne.expr_typ;
      TEfor(ne,nb),tvoid,false
  | PEif (e1, e2, e3) ->
      let returns = ref false in
      let ne1,_ = expr env e1 
      and ne2,b2 = expr env e2 
      and ne3,b3 = expr env e3 in
      if ne1.expr_typ <> Tbool then
        errtyp loc Tbool ne1.expr_typ;
      returns := b2 && b3;
      TEif(ne1,ne2,ne3),
      (if !returns then ne1.expr_typ else tvoid),
      !returns
  | PEnil -> TEnil ,Tptr(tvoid),false 
  | PEident {id=id} ->
    if id = "_" then error loc " _ is not a variable";
     (try 
        let v = Env.find id !env in
         v.v_used <- true;
         TEident v, v.v_typ, false
      with Not_found -> error loc ("unbound variable " ^ id))
  | PEdot (e, id) ->
      let (ne,r) = expr env e in
      let s = match ne.expr_typ with
        |Tstruct a 
        |Tptr(Tstruct a)-> pstruct_to_struct a.s_name
        |a -> error loc ("type "^(typstr a)^" doesn't have any method")
      in 
      if not (Hashtbl.mem s.s_fields id.id) then
        error loc ("structure "^s.s_name^" doesn't have method "^id.id)
      else 
        let field = Hashtbl.find s.s_fields id.id in
        TEdot(ne,field),field.f_typ,false
  | PEassign (lvl, el) ->
      let rec aux = function
        | [] -> []
        | {pexpr_desc = PEident {id=id}; pexpr_loc}::t -> 
          (try 
            let v = Env.find id !env in
            {expr_desc = TEident v;expr_typ = v.v_typ}::aux t
          with Not_found -> error pexpr_loc ("unbound variable " ^ id)) 
        | h::t -> (fst (expr env h))::(aux t)
      in
      let nlvl = aux lvl
      and nel = List.map (fun x -> fst (expr env x)) el in
      if not (List.for_all is_l_value nlvl) then
        error loc "ill-formed l-value";      
      correct_assign loc nlvl nel;
      TEassign (nlvl, nel), tvoid, false 
  | PEreturn el ->
      let sons = List.map (fun x -> fst (expr env x))  el in
      let ret = l_to_typ (List.map (fun x -> x.expr_typ) sons) in
      if not (eq_type ret !ret_type) then
        errtyp loc !ret_type ret;
      TEreturn sons , tvoid, true
  | PEblock el ->
      let curenv = !env in
      let has_a_return = ref false in
      let rec aux = function
        |[] -> ()
        |[(e,true)] -> has_a_return := true
        |[(_,false)] -> 
          if !has_a_return then error loc "return at the end of block expected";
        |(e,b)::t -> 
          if b then (
            has_a_return := true;
            if !ret_type <> tvoid && e.expr_typ <> !ret_type then
              errtyp loc !ret_type e.expr_typ;
            ret_type := e.expr_typ
          )
          else aux t
          in
      let sons = List.map (fun x -> expr env x) el in
      aux sons;
      env := curenv;
      TEblock (List.map fst sons), tvoid, !has_a_return
  | PEincdec(e, op) ->
      let (ne,_) = expr env e in
      if ne.expr_typ <> Tint then
        errtyp e.pexpr_loc Tint ne.expr_typ;
      if not (is_l_value ne) then
        error e.pexpr_loc "l-value expected";
      TEincdec(ne,op),tvoid,false
  | PEvars(ids,ptyps,pexprs) -> (
      match ptyps with
        |None -> 
          if pexprs = [] then
            error loc "empty declarations must be explicitly typed"
          else let types = List.map (fun x -> (fst (expr env x)).expr_typ) pexprs in
            (try 
              let vars = List.map2 
                (fun x y -> 
                  let next = Env.var x.id x.loc y !env in
                  env := fst next;
                  snd next) 
                ids (flatten types) in
              TEvars(vars),Tmany(types),false
            with (Invalid_argument _) -> error loc "incorrect number of elements returned" )
        |Some pt ->
          let p = type_type pt in 
          TEvars(
            List.map 
              (fun x -> 
                let next = Env.var x.id x.loc p !env in
                env := fst next;
                snd next) ids),
          tvoid,false
    )
    


let found_main = ref false

(* 1. declare structures *)
(*A TESTER*)
let phase1 = function
  | PDstruct ({ ps_name = { id = id; loc = loc }} as s) -> 
      if Pstructs.is_defed id then 
        error loc ("structure "^id^" defined twice")
      else Pstructs.add_name s
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
  | PTident ({id = s })-> Pstructs.is_defed s


  let phase2 = function
  | PDfunction ({ pf_name={id; loc}; pf_params=pl; pf_typ=tyl; } as f) ->
    if id="main" then (
     if pl <> [] then error loc "function main can't have any argument";
     if tyl <> [] then error loc "function main must have no return";
     found_main := true);
     if List.for_all is_well_formed (List.map snd pl) && List.for_all is_well_formed tyl then 
      Funcs.add f
    else error loc ("fonction "^id^" is ill-formed")
  | PDstruct ({ ps_name = {id; loc}; ps_fields = fl } as s)->
    if not (List.for_all is_well_formed (List.map snd fl)) then 
      error loc ("ill-formed types in structure :"^id);
    if not (Pstructs.are_fields_unique s) then
      error loc ("duplicate attributes in structure "^s.ps_name.id);
    Pstructs.add s


(* 3. type check function bodies *)
let decl = function
  | PDfunction { pf_name={id; loc}; pf_params = params;pf_body = e; pf_typ=tyl } ->
    let return_type = List.map type_type tyl in
    ret_type := l_to_typ return_type;
    let f = { fn_name = id; fn_params = []; fn_typ = []} in
    let env = ref Env.new_env in
    List.iter (fun (p,t) -> env := fst (Env.var p.id p.loc (type_type t) !env)) params ;
    let e, rt = expr env e in
    if !ret_type <> tvoid && not rt then error loc "return expected";
    TDfunction (f, e)
  | PDstruct {ps_name={id}} ->
     let s = { s_name = id; s_fields = Hashtbl.create 5 } in
     TDstruct s


let file ~debug:b (imp, dl) =
  debug := b;
  fmt_imported := imp; 
  List.iter phase1 dl;
  List.iter phase2 dl;
  Pstructs.iter (fun st -> fun _ -> Structs.add (pstruct_to_struct st));
  if not !found_main then error dummy_loc "main not found";
  let dl = List.map decl dl in
  Structs.iter (fun _ -> fun s -> if is_recursive s then error dummy_loc ("structure "^s.s_name^" is recursive") else ());
  Env.check_unused (); 
  if imp && not !fmt_used then error dummy_loc "fmt imported but not used";
  dl
