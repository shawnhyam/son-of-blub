(*
  Obviously there were a couple of issues with inputting instructions for
  the interpreter to run.  First, you were forced to hand code the data
  structure, and second, you were forced to hand-code the variable bindings
  by indicating the frame and offset indices to find the value of the
  variable.  In this installment, we will automatically do the variable
  lookups and get a tiny bit closer to having a 'nice' input format.

  Before that, however, we will get to the point of all this -- running the
  LLVM JIT compiler.  In this installment the compiler will be the
  bare minimum, but the basics will be in place.  We will pass in a function
  to be compiled and get an llvalue back representing the code to be
  executed, and then we will call that function with some arguments and
  get a result.  This is the basic core that will slowly but surely build
  up into something impressive.

  <look at code here>

  Also add a simple let-statement that is like this:
  (let (x y) expr) ==> ((lambda (x) expr) y)

  ((lambda (a x y) (if a x y)) #t 34 37)

  (((lambda (y) (lambda (a x) (if a x y))) 37) #t 34) -- even C can't do that!
  (let (y 37) ((lambda (a x) (if a x y)) #t 34))
*)

module EE = Llvm_executionengine.ExecutionEngine
module GV = Llvm_executionengine.GenericValue

type variable = Variable of string

type ast =
    Ast_lit of sval
  | Ast_ref of variable * int * int  (* variable; frame #; offset # *)
  | Ast_cnd of ast * ast * ast  (* if part; then part; else part *)
  | Ast_app of ast * ast array
  | Ast_abs of variable array * ast

and lambda = {
  lam_ast : ast;
  lam_params : variable array;
}

and sval =
    Sclosure of sclosure
  | Sllvm of Llvm.lltype * GV.t
  | Sunbound

and sclosure = {
  close_env : environment;
  close_lam : lambda;
  (* we could either have this as part of the lambda (compiled without a
     local environment) or as part of a closure (compiled in an environment) *)
  mutable close_jitcode : (Llvm.lltype * Llvm.llvalue) option  (* return value; code *)
}

and environment_frame = {
  env_frame_vars : variable array;  (* mostly for debugging *)
  env_frame_vals : sval array
}

and environment = environment_frame list

(* LLVM SECTION *)

exception Jit_failed
open Llvm

let make_bool value = 
  Sllvm (Llvm.i1_type, GV.of_int Llvm.i1_type (if value then 1 else 0)) ;;
let make_int value = Sllvm (Llvm.i64_type, GV.of_int Llvm.i64_type value) ;;

let get_type ast (typeinfo:(ast * lltype) list) =
  List.assq ast typeinfo
;;

let llvalue_of_gv t v =
  match t with
      i1_type -> const_int t (GV.as_int v)
    | i64_type -> const_of_int64 t (GV.as_int64 v) true (* WTF *)
    | _ -> Format.printf "FAIL\n%!"; raise Jit_failed
;;
 

let cur_module = Llvm.create_module "helloworld" ;;
let jit = EE.create (ModuleProvider.create cur_module) ;;

let compile_fn (env:environment) (ast:ast) typeinfo = 
  let fn_type = try get_type ast typeinfo 
  with Not_found -> Format.printf "OH OH\n%!"; raise Jit_failed in
  let rettype = return_type fn_type in
  let argtypes = param_types fn_type in

    let cur_fn = define_function "lambda" fn_type cur_module in
    let builder = builder_at_end (entry_block cur_fn) in
    
    let params, body = match ast with
	Ast_abs (params, body) -> params, body
      | _ -> Format.printf "FAILED\n%!"; raise Jit_failed
    in
    let rec gen_llvm (builder:llbuilder) (ast:ast) : (llbuilder * llvalue) =
      match ast with
	  Ast_lit (Sllvm (t, v)) -> 
	    let lit_val = llvalue_of_gv t v in
	    (builder, lit_val)
	| Ast_ref (var, 0, offset) ->
	    assert (ExtArray.Array.mem var params);
	    (builder, param cur_fn offset)
	| Ast_ref (var, frame, offset) -> 
	    Format.printf "(%d, %d) with %d frames" frame offset (List.length env);
	    let v = match (List.nth env (frame-1)).env_frame_vals.(offset) with
		Sllvm (t, v) -> llvalue_of_gv t v 
	      | _ -> raise Jit_failed
	    in
	    (builder, v)
	| Ast_cnd (pred, cons, alt) ->
	  let builder, pred_val = gen_llvm builder pred in
	  let test = build_icmp Icmp.Ne pred_val (const_int i1_type 0) 
	    "test" builder in
      
	  let cons_block = append_block "true_branch" cur_fn in
	  let alt_block = append_block "false_branch" cur_fn in
	  let join_block = append_block "join_branches" cur_fn in
	  ignore (build_cond_br test cons_block alt_block builder);
      
	  let cons_builder = builder_at_end cons_block in
	  let cons_builder, v1 = gen_llvm cons_builder cons in
	  ignore (build_br join_block cons_builder);
	
	  let alt_builder = builder_at_end alt_block in
	  let alt_builder, v2 = gen_llvm alt_builder alt in
	  ignore (build_br join_block alt_builder);

	  let join_builder = builder_at_end join_block in
	  let return = build_phi [(v1, cons_block); (v2, alt_block)] 
	    "phi" join_builder in
	  (join_builder, return)
    in

    let builder, retval = gen_llvm builder body in
    ignore (build_ret retval builder);
    (rettype, cur_fn)

  
;;


(* INTERPRETER SECTION *)

let eval typeinfo env ast =
  let rec eval env = function 
    Ast_lit lit -> lit
  | Ast_ref (_, frame, offset) -> (List.nth env frame).env_frame_vals.(offset)
  | Ast_cnd (pred, cons, alt) -> begin
      (* we will just treat it as an i1_type because in general we won't
	 be converting the Llvm values into svals *)
      (* also note that this is Scheme-style semantics, where everything
	 other than #f is considered #t *)
      match eval env pred with
	  Sllvm (t, v) when t = Llvm.i1_type && (GV.as_int v = 0) ->
	    eval env alt
	| _ -> eval env cons
    end
  | Ast_app (fn, args) -> apply (eval env fn) (Array.map (eval env) args)    
  | Ast_abs (params, body) as ast ->
      let jitcode = 
	try
	  Format.printf "Try compiling\n%!";
	  let c = compile_fn env ast typeinfo in
	  Format.printf "Got code\n%!";
	  Some c
	with Jit_failed -> None
      in
      Sclosure { close_env = env; 
		 close_lam = { lam_ast = body;
			       lam_params = params };
		 close_jitcode = jitcode }
and apply fn args =
  match fn with
      Sclosure close -> begin
	match close.close_jitcode with
	    Some (rettype, fn) -> 
	      let args = Array.map (fun (Sllvm (_, v)) -> v) args in
	      let result = EE.run_function fn args jit in
	      Format.printf "Ran code\n%!";
	      Sllvm (rettype, result)

	  | None ->
	      let frame = { env_frame_vars = close.close_lam.lam_params;
			    env_frame_vals = args } in
	      let env' = frame :: close.close_env in
	      eval env' close.close_lam.lam_ast
      end
  in
  eval env ast
;;

let make_var name = Variable name ;;

let make_lit gv = Ast_lit gv ;;
let make_ref var frame offset = Ast_ref (var, frame, offset) ;;
let make_cnd pred cons alt = Ast_cnd (pred, cons, alt) ;;
let make_abs params body = Ast_abs (Array.of_list params, body) ;;
let make_app fn args = Ast_app (fn, Array.of_list args) ;;

module Input = struct
  type node =
      Lit of myval
    | Ref of string
    | Cnd of node * node * node
    | Abs of string list * node
    | App of node * node list
  and myval =
      Bool of bool
    | Int of int

  let find var env =
    let rec find frame_idx = function
	[] -> assert false
      | { env_frame_vars = vars } :: env -> 
	  try 
	    frame_idx, ExtArray.Array.findi ((=) var) vars
	  with Not_found -> find (frame_idx+1) env
    in
    find 0 env
  ;;

  let rec to_ast env = function
      Lit (Bool x) -> make_lit (make_bool x)
    | Lit (Int x) -> make_lit (make_int x)
    | Ref v -> 
	let variable = Variable v in
	let frame, offset = find variable env in
	Ast_ref (variable, frame, offset)
    | Cnd (pred, cons, alt) -> Ast_cnd (to_ast env pred,
					to_ast env cons,
					to_ast env alt)
    | Abs (params, body) -> 
	let vars = Array.of_list (List.map (fun v -> Variable v) params) in
	let vals = Array.map (fun _ -> Sunbound) vars in
	let frame = { env_frame_vars = vars;
		      env_frame_vals = vals } in
	let env = frame :: env in
	Ast_abs (vars, to_ast env body)
    | App (fn, args) ->
	Ast_app (to_ast env fn, Array.of_list (List.map (to_ast env) args))
end


let () =
  let lit_bool x = Input.Lit (Input.Bool x) in
  let lit_int x = Input.Lit (Input.Int x) in
  let app fn args = Input.App (fn, args) in
  let abs params body = Input.Abs (params, body) in
  let cnd pred cons alt = Input.Cnd (pred, cons, alt) in
  let var v = Input.Ref v in

  let lambda = abs ["a"; "x"; "y"] (cnd (var "a") (var "x") (var "y")) in
  let expr = app lambda [lit_bool true; lit_int 34; lit_int 47] in
  let expr = Input.to_ast [] expr in
  let Ast_app (lambda, _) = expr in 

  let result = eval [lambda, function_type i64_type 
		       [| i1_type; i64_type; i64_type |] ]
    [] expr
  in

  let () = match result with
      Sllvm (t, v) when t = Llvm.i64_type -> assert (GV.as_int v = 34)
    | _ -> assert false
  in
  

  let lambda1 = abs ["a"; "x"] (cnd (var "a") (var "x") (var "y")) in
  let lambda2 = abs ["y"] lambda1 in
  let expr = app lambda2 [lit_int 37] in
  let expr = app expr [lit_bool true; lit_int 34] in
  let expr = Input.to_ast [] expr in

  let Ast_app (Ast_app (Ast_abs (_, lambda), _), _) = expr in

  let result = eval [lambda, function_type i64_type
		       [| i1_type; i64_type |] ] [] expr in
  let () = match result with
      Sllvm (t, v) when t = Llvm.i64_type -> assert (GV.as_int v = 34)
    | _ -> assert false
  in
  dump_module cur_module
;;


(* add a Let statement *)
