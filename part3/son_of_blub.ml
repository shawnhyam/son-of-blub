(*
  This installment is oriented around running the fib function

  We have to be able to create a new symbol in the global environment, but
  we will fudge around that for now.

  Must have support for integer * + and -
    (lambda (n) (+ n 1))

  Must have support in the JIT compiler for invoking other functions; in
  this case, it is a recursive invocation.
    (define fact (lambda (n) (if (= n 0) 1 ( * n (fact (- n 1))))))

  Use the fast calling convention.
*)

module EE = Llvm_executionengine.ExecutionEngine
module GV = Llvm_executionengine.GenericValue
module L = Llvm

type variable = Variable of string

type ast =
    Ast_lit of sval
  | Ast_ref of variable 
  | Ast_cnd of ast * ast * ast
  | Ast_app of ast * ast array
  | Ast_abs of lambda
  | Ast_define of variable * ast

and lambda = {
  lam_ast : ast;
  lam_params : variable array;
  lam_lltype : L.lltype option;  (* this is a hack because we don't have a
				    type system of any kind yet *)
}

and sval =
    Sclosure of sclosure    (* closure is a fn + env *)
  | Sllvm of L.lltype * GV.t
  | Sllvminst of sinstbuilder
  | Sprimfn of sprimitive
  | Sunbound

and sinstbuilder = L.llvalue list -> L.llbuilder -> (L.llbuilder * L.llvalue)

and environment = frame list

and frame = {
  frame_vars : variable array;
  frame_vals : sval array
}

and sclosure = {
  close_env : environment;
  close_lam : lambda;
  (* we could either have this as part of the lambda (compiled without a
     local environment) or as part of a closure (compiled in an environment) *)
  close_jitcode : sprimitive Lazy.t 
}

and sprimitive = L.lltype * L.llvalue  (* fn sig; code *)

(* BASIC UTILITIES *)

let find_in_env env v : (sval * int * int) =
  let rec find frame_idx = function
      [] -> raise Not_found
    | frame :: env ->
	try
	  let offset = ExtArray.Array.findi ((=) v) frame.frame_vars in
	  (frame.frame_vals.(offset), frame_idx, offset)
	with Not_found ->
	  find (frame_idx+1) env
  in
  find 0 env
;;

(* LLVM SECTION *)

exception Jit_failed
open Llvm

let llvm_val_of_int t x = Sllvm (t, GV.of_int t x) ;;
let make_bool value = llvm_val_of_int L.i1_type (if value then 1 else 0) ;;
let make_int value = llvm_val_of_int L.i64_type value ;;

let llvalue_of_gv t v =
  match t with
      i1_type -> const_int t (GV.as_int v)
    | i64_type -> const_of_int64 t (GV.as_int64 v) true (* WTF is the bool for? *)
    | _ -> Format.printf "FAIL\n%!"; raise Jit_failed
;;
 
let cur_module = L.create_module "helloworld" ;;
let jit = EE.create (ModuleProvider.create cur_module) ;;

let compile_fn (env:environment) (lambda:lambda) = 
  (* add the params to the environment; but the params should be accessed
     differently from the surrounding frames *)
  let frame = { frame_vars = lambda.lam_params;
		frame_vals = Array.map (fun _ -> Sunbound) lambda.lam_params } in
  let env = frame :: env in
  let fn_type = match lambda.lam_lltype with
      Some t -> t | None -> raise Jit_failed
  in
  let cur_fn = define_function "lambda" fn_type cur_module in

  (* MAJOR MAJOR HACK to get fact function to work *)
  let global_frame = List.nth env ((List.length env)-1) in
  if (Array.length global_frame.frame_vals > 4) then
    global_frame.frame_vals.(4) <- Sprimfn (fn_type, cur_fn);

  let builder = builder_at_end (entry_block cur_fn) in

  let rec gen_llvm (builder:llbuilder) (ast:ast) : (llbuilder * llvalue) =
    match ast with
	Ast_lit (Sllvm (t, v)) -> 
	  let lit_val = llvalue_of_gv t v in
	  (builder, lit_val)
      | Ast_lit _ -> raise Jit_failed  (* can't handle other types yet *)
      | Ast_ref var -> begin
	  try
	    match find_in_env env var with
		_, 0, offset -> (builder, param cur_fn offset)
	      | Sllvm (t, v), _, _ -> (builder, llvalue_of_gv t v)
	      | _ -> raise Jit_failed
	  with Not_found ->
	    raise Jit_failed
	end
      | Ast_cnd (pred, cons, alt) ->
	  let builder, pred_val = gen_llvm builder pred in
	  
	  let cons_block = append_block "true_branch" cur_fn in
	  let alt_block = append_block "false_branch" cur_fn in
	  let join_block = append_block "join_branches" cur_fn in
	  ignore (build_cond_br pred_val cons_block alt_block builder);
      
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
      | Ast_abs _ -> raise Jit_failed  (* coming soon... *)
      | Ast_app (Ast_ref var, params) -> begin
	  (* compile the code for each of the params *)
	  let builder, llvals = Array.fold_right
	    (fun param (builder, llvals) ->
	       let builder', llval = gen_llvm builder param in
	       builder', llval :: llvals)
	    params (builder, [])
	  in

	  match find_in_env env var with
	      Sllvminst buildfn, _, _ -> buildfn llvals builder
	    | Sprimfn (_, fn), _, _ ->
		(builder, build_call fn (Array.of_list llvals) "" builder)
	    | _ -> raise Jit_failed
	end
	  
      | Ast_app _ -> raise Jit_failed  (* also coming soon? *)
    in

    let builder, retval = gen_llvm builder lambda.lam_ast in
    ignore (build_ret retval builder);
    (fn_type, cur_fn)
;;


(* INTERPRETER SECTION *)

let rec eval env = function 
    Ast_lit lit -> lit
  | Ast_ref var -> begin
      try
	let v, _, _ = find_in_env env var in v
      with Not_found as ex ->
	let Variable v = var in
	Format.printf "Variable %s not found\n%!" v;
	raise ex
    end
  | Ast_cnd (pred, cons, alt) -> begin
      (* we will just treat it as an i1_type because in general we won't
	 be converting the Llvm values into svals *)
      (* also note that this is Scheme-style semantics, where everything
	 other than #f is considered #t *)
      match eval env pred with
	  Sllvm (t, v) when t = L.i1_type && (GV.as_int v = 0) ->
	    eval env alt
	| _ -> eval env cons
    end
  | Ast_app (fn, args) -> apply (eval env fn) (Array.map (eval env) args)    
  | Ast_abs lambda ->
      Sclosure { close_env = env; 
		 close_lam = lambda;
		 close_jitcode = lazy (compile_fn env lambda) }
and apply fn args =
  match fn with
      Sclosure close -> begin
	try
	  let fntype, fn = Lazy.force close.close_jitcode in
	  let args = Array.map (function
				    Sllvm (_, v) -> v
				  | _ -> raise Jit_failed) args in
	  let result = EE.run_function fn args jit in
	  Sllvm (return_type fntype, result)
	with Jit_failed ->
	  let frame = { frame_vars = close.close_lam.lam_params;
			frame_vals = args } in
	  let env' = frame :: close.close_env in
	  eval env' close.close_lam.lam_ast
      end
    | _ -> assert false  (* no other values we can 'apply' yet *)
;;


(* TEST SECTION *)

let mkvar name = Variable name ;;
let mklit gv = Ast_lit gv ;;
let lit_bool x = mklit (make_bool x) ;;
let lit_int x = mklit (make_int x) ;;
let mkref var = Ast_ref (mkvar var) ;;
let mkcnd pred cons alt = Ast_cnd (pred, cons, alt) ;;
let mkabs params body lltype = 
  Ast_abs { lam_params = Array.of_list (List.map mkvar params);
	    lam_ast = body;
	    lam_lltype = lltype } ;;
let mkapp fn args = Ast_app (fn, Array.of_list args) ;;
let mkdef name ast = Ast_define (mkvar name, ast)

let () =
  let lambda = mkabs ["a"; "x"; "y"] 
    (mkcnd (mkref "a") (mkref "x") (mkref "y"))
    (Some (function_type i64_type [| i1_type; i64_type; i64_type |]))
  in
  let expr = mkapp lambda [lit_bool true; lit_int 34; lit_int 47] in
  let result = eval [] expr in

  match result with
      Sllvm (t, v) when t = L.i64_type -> assert (GV.as_int v = 34)
    | _ -> assert false
;;

let () =
  (* (lambda (a x) (if a x y)) *)
  let lambda1 = mkabs ["a"; "x"] 
    (mkcnd (mkref "a") (mkref "x") (mkref "y"))
    (Some (function_type i64_type [| i1_type; i64_type |]))
  in
  (* this is a function that takes param 'y' and returns a function with
     2 params, 'a' and 'x' -- this function return 'x' if 'a' is #t,
     'y' otherwise   (lambda (y) (lambda (a x) (if a x y)))   *)
  let lambda2 = mkabs ["y"] lambda1 None in (* can't type this _yet_ *)
  let expr = mkapp lambda2 [lit_int 47] in
  let result = eval [] expr in
  (* result should be a function that takes 2 params 'a' and 'x' *)
  let () = match result with
      Sclosure _ -> ()
    | _ -> assert false
  in

  (* invoke this result with 2 args #f and 34, should return 47 *)
  let expr' = mkapp (mklit result) [lit_bool false; lit_int 34] in
  let () = match eval [] expr' with
      Sllvm (t, v) when t = L.i64_type -> assert (GV.as_int v = 47)
    | _ -> assert false
  in

  (* invoke with 2 args #t and 34, should return 34 *)
  let expr' = mkapp (mklit result) [lit_bool true; lit_int 34] in
  let () = match eval [] expr' with
      Sllvm (t, v) when t = L.i64_type -> assert (GV.as_int v = 34)
    | _ -> assert false
  in
  ()
;;


let gen_bin_op op =
  Sllvminst (fun [x; y] builder ->
		  (builder, op x y "" builder))
;;

let global_vars = [| Variable "+"; 
		     Variable "-"; 
		     Variable "*";
		     Variable "=";
		     Variable "fact";
		     Variable "<" |] ;;
let global_vals = [| gen_bin_op build_add;
		     gen_bin_op build_sub;
		     gen_bin_op build_mul;
		     Sllvminst (fun [x; y] builder ->
				     (builder, build_icmp Icmp.Eq x y "" builder));
		     Sunbound;
		     Sllvminst (fun [x; y] builder ->
				     (builder, build_icmp Icmp.Slt x y "" builder)) |] ;;
let globals = { frame_vars = global_vars;
		frame_vals = global_vals } ;;

let () =
  let lambda = mkabs ["n"] (mkapp (mkref "+") [mkref "n"; lit_int 1])
    (Some (function_type i64_type [| i64_type |]))
  in
  let expr = mkapp lambda [lit_int 44] in
  let result = eval [globals] expr in
  let () = match result with
      Sllvm (t, v) when t = L.i64_type -> assert (GV.as_int v = 45)
    | _ -> assert false
  in
  ()
;;

let () =
  let lambda = mkabs ["n"] (mkcnd 
			      (mkapp (mkref "<") [mkref "n"; lit_int 3])
			      (lit_int 1)
			      (mkapp (mkref "+") 
				 [mkapp (mkref "fib") 
				    [mkapp (mkref "-") [mkref "n"; lit_int 2]];
				  mkapp (mkref "fib") 
				    [mkapp (mkref "-") [mkref "n"; lit_int 1]]]))
    (Some (function_type i64_type [| i64_type |]))
  in

  let bind_it = mkdef "fib" lambda in
  let _ = eval [globals] bind_it in

  let expr = mkapp (mkref "fib") [lit_int 40] in

  let result = eval [globals] expr in
  let () = match result with
      Sllvm (t, v) when t = L.i64_type -> 
	Format.printf "answer: %d\n%!" (GV.as_int v);
	assert (GV.as_int v = 102334155)
    | _ -> assert false
  in
  ()
;;
(*
let () =
  let lambda = mkabs ["n"] (mkcnd 
			      (mkapp (mkref "=") [mkref "n"; lit_int 1])
			      (lit_int 1)
			      (mkapp (mkref "*") 
				 [mkref "n";
				  (mkapp (mkref "fact") 
				     [mkapp (mkref "-") [mkref "n"; lit_int 1]])]))
    (Some (function_type i64_type [| i64_type |]))
  in
  let expr = mkapp lambda [lit_int 5] in
  let result = eval [globals] expr in
  let () = match result with
      Sllvm (t, v) when t = L.i64_type -> 
	Format.printf "answer: %d\n%!" (GV.as_int v);
	assert (GV.as_int v = 120)
    | _ -> assert false
  in
  ()
;;

*)


let () = L.dump_module cur_module ;;


