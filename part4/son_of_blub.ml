(*
  This installment is oriented around running the fib function

  We have to be able to create a new symbol in the global environment, but
  we will fudge around that for now.

  Must have support for integer * + and -
    (lambda (n) (+ n 1))

  Must have support in the JIT compiler for invoking other functions; in
  this case, it is a recursive invocation.
    (define fact (lambda (n) (if (< n 3) 1 ( * n (fact (- n 1))))))

  Use the fast calling convention.
*)

module EE = Llvm_executionengine.ExecutionEngine
module GV = Llvm_executionengine.GenericValue
module L = Llvm

type variable = Variable of string
type binding = Lref of (int * int) | Gref of int

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
  | Sunbound

and sinstbuilder = L.llvalue list -> L.llbuilder -> (L.llbuilder * L.llvalue)

and global_environment = (variable * sval) DynArray.t

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

(* PRETTY PRINTING AND DEBUGGING *)


let pp_var f (Variable v) = Format.fprintf f "@[%s@]" v

let _pp_list ppe f = function
    [] -> ()
  | a1::an ->
      let pprest f = List.iter (fun e -> Format.fprintf f "@ %a" ppe e) in
      Format.fprintf f "%a%a" ppe a1 pprest an
;;

let pp_list ppe f = function
    lst ->
      Format.fprintf f "@[(%a)@]" (_pp_list ppe) lst 
;;

let pp_pair pp1 pp2 f (v1,v2) =
  Format.fprintf f "@[<hv 1>(%a@ %a)@]" pp1 v1 pp2 v2
;;

let pp_sllvm f (t, v) =
  if (t = L.i1_type) && (GV.as_int v = 0) then Format.fprintf f "#f"
  else if (t = L.i1_type) && (GV.as_int v = 1) then Format.fprintf f "#t"
  else if (t = L.i64_type) then Format.fprintf f "%d" (GV.as_int v)
;;

let rec pp_ast f = function
    Ast_lit l -> pp_sval f l
  | Ast_ref r -> pp_var f r
  | Ast_cnd (e1, e2, e3) -> 
      Format.fprintf f "(if @[<hv>%a@ %a@ %a)@]" pp_ast e1 pp_ast e2 pp_ast e3
  | Ast_app (fn, params) ->
      Format.fprintf f "%a" (pp_list pp_ast) (fn :: (Array.to_list params))
  | Ast_abs lam ->
      Format.fprintf f "(fn @[<hv>%a@ %a)@]" (pp_list pp_var) (Array.to_list lam.lam_params) pp_ast lam.lam_ast
  | Ast_define (v, ast) ->
      Format.fprintf f "(define @[<hv>%a@ %a)@]" pp_var v pp_ast ast


and pp_sval f = function
    Sclosure { close_lam = lam; close_env = env } -> 
      Format.fprintf f "@[@,<#closure @[<hv>args:%a@ ast:%a@ env:%a>@]@]" (pp_list pp_var) (Array.to_list lam.lam_params) pp_ast lam.lam_ast pp_env env
  | Sllvm (t, v) -> Format.fprintf f "%a" pp_sllvm (t, v)

and pp_frame f frame =
  let lst = Array.to_list (Array.mapi (fun idx var -> (var, frame.frame_vals.(idx)))
                             frame.frame_vars)
  in
  Format.fprintf f "@[{#frame @[<hv>%a}@]@]" 
    (pp_list (pp_pair pp_var pp_sval)) lst

and pp_env f env =
  Format.fprintf f "@[{#env @[<hv>%a}@]@]" (pp_list pp_frame) env
;;


(* BASIC UTILITIES *)

exception Found of (sval * binding)

let find_in_env globals env v : (sval * binding) =
  let rec find frame_idx = function
      [] -> raise Not_found
    | frame :: env ->
	try
	  let offset = ExtArray.Array.findi ((=) v) frame.frame_vars in
	  (frame.frame_vals.(offset), Lref (frame_idx, offset))
	with Not_found ->
	  find (frame_idx+1) env
  in
  try
    DynArray.iteri (fun idx (var_name, var_val) ->
		      if var_name = v then 
			raise (Found (var_val, Gref idx)))
      globals;
    find 0 env
  with Found result ->
    result
;;

(* LLVM SECTION *)

exception Jit_failed

let llvm_val_of_int t x = Sllvm (t, GV.of_int t x) ;;
let make_bool value = llvm_val_of_int L.i1_type (if value then 1 else 0) ;;
let make_int value = llvm_val_of_int L.i64_type value ;;

let llvalue_of_gv t v =
  match t with
      i1_type -> L.const_int t (GV.as_int v)
    | i64_type -> L.const_of_int64 t (GV.as_int64 v) true (* WTF is the bool for? *)
    | _ -> Format.printf "FAIL\n%!"; raise Jit_failed
;;

let cur_module = L.create_module "helloworld" ;;
let jit = EE.create (L.ModuleProvider.create cur_module) ;;

let compile_fn ?fn globals (env:environment) (lambda:lambda) = 
  (* add the params to the environment; but the params should be accessed
     differently from the surrounding frames *)
  let frame = { frame_vars = lambda.lam_params;
		frame_vals = Array.map (fun _ -> Sunbound) lambda.lam_params } in
  let env = frame :: env in
  let fn_type = match lambda.lam_lltype with
      Some t -> t 
    | None -> 
	Format.printf "Unknown type for this function\n%!";
	raise Jit_failed
  in
  let cur_fn = match fn with
      None -> L.define_function "lambda" fn_type cur_module 
    | Some x -> x
  in
  let builder = L.builder_at_end (L.entry_block cur_fn) in

  let rec gen_llvm (builder:L.llbuilder) (ast:ast) : (L.llbuilder * L.llvalue) =
    match ast with
	Ast_lit (Sllvm (t, v)) -> 
	  let lit_val = llvalue_of_gv t v in
	  (builder, lit_val)
      | Ast_lit _ -> 
	  Format.printf "Can't handle Lit nodes yet\n%!";
	  raise Jit_failed
      | Ast_ref var -> begin
	  try
	    match find_in_env globals env var with
		_, Lref (0, offset) -> (builder, L.param cur_fn offset)
	      | Sllvm (t, v), _ -> (builder, llvalue_of_gv t v)
	      | _ -> 
		  Format.printf "Var lookup failed\n%!";
		  raise Jit_failed
	  with Not_found ->
	    let Variable v = var in
	    Format.printf "Var %s not currently bound in env\n%!" v;
	    raise Jit_failed
	end
      | Ast_cnd (pred, cons, alt) ->
	  let builder, pred_val = gen_llvm builder pred in
	  
	  let cons_block = L.append_block "true_branch" cur_fn in
	  let alt_block = L.append_block "false_branch" cur_fn in
	  let join_block = L.append_block "join_branches" cur_fn in
	  ignore (L.build_cond_br pred_val cons_block alt_block builder);
      
	  let cons_builder = L.builder_at_end cons_block in
	  let cons_builder, v1 = gen_llvm cons_builder cons in
	  ignore (L.build_br join_block cons_builder);
	
	  let alt_builder = L.builder_at_end alt_block in
	  let alt_builder, v2 = gen_llvm alt_builder alt in
	  ignore (L.build_br join_block alt_builder);

	  let join_builder = L.builder_at_end join_block in
	  let return = L.build_phi [(v1, cons_block); (v2, alt_block)] 
	    "phi" join_builder in
	  (join_builder, return)
      | Ast_abs _ -> 
	  Format.printf "Can't compile Abs node yet\n%!";
	  raise Jit_failed  (* coming soon... *)
      | Ast_app (Ast_ref var, params) -> begin
	  (* compile the code for each of the params *)
	  let builder, llvals = Array.fold_right
	    (fun param (builder, llvals) ->
	       let builder', llval = gen_llvm builder param in
	       builder', llval :: llvals)
	    params (builder, [])
	  in

	  match find_in_env globals env var with
	      Sllvminst buildfn, _ -> buildfn llvals builder
            | Sclosure { close_jitcode = jc }, _ ->
                let _, fn = Lazy.force jc in
		(builder, L.build_call fn (Array.of_list llvals) "" builder)
	    | _ -> 
		let Variable v = var in
		Format.printf "Var %s not found\n%!" v;
		raise Jit_failed
	end
	  
      | Ast_app _ -> 
	  Format.printf "App node not yet fully supported\n%!";
	  raise Jit_failed  (* also coming soon? *)
    in

    let builder, retval = gen_llvm builder lambda.lam_ast in
    ignore (L.build_ret retval builder);
    (fn_type, cur_fn)
;;


(* INTERPRETER SECTION *)

let eval (globals:global_environment) expr =
  let rec eval env ast = 
    let inner_eval env = function 
      Ast_lit lit -> lit
    | Ast_ref var -> begin
	try
	  let v, _ = find_in_env globals env var in v
	with Not_found as ex ->
	  let Variable v = var in
	  Format.printf "Variable %s not found\n%!" v;
	  raise ex
      end
    | Ast_cnd (pred, cons, alt) -> begin
	(* we will just treat it as an i1_type because in general we won't
	   be converting the Llvm values into svals *)
	match eval env pred with
	    Sllvm (t, v) when t = L.i1_type && (GV.as_int v = 0) ->
	      eval env alt
	  | _ -> eval env cons
      end
    | Ast_app (fn, args) -> apply (eval env fn) (Array.map (eval env) args)    
    | Ast_abs lambda ->
	Sclosure { close_env = env; 
		   close_lam = lambda;
		   close_jitcode = lazy (compile_fn globals env lambda) }
    | Ast_define (Variable v as var, Ast_abs lambda) -> begin
	let Some fn_type = lambda.lam_lltype in
	let idx = try
	  match find_in_env globals [] var with
	      _, Gref idx -> idx
	with Not_found ->
	  let i = DynArray.length globals in
	  DynArray.add globals (var, Sunbound);
	  i
	in
	let cur_fn = L.define_function v fn_type cur_module in
        DynArray.set globals idx 
          (var, Sclosure { close_env = [];
                           close_lam = lambda;
                           close_jitcode = Lazy.lazy_from_val (fn_type, cur_fn) });
        compile_fn ~fn:cur_fn globals env lambda;
	let _, sval = DynArray.get globals idx in
	sval
      end
    in
    let sval = inner_eval env ast in
    Format.printf "@[<hv 4>%a@ -->@ %a@]@." pp_ast ast pp_sval sval;
    sval
	
  and apply fn args =
    let prepare_llvm_args args =
      Array.map (function
		     Sllvm (_, v) -> v
		   | _ -> Format.printf "arg conversion failed\n%!";
		       raise Jit_failed) args
    in
    match fn with
	Sclosure close -> begin
	  try
	    let fntype, fn = Lazy.force close.close_jitcode in
	    let args = prepare_llvm_args args in
	    let result = EE.run_function fn args jit in
	    Sllvm (L.return_type fntype, result)
	  with Jit_failed ->
	    let frame = { frame_vars = close.close_lam.lam_params;
			  frame_vals = args } in
	    let env' = frame :: close.close_env in
	    eval env' close.close_lam.lam_ast
	end
  in
  eval [] expr
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
let mkdef name ast = Ast_define (mkvar name, ast) ;;


let gen_bin_op op =
  Sllvminst (fun [x; y] builder ->
		  (builder, op x y "" builder))
;;

let make_global_env bindings =
  DynArray.of_list (List.map (fun (var, sval) -> (Variable var, sval)) bindings)
;;

let globals = [ 
  "+", gen_bin_op L.build_add;
  "-", gen_bin_op L.build_sub;
  "*", gen_bin_op L.build_mul;
  "/", gen_bin_op L.build_sdiv;
  "=", Sllvminst (fun [x; y] builder ->
		    (builder, L.build_icmp L.Icmp.Eq x y "" builder));
  "<", Sllvminst (fun [x; y] builder ->
		    (builder, L.build_icmp L.Icmp.Slt x y "" builder));
  "<=", Sllvminst (fun [x; y] builder ->
		    (builder, L.build_icmp L.Icmp.Sle x y "" builder));
  ">", Sllvminst (fun [x; y] builder ->
		    (builder, L.build_icmp L.Icmp.Sgt x y "" builder));
  ">=", Sllvminst (fun [x; y] builder ->
		    (builder, L.build_icmp L.Icmp.Sge x y "" builder));

] ;;
		
let globals = make_global_env globals ;;

let () =
  let lambda = mkabs ["n"] (mkcnd 
			      (mkapp (mkref "<") [mkref "n"; lit_int 3])
			      (lit_int 1)
			      (mkapp (mkref "+") 
				 [mkapp (mkref "fibonacci") 
				    [mkapp (mkref "-") [mkref "n"; lit_int 2]];
				  mkapp (mkref "fibonacci") 
				    [mkapp (mkref "-") [mkref "n"; lit_int 1]]]))
    (Some (L.function_type L.i64_type [| L.i64_type |]))
  in

  let bind_it = mkdef "fibonacci" lambda in
  let _ = eval globals bind_it in

  let expr = mkapp (mkref "fibonacci") [lit_int 40] in

  let result = eval globals expr in
  let () = match result with
      Sllvm (t, v) when t = L.i64_type -> 
	Format.printf "answer: %d\n%!" (GV.as_int v);
	assert (GV.as_int v = 102334155)
    | _ -> assert false
  in
  ()
;;

(*
let () = L.dump_module cur_module ;;
*)


