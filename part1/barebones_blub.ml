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
  | Sbool of bool
  | Sint of int

and sclosure = {
  close_env : environment;
  close_lam : lambda;
}

and environment_frame = {
  env_frame_vars : variable array;  (* mostly for debugging *)
  env_frame_vals : sval array
}

and environment = environment_frame list

let rec eval env = function
    Ast_lit lit -> lit
  | Ast_ref (_, frame, offset) -> (List.nth env frame).env_frame_vals.(offset)
  | Ast_cnd (pred, cons, alt) -> 
      if eval env pred = Sbool false then eval env alt else eval env cons
  | Ast_app (fn, args) -> apply (eval env fn) (Array.map (eval env) args)
  | Ast_abs (params, body) ->
      Sclosure { close_env = env; 
		 close_lam = { lam_ast = body;
			       lam_params = params } }
and apply fn args =
  match fn with
      Sclosure close ->
	let frame = { env_frame_vars = close.close_lam.lam_params;
		      env_frame_vals = args } in
	let env' = frame :: close.close_env in
	eval env' close.close_lam.lam_ast
;;

let make_var name = Variable name ;;
let make_lit sval = Ast_lit sval ;;
let make_ref var frame offset = Ast_ref (var, frame, offset) ;;
let make_cnd pred cons alt = Ast_cnd (pred, cons, alt) ;;
let make_abs params body = Ast_abs (Array.of_list params, body) ;;
let make_app fn args = Ast_app (fn, Array.of_list args) ;;

let () =
  let a = make_var "a" in
  let x = make_var "x" in
  (* if a is true then x else 0 -- (if a x 0) *)
  let cnd = make_cnd (make_ref a 0 0) (make_ref x 0 1) (make_lit (Sint 0)) in
  (* (lambda (a x) (if a x 0)) *)
  let lambda = make_abs [a; x] cnd in
  (* ((lambda (a x) (if a x 0)) #t 4) *)
  let expr = make_app lambda [make_lit (Sbool true); make_lit (Sint 4)] in
  assert (eval [] expr = Sint 4)
;;
