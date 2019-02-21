open Grammar;;
open Buffer;;
open List;;

module Ht = Hashtbl;;

let (>>) x f = f x;;

let buf = create 10_000_000;;

try
  while true do
    let line = stdin >> input_line in
      add_string buf line;
      add_char buf ' ';
  done;
with End_of_file ->
  ()

let expr = buf >> contents >> Lexing.from_string >> Parser.main Lexer.main;;


type res_type =
    | Atom          of int
    | Implication   of res_type * res_type
;;

type type_fun = {left : res_type; right : res_type};;

let rec type_to_string res_type = match res_type with
    | Atom          (ind) ->  "t" ^ (string_of_int ind)
    | Implication   (f, s)->  "(" ^ type_to_string f ^ " -> " ^ type_to_string s ^ ")"
;;

let type_fun_to_string type_fun = type_to_string type_fun.left ^ " = " ^ type_to_string type_fun.right;;


let rec make_system expr cnt map_f map_nf = match expr with
    | Var     (n)   ->  begin
        let num = if (Ht.mem map_nf n) then Ht.find map_nf n
            else begin
              if (Ht.mem map_f n) then Ht.find map_f n
              else begin
                Ht.add map_f n (cnt + 1);
                (cnt + 1)
              end
            end in
        ([], Atom(num), cnt + 1)
      end
    | Abst    (n, e)->  begin
        Ht.add map_nf n (cnt + 1);
        let (sys, t, cnt_n) = make_system e (cnt + 1) map_f map_nf in begin
          Ht.remove map_nf n;
          (sys, Implication(Atom(cnt + 1), t), cnt_n)
        end
      end
    | Appl    (f, s)->  begin
        let (f_s, f_t, f_cnt) = make_system f cnt map_f map_nf in
        let (s_s, s_t, s_cnt) = make_system s f_cnt map_f map_nf in
        let n_s = {left = f_t; right = Implication(s_t, Atom(s_cnt + 1))}::(List.rev_append f_s s_s) in
        (n_s, Atom(s_cnt + 1), s_cnt + 1)
    end
;;


let map_f   = (Ht.create 255 : (string, int) Ht.t);;
let map_nf  = (Ht.create 255 : (string, int) Ht.t);;

let (system, t_e, cnt) = make_system expr 0 map_f map_nf;;

(*
t_e >> type_to_string >> print_endline;;
*)

let rec print_system system = match system with
    |head::tail ->  begin
          type_fun_to_string head >> print_endline;
          print_system tail
        end
    | _         ->  print_endline "==================="
;;


(*print_system system;;*)


let a_cond type_fun = match type_fun with
    | {left = Implication(f, s); right = Atom(i)} ->  {left = type_fun.right; right = type_fun.left}
    | _                                           ->  type_fun
;;

let b_cond type_fun = match type_fun with
    | {left = Atom(l); right = Atom(r)} -> if (l = r) then false else true
    | _                                 ->  true
;;

let c_cond type_fun = match type_fun with
    | {left = Implication(l_f, l_s); right = Implication(r_f, r_s)}
        ->  [{left = l_f; right = r_f}; {left = l_s; right = r_s}]
    | _ ->  [type_fun]
;;

let rec d_cond type_fun func = match type_fun with
    | {left = x ; right = Implication(f, s)}  -> {left = x; right = Implication((d_cond {left = x; right = f} func).right, (d_cond {left = x; right = s} func).right)}
    | {left = x ; right = Atom(t)}            -> if (type_fun.right = func.left) then {left = x; right = func.right} else type_fun
;;

let rec subst_in_list list_exp func = match list_exp with
    | head::tail  ->  (d_cond head func)::(subst_in_list tail func)
    | _           ->  []
;;

let rec subst_all_list list_choose list_exp = match list_choose with
    | head::tail  ->
        let new_list = subst_in_list list_exp head in
          subst_all_list tail new_list
    | _           -> list_exp
;;

let rec check_atom atom impl = match impl with
    | Atom        (a)     ->  atom = impl
    | Implication (f, s)  ->  (check_atom atom f) || (check_atom atom s)
;;

let check_fun type_fun = match type_fun with
    | {left = Atom(i); right = Implication(f, s)} -> check_atom type_fun.left type_fun.right
    | _                                           ->    false
;;

let rec solve_system system = if (List.exists check_fun system) then [{left = Atom(-1); right = Atom(0)}] else begin
    (*print_system system;*)
    let prev_system     = system in
    let rev_system      = List.rev_map a_cond system in
    let red_system      = List.rev_map c_cond rev_system in
    let new_rev_system  = List.rev_map a_cond (List.flatten red_system) in
    let clean_system    = List.filter b_cond new_rev_system in
    let subst_system    = subst_all_list clean_system clean_system in
    let norm_system     = List.rev subst_system in
    if (prev_system = norm_system) then prev_system else (solve_system norm_system)
end;;

let final_system = solve_system system;;

(*print_system final_system;;*)

(**)

let rec apply_atom at unif = match unif with
  | head::tail  ->  if (at = head.left) then head.right else apply_atom at tail
  | _           ->  at
;;

let rec apply_subst typ unif = match typ with
  | Atom    (i)       ->  apply_atom typ unif
  | Implication(f, s) ->  Implication((apply_subst f unif), (apply_subst s unif))
;;

let rec free_types map_f unif = begin
  let pairs = Ht.fold (fun k v acc -> (k, v)::acc) map_f [] in
  let ind_to_type_s ind = type_to_string(apply_subst(Atom(ind)) unif) in
  let pair_to_elem (k, v) = String.concat " " [k; ":"; (ind_to_type_s v)] in
  String.concat ", " (List.map pair_to_elem pairs)
end;;

let free_typ = free_types map_f final_system;;

let rec type_print_system expr cnt map_f map_nf unif tabs free_str = match expr with
    | Var     (n)   ->  begin
        let num = if (Ht.mem map_nf n) then Ht.find map_nf n
            else begin
              if (Ht.mem map_f n) then Ht.find map_f n
              else begin
                Ht.add map_f n (cnt + 1);
                (cnt + 1)
              end
            end in begin
            (*print_endline (type_to_string (apply_subst (Atom(num)) unif));*)
            let t = apply_subst (Atom(num)) unif in
            (t, cnt + 1, String.concat " " [tabs; free_str; "|-"; n; ":"; type_to_string t; "[rule #1]\n"])
        end
      end
    | Abst    (n, e)->  begin
        Ht.add map_nf n (cnt + 1);
        let new_t = apply_subst (Atom(cnt + 1)) unif in
        let new_G = if (free_str = "") then String.concat " " [n; ":"; type_to_string new_t] else String.concat " " [free_str; ","; n; ":"; type_to_string new_t] in
        let (t, cnt_n, res_str) = type_print_system e (cnt + 1) map_f map_nf unif (tabs ^ "*   ") new_G in begin
          Ht.remove map_nf n;
          let tt = apply_subst (Implication(new_t, t)) unif in begin
            (tt, cnt_n, String.concat " " [tabs; free_str; "|-"; string_of_expr expr; ":"; type_to_string tt; "[rule #3]\n"; res_str])
          end
        end
      end
    | Appl    (f, s)->  begin
        let (f_t, f_cnt, f_out) = type_print_system f cnt map_f map_nf unif (tabs ^ "*   ") free_str in
        let (s_t, s_cnt, s_out) = type_print_system s f_cnt map_f map_nf unif (tabs ^ "*   ") free_str in
        let f_t_new = apply_subst f_t unif in
        let s_t_new = apply_subst s_t unif in
        match f_t_new with
          | Implication(f_, s_) -> begin
          (*print_endline f_out;*)
          (*print_endline s_out;*)
            let s_tt = apply_subst s_ unif in
            (s_tt, (s_cnt + 1),
              String.concat " " [tabs; free_str; "|-"; string_of_expr expr; ":"; type_to_string s_tt; "[rule #2]\n"; f_out; s_out])
          end
    end
;;

match (final_system) with
  | [{left = Atom(-1); right = Atom(0)}]  ->  "Expression has no type" >> print_endline
  | _   ->  begin
      let (f_t, cnt, final_str) = type_print_system expr 0 map_f map_nf final_system "" free_typ in
      final_str >> print_endline
    end
;;


