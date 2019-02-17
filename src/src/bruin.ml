open Grammar;;

let (>>) x f = f x;;

type bruin =
    | FreeVar     of string
    | NotFreeVar  of int
    | Abstract    of bruin
    | Aplic       of bruin * bruin
;;



let rec string_of_normalized_expr bruin level = match bruin with
    | FreeVar     (s)   ->  "(" ^ s ^ ")"
    | NotFreeVar  (l)   ->  "(" ^ "varaible" ^ ((string_of_int (level - l - 1))) ^ ")"
    | Abstract    (e)   -> "(\\" ^ "varaible" ^ (string_of_int level) ^ "." ^ "(" ^ (string_of_normalized_expr e (level + 1)) ^ ")" ^ ")"
    | Aplic       (f, s)-> "(" ^ (string_of_normalized_expr f level) ^ " " ^ (string_of_normalized_expr s level) ^ ")"
;;

let string_of expr = string_of_normalized_expr expr 0;;



let rec numirate var not_free_vars level = match not_free_vars with
    | head::tail  ->  if (head = var) then level else numirate var tail (level + 1)
    | _           -> -1337
;;

let rec renumirate_not_free_var expr lvl curlvl = match expr with
    | FreeVar     (s)   ->  expr
    | NotFreeVar  (l)   ->  if (l >= curlvl) then NotFreeVar (l + lvl) else expr
    | Abstract    (e)   ->  Abstract (renumirate_not_free_var e lvl (curlvl + 1))
    | Aplic       (f, s)->  Aplic ((renumirate_not_free_var f lvl curlvl), (renumirate_not_free_var s lvl curlvl))
;;



let rec to_normalize_expr_impl expr not_free_vars = match expr with
    | Var     (v)     ->  if (List.mem v not_free_vars) then NotFreeVar(numirate v not_free_vars 0) else FreeVar(v)
    | Appl    (f, s)  ->  Aplic((to_normalize_expr_impl f not_free_vars), (to_normalize_expr_impl s not_free_vars))
    | Abst    (f, s)  ->  Abstract((to_normalize_expr_impl s (f::not_free_vars)))

let to_normalize_expr expr = to_normalize_expr_impl expr [];;
