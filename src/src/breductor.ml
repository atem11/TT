open Bruin;;

let (>>) x f = f x;;

let rec subst_impl where what lvl = match where with
    | FreeVar     (v)   ->  where
    | NotFreeVar  (l)   ->  (
        match (compare lvl l) with
        | -1  ->  NotFreeVar (l - 1)
        | 0   ->  renumirate_not_free_var what lvl 0
        | 1   ->  where
    )
    | Abstract    (e)   ->  Abstract((subst_impl e what (lvl + 1)))
    | Aplic       (f, s)->  Aplic((subst_impl f what lvl), (subst_impl s what lvl))
;;

let subst where what = match where with
    | Abstract  (e)   ->  subst_impl e what 0
;;

let rec reduction_impl expr = match expr with
    | Abstract    (e)   ->  Abstract(reduction_impl e)
    | Aplic       (f, s)->  (
          match f with
            | Abstract  (e) ->  subst f s
            | _             ->  begin
                let norm_reduction = reduction_impl f in 
                  if (norm_reduction = f)
                    then  Aplic(f, (reduction_impl s))
                    else  Aplic(norm_reduction, s)
            end
    )
    | _  -> expr
;;

let rec reduction_loop expr = begin
    (*expr >> string_of >> print_endline;
    print_endline " ";*)
    let reduction_res = reduction_impl expr in 
        if (reduction_res = expr)
          then  reduction_res
          else reduction_loop reduction_res
end;;

let reduction expr = expr >> to_normalize_expr >> reduction_loop;;
