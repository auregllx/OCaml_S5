open List
open String
open Char

type 'a expr = 
  | Eps
  | Base of 'a
  | Joker
  | Concat of 'a expr * 'a expr
  | Alt of 'a expr * 'a expr
  | Star of 'a expr


exception Invalid_Argument


(* 4.1 *)

(* repeat : int -> 'a list -> 'a list *)
let repeat n l = 
  if n <= 0 then raise Invalid_Argument
  else 
    let rec aux acc n list = 
      match list with 
      |[] -> []
      |_ ->
        if n = 1 then acc 
        else aux (acc @ l) (n-1) list
    in aux l n l 
;;


(* 4.2 *)

(* expr_repeat : int -> 'a expr -> 'a expr *)
let expr_repeat n expr = 
  if n<= 0 then raise Invalid_Argument
  else 
    let rec aux acc n expr = 
      match expr with 
      |Eps -> Eps
      |_ -> 
        if n = 1 then acc
        else aux (Concat (expr,acc)) (n-1) expr
    in aux expr n expr
;;   


(* 4.3 *)

(* is_empty : 'a expr -> bool *)
let rec is_empty expr =
  match expr with 
  |Eps -> true 
  |Base _ -> false
  |Joker -> false
  |Concat(e1,e2) -> is_empty e1 && is_empty e2
  |Alt(e1,e2) -> is_empty e1 || is_empty e2
  |Star _ -> true
;;


(* 4.4 *)

(* null : 'a expr -> bool *)
let rec null expr = 
  match expr with 
  |Eps -> true
  |Base _ -> false
  |Joker -> true
  |Concat(e1,e2) -> null e1 && null e2
  |Alt(e1,e2) -> null e1 || null e2
  |Star _ -> true
;;



(* 4.5 *)

(* is_finite : char expr -> bool *)
let rec is_finite expr = 
  let rec concat_finite e1 e2 = 
    match (is_finite e1, is_finite e2) with 
    |(true,true) -> true
    |_ -> false
  in match expr with 
    |Eps -> true
    |Base _ -> true
    |Joker -> true
    |Concat(e1,e2) -> concat_finite e1 e2
    |Alt (e1,e2) -> is_finite e1 && is_finite e2
    |Star _ -> false
;;



(* 4.6 *)

(* product : 'a list list -> 'a list list -> 'a list list *)
let rec product l1 l2 = 
  let produit_cartesien mot l = 
    List.map (fun e -> mot @ e) l
  in 
  let rec aux l1 l2 acc = 
    match l1 with 
    |[] -> acc
    |mot :: tl -> 
      let res = produit_cartesien mot l2 in 
      aux tl l2 (acc @ res)
  in
  aux l1 l2 []
;;



(* 4.7 *)

(* enumerate : char list -> char expr -> char list list option *)







(* 4.8 *)

(* alphabet_expr : 'a expr -> 'a list *)
let rec alphabet_expr expr = 
  let rec supprime_duplications list = 
    List.sort_uniq (fun a b -> List.compare a b) list
  in
  match expr with 
  |Eps -> [] 
  |Base a -> [a]
  |Joker -> []
  |Concat(e1,e2) -> supprime_duplications (alphabet_expr e1 @ alphabet_expr e2)
  |Alt(e1,e2) -> supprime_duplications (alphabet_expr e1 @ alphabet_expr e2)
  |Star s -> alphabet_expr s
;;
