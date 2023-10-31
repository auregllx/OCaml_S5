

open List
open String
open Char

type base = A|C|G|T|WC

type dna = base list

(* 3.0 *)

(* Explode : string -> char list *)
let explode (chaine: string) : char list =
  let rec aux index acc = 
    if index < length chaine then     
      aux (index + 1) (chaine.[index] :: acc)
    else 
      rev acc
  in 
  aux 0 []
;;


(* 3.1 *)

(* base_of_char : char -> base *)
let base_of_char (caractere : char) : base = 
  let c = Char.uppercase_ascii caractere in 
  match c with 
  |'A' -> A
  |'C' -> C
  |'G' -> G
  |'T' -> T
  | _ -> WC
;;



(* 3.2 *)

(* char_of_base : base -> char *)
let char_of_base (b : base) : char = 
  match b with 
  |A -> 'A'
  |C -> 'C'
  |G -> 'G'
  |T -> 'T'
  |WC -> '.'
;;


(*dna_of_string : string -> dna *)
let dna_of_string (chaine : string) : dna = 
  let rec aux index acc = 
    if index < String.length chaine then 
      aux (index + 1) ((base_of_char chaine.[index]) :: acc)
    else 
      List.rev acc
  in
  aux 0 []
;; 


(* 3.3 *)

(* strinf_of_dna : dna -> string *)
let string_of_dna (list : dna) : string = 
  init (List.length list) (fun i -> char_of_base(nth list i))
;;



(* 3.4 *)

(* cut_prefix : 'a list -> 'a list -> 'a list option *)
let rec cut_prefix pre l = 
  match (pre, l) with 
  |([],suf) -> Some suf
  |(x :: pre', y :: l') when x = y -> cut_prefix pre' l'
  |_ -> None
;;


(* 3.5 *)

(* first_occ : 'a list -> 'a list -> ('a list * 'a list) option *)
let first_occ slice l =
  let rec aux ls l = 
    match l, ls with
    |t, [] -> true, t
    |[], _ -> false, []
    |h :: t, hs :: ts -> if h = hs then aux ts t 
        else false, []
  in
  let rec aux_bis slice l af =
    match slice, l with
    |[], _ | _, [] -> None
    |hs :: ts, h :: t -> 
        if h = hs then
          (
            let b, tail = aux ts t in 
            if b then
              Some (af, tail)
            else aux_bis slice t (af @ [h])
          )
        else aux_bis slice t (af @ [h])
  in
  aux_bis slice l []
;;



(* 3.6 *)

(* slices_between : 'a list -> 'a list -> 'a list -> 'a list list *)















let () =
  

  (* TESTS 3.0 à 3.3 *)
  (*
  let tab1 = [A; C; G; T; T; A; C; WC; C; WC] in   
  (* let str1 = "GTAA..CT" in *)  

  print_string "string_of_dna :";
  print_newline ();
  print_string (string_of_dna tab1);
  print_newline ();
  print_string "dna_of_string :";
  print_newline();
  *)

  (* TESTS 3.4 
    -> verifié dans le compilateur *)


  let l1 = ['A';'A';'A';'G';'T';'C'] in
  let l2 = ['A';'A';'A';'G';'T';'C'] in 
  let l3 = ['A';'A';'A';'G';'T';'C'] in 
  let slice1 = ['A';'G'] in 
  let slice2 = ['A';'A'] in 
  let slice3 = ['A';'T'] in

  let result1 = first_occ slice1 l1 in 
  let result2 = first_occ slice2 l2 in 
  let result3 = first_occ slice3 l3 in 

  let print_result = function
    | Some (before, after) ->
      Printf.printf "Before: [%s], After: [%s]\n"
        (String.concat "; " (List.map (String.make 1) before))
        (String.concat "; " (List.map (String.make 1) after))
    | None -> print_string "None\n"
  
  in 
  print_result result1; 
  print_result result2;
  print_result result3;