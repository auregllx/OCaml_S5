

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

  
























(* let () = *)
  

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




