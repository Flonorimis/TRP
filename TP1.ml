(* Florine SIMON G5 TP1 *)

let rec profondeurBeta liste = 
match liste with 
[] -> failwith "pas de but"
| x::reste -> print_list_string reste; if (estBut1 x) then x else (profondeur ((etatsSuivants1 x) @ reste));;


let rec print_list_string liste =
  match liste with
  [] -> print_string "."
  | x::reste -> print_string x; print_string " "; print_list_string reste;;


let rec etatsSuivantsNonVus x vus = 
match etatsSuivants1 x with
[] -> []
| x::reste -> if (List.mem x vus) then (etatsSuivantsNonVus reste vus) else (etatsSuivantsNonVus x x::vus );;


let rec profondeurBetaVus liste vus =
 match liste with 
 [] -> failwith "pas de but"
 | x::reste -> print_list_string reste;
if (estBut1 x) then x  (* Si x est un but, on le retourne *)
else (profondeurBetaVus ((etatsSuivantsNonVus x vus) @ reste) (x::vus));;
