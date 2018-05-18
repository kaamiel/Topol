(*  Zadanie 5: Sortowanie topologiczne  *)
(*  autor: Kamil Dubil, 370826          *)
(*  reviewer: Szymon Haponiuk           *)

open PMap

(* pomocniczy typ służący do zaznaczania faktu 'wejścia' do        *)
(* wierzchołka (rozpoczęcie przetwarzania, przetwarzanie sąsiadów) *)
(* i 'wyjścia' z wierzchołka (zakończenie przetwarzania)           *)
type opt =
    | Weszlismy
    | Wyszlismy
    | Nie_przetwarzalismy

(* wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)
exception Cykliczne

(* procedura tworząca mapę z listy reprezentującej graf             *)
(* kluczem jest wierzchołek grafu, wartością - lista jego sąsiadów  *)
(* mapa służy do szybszego wyszukiwania sąsiadów danego wierzchołka *)
let mapa_z_listy lista =
    let rec pom l a =
        match l with
        | [] -> a
        | (e, ls)::t ->
            (* na wypadek gdyby wierzchołek wielokrotnie występował *)
            (* w danej liście jako pierwszy element pary            *)
            let lp = try find e a with Not_found -> [] in
            pom t (add e (lp @ ls) a)
    in pom lista empty

(* Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] 
   zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
   dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
   element a_i jest przed kazdym z elementow a_i1 ... a_il *)
let topol lista =
    (* mapa reprezentująca graf dany jako lista *)
    let graf = mapa_z_listy lista in
    (* wyn - konstruowana lista wynikowa, odwiedzone - mapa odwiedzonych wierzcholkow *)
    let rec walk (wyn, odwiedzone) e =
        match try find e odwiedzone with Not_found -> Nie_przetwarzalismy with
        | Weszlismy -> raise Cykliczne
        | Wyszlismy -> (wyn, odwiedzone)
        | Nie_przetwarzalismy ->
            let ls = try find e graf with Not_found -> [] in
            let (wyn, odwiedzone) = List.fold_left walk (wyn, add e Weszlismy odwiedzone) ls in
            (e::wyn, add e Wyszlismy odwiedzone)
    in
    fst (List.fold_left (fun a (e, _) -> walk a e) ([], empty) lista)

(*****************************************************************************)

(* testy *)

(*

open Topol;;
topol [(3, [4]); (5, [6]); (6, [7]); (10, [11]); (11, [12]); (12, [13]); (7, [8]); (9, [13]); (8, [9]); (1, [2]); (13, [6])];; (*Cykliczne*)
topol [(11, [12]); (12, [13]); (7, [8]); (8, [9]); (1, [2]); (13, [6]); (3, [4]); (5, [6]); (6, [7]); (10, [11])];; (* Niecykliczne *)
topol [];;
topol [("ab", ["ba"]); ("ba", ["ab"])];; (* Cykliczne *)
topol [(1, [2]); (3, [4]); (4, [1]); (5, [6]); (6, [3])];; (* Niecykliczne *)
topol [(1, [2]); (3, [4]); (4, [1]); (5, [6]); (6, [3]); (2, [5])];; (* Cykliczne *)
topol [(1, [2]); (3, [4]); (4, [1]); (5, [6]); (6, [3]); (1, [5])];; (* Cykliczne *)
topol [(1, [2]); (3, [4]); (4, [1]); (5, [6]); (6, [3]); (2, [6])];; (* Cykliczne *)
topol [(1, [2]); (3, [4]); (4, [1]); (5, [6]); (6, [3]); (1, [6])];; (* Cykliczne *)
topol [(1, [2; 3; 4]); (3, [7; 8]); (4, [9; 10]); (10, [15; 16]); (2, [5; 6]); (13, [4; 10]); (11, [12]); (12, [13; 14])];; (* Niecykliczne *)
let cyclic = [
    (1, [2]);
    (2, [3]);
    (3, [1])]
in topol cyclic;;

let l1 = [
    (1, [2]);
    (2, []);
    (3, [2])]
in topol l1;;

let l2 = [
    ('a', ['e']);
    ('b', ['a'; 'c']);
    ('c', ['a']);
    ('e', [])]
in topol l2;;
topol [(1, [2; 3; 4]); (3, [7; 8]); (4, [9; 10]); (10, [15; 16]); (2, [5; 6]); (13, [4; 10]); (11, [12]); (12, [13; 14]); (15, [16; 8])];; (* Niecykliczne *)
topol [(1, [2; 3; 4]); (3, [7; 8]); (4, [9; 10]); (10, [15; 16]); (2, [5; 6]); (13, [4; 10]); (11, [12]); (12, [13; 14]); (15, [16; 8]); (8, [14])];; (* Niecykliczne *)
topol [(1, [2; 3; 4]); (3, [7; 8]); (4, [9; 10]); (10, [15; 16]); (2, [5; 6]); (13, [4; 10]); (11, [12]); (12, [13; 14]); (15, [16; 8]); (8, [12])];; (* Cykliczne *)

*)
