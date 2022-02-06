(* Factorielle *)
let rec fact n =
	if n < 2 then 1 else n*fact(n-1);;
fact 6;;

(* Opération sur les listes *)
let l1 = [1;2;3];;
let l2 = 1::2::[3];;
let l1l2 = l1 @ l2;;

(* Longueur de la liste *)
let rec longueurListe l = match l with
	| [] -> 0
	| t::q -> 1 + longueurListe q;;

(* longueurListe l1l2;; *)


(* Liste de i jusqu'à j *)
let rec iaj i j = 
	if i > j then []
	else i::(iaj(i+1)j);;

(* iaj 2 9;; *)


(* Concaténation de liste *)
let rec concat a b =
	if [] = b then a
	else match a with
		| [] -> b
		| t::q -> t::(concat q b );;

(* concat l1 l2;; *)

(* Filtrer une liste par un prédicat *)
let rec filtrer pred l = 
	match l with
		| [] -> []
		| t::q -> if pred t
			then t::(filtrer pred q)
			else filtrer pred q;;

 (* filtrer (function n -> 0 == n mod 2)
		( iaj 1 20);; *)

(* Tri rapide *)
let rec qksort cmp l = 
	match l with 
	| [] | [_] -> l
	| t::q -> 
		let small =
			filtrer (function n -> cmp t n > 0) l in
		let big = 
			filtrer (function n -> cmp t n < 0) l in
		let eq = 
			filtrer (function n -> cmp t n = 0) l in
		concat (qksort cmp small)
				(concat eq (qksort cmp big));;
		
(* renvoie n nombres aléatoires *)
let rec lalea n l =
	if 0 = n then l
	else lalea (n-1)
	((Random.int 1000)::l);;

(*qksort (-) (lalea 500[]);;
qksort (fun a b -> b - a)
	(lalea 500 []);;*)


(* Tri naif *)

let rec remfirst elt l = 
	match l with
	| [] -> []
	| t::q -> if t = elt then q
		else t::(remfirst elt q);;

(* remfirst 5 (iaj 1 10);; *)



let rec foldl op ter l = 
	match l with 
		| [] -> ter
		| t::q -> foldl op (op  ter t) q;;

(* foldl (+) 0 (iaj 1 10);; 
foldl max 1 (iaj 1 13);; *)

let rec trinaif cmp l = 
	match l with
		| [] | [] -> l
		| t::q ->
			let pp = foldl (fun a b -> if cmp a b < 0 then a else b) t q in
			pp::(trinaif cmp (remfirst pp l));;
(* trinaif (-) (lalea 30 []);; *)


(* Appliquer une fonction à chaque élément d'une liste*)
let rec map f l =
	match l with
		| [] -> []
		| t::q -> (f t)::(map f q);;
(* map (function n -> n*n) (iaj 1 10);; *)

(* Retourner une liste *)
let rec reverse l = 
	match l with
		| [] -> []
		| t::q -> concat (reverse q) [t];;
(* reverse (iaj 1 10);; *)


let rec rev l pile =
	match l with
		| [] -> pile
		| t::q -> rev q (t::pile);;

let rec iota i j l = 
	if i > j then l 
	else iota i (j-1) (j::l);;

(* rev (iota 1 1000000 []) [];; *)

let concat2 a b =
	rev (rev b (rev a [])) [];;

(* concat2 (iota 1 1000000 []) (iota 1 1000000 []);; *)

(* Si une liste est prefix d'une autre *)

(* 
let rec prefix a b =
	[] = a ||
	(b <> [] &&
		List.hd b = List.hd a
		&&
		prefix (List.tl a) (List.tl b)
	);;
	

*)
let rec prefix a b = 
	match a with
		| [] -> true
		| ta::qa -> (match b with
						| [] -> false
						| tb::qb -> ta=tb && prefix qa qb
					);;
(* prefix (iota 1 1000 []) (iota 1 10000 []);; *)

let rec findsum e l =
	match l with 
	| [] -> if e=0 then [[]] else []
	| t::q -> let sanst = findsum e q in
			let avect = findsum (e-t) q in
			avect@(map (function st -> t::st) sanst);;
(* findsum 10 [1;4;6;9];; *)

let rec subsets e =
	match e with
	| [] -> [[]]
	| t::q -> 
		let partsq = subsets q in
		partsq@(map (function s -> t::s) partsq);;
(* subsets (iaj 1 4);; *)

type 'e abin = Nil | Noeud of ('e abin * 'e *' e abin);;

let ex = Noeud(Noeud(Nil, 2, Nil), 5, Noeud(Nil, 10, Nil));;


(* Insertion dans un arbre *)
let rec insert cmp arb v = 
	match arb with
		| Nil -> Noeud(Nil, v, Nil)
		| Noeud(fg, key, fd) -> 
		if cmp v key < 0 
		then
			Noeud (insert cmp fg v, key, fd)
		else
			Noeud (fg, key, insert cmp fd v);;
			
(* foldl (insert (-)) Nil (lalea 10 []);; *)

let rec larb a = 
	match a with
		| Nil -> []
		| Noeud(fg, k, fd) ->
			concat2 (larb fg) (k::(larb fd));;

let triarbre cmp l = 
	larb (foldl (insert cmp) Nil l );;

(* triarbre (-) (lalea 20000 []);; *)

let rec sorted cmp l =
	match l with
		| [] | [_] -> true
		| a::b::q ->
			cmp a b <= 0
			&& sorted cmp (b::q);;

(* Décomposer un nombre *)

let rec digits base n l =
	if n=0 then l
	else
		digits base (n/base) ((n mod base)::l);;

(* digits 10 123456789 [];; *)

(* liste de nombres -> nombre *)
let rec build base l n =
	match l with
		| [] -> n
		| t::q -> build base q (n*base+t);;

(* build 10 (digits 10 123456789 [] ) 0;; *)


(* Fibo *)

let rec fibo n =
	if n < 2 then n
	else fibo (n-1) + fibo (n-2);;
	
(* map fibo (iaj 1 12);; *)


let rec fi (n, fin_1, fin) k =
	if k < 2 then k
	else if k = n then fin
	else fi (n+1, fin, fin_1+fin) k;;

(* fi (1, 0, 1) 12 *)

module A = Array;;
let matrice nl nc f =
	A.init nl
		(function l ->
			A.init nc (function c -> f l c));;


(* matrice 4 2 (fun l c -> 10*l +c);; *)

let rec somme k1 k2 fk =
	if k1 = k2 then fk k1
	else (fk k1) + somme (k1+1) k2 fk;;

let mfib = [|[|1;1|]; [|1;0|] |];;

let mprod a b = 
	let la = A.length a in
	let ca = A.length a.(0) in
	let lb = A.length b in
	let cb = A.length b.(0) in
	assert(ca=cb);
	matrice la cb (fun l c -> somme 0 (ca-1) (function k -> a.(l).(k)*b.(k).(c)));;


let rec mpuiss a k =
	let l = A.length a in
	if k=0 then matrice l l (fun l c -> if l=c then 1 else 0)
	else if k=1 then a
	else if k mod 2 = 0 then mpuiss (mprod a a) (k/2)
	else mprod a (mpuiss a (k-1));;

map (mpuiss mfib) (iaj 1 12);;

