(*definition d'automate*)
type automate = {
	etat_initial : int ;
	ensemble_des_etats : int list;
	alphabets : char list;
	transitions : (int*char*int) list;
	etats_finaux : int list;
};;
(*ce fonction permette de saisir une list des etat de nom ne en le donnent le nombre des etats *)
let ne =let ()= print_string "les etats de l'automate \n" in
let ()= print_string "nombre des etats: " in let n=read_int () in
let rec r a = match a with
0 -> []
|_ -> let () = print_string "donnez un etat: " in let t=read_int () in t::r (a-1) in
r n;;
(*ce fonction permette de saisir l'etat initial avec le nom ei*)
let ei = let ()= print_string "l'etat_initial de l'automate\n" in
let () = print_string "donnez l'etat initial: " in let n=read_int ()
in n;;
(*ce fonction permette de saisir une list d'etat finaux de nom ef en le donnent le nombre d'etat finaux  *)
let ef = let ()= print_string "les etat_finaux de l'automate\n" in
let ()= print_string "nombre d'etats_finaux: " in let n=read_int () in
let rec r a = match a with
0 -> []
|_ -> let () = print_string "donnez un etat_final: " in let t=read_int () in t::r (a-1) in
r n;;
(*ce fonction permette de saisir une list des alphabets de nom al en le donnent leurs nombre*)
let al = let ()= print_string "les alphabets de l'automate \n" in
let ()= print_string "nombre des alphabets: " in let n=read_int () in
let rec r a = match a with
0 -> []
|_ -> let () = print_string ("donnez un alphabets: ") in let t=read_line() in let k= String.get t 0 in k::r (a-1) in
r n;;
(*ce fonction permette de saisir une list de transitions de type triple de nom tr en le donnent leurs nombre *)
 let tr =
 let () = print_string "les transitions de l'automate \n" in
 let () = print_string "nombre des transitions: " in
 let n=read_int () in
let rec g a=
match a with
0 -> []
|_ -> let ()= print_string "\ntransition: \n"
in let ()=print_string "Donnez l’Etat de départ: " 
in let e= read_int()
in
let ()= print_string "Donnez l’alphabet: "
in
let w=read_line() in let k= String.get w 0
in 
let ()= print_string "Donnez l’Etat drivée: "
in
let d=read_int () in 
(e,k,d)::(g (a-1));
in 
g n;;
(*on affecte chaque valeur par son element correspondant*)
let a1={
	etat_initial =ei;
	ensemble_des_etats =ne ;
	alphabets =al;
	transitions =tr;
	etats_finaux =ef;
};;
(*le fonction valide qui verifie les different element de l'automate*)
let valide auto=
let rec separer ts = 
match ts with
[] -> ([],[],[])
| (x,y,z)::r -> 
let (xs, ys, zs) = (separer r) in (x::xs, y::ys, z::zs) ;in
let (dom, sym, img) = separer auto.transitions ;in
let rec member_element x l = match l with
[] -> false 
|(t::r) -> if x=t then true else member_element x r ;in
let rec membre_liste l l' = 
match (l, l') with
([], _) -> true
| (x::xs, _) -> 
(member_element x l') && (membre_liste xs l') ;in
 if (membre_liste dom auto.ensemble_des_etats =true && membre_liste sym auto.alphabets = true &&
membre_liste img auto.ensemble_des_etats = true && membre_liste auto.etats_finaux auto.ensemble_des_etats =true && member_element auto.etat_initial auto.ensemble_des_etats=true) then true else false;;
let affichage = if valide a1=true then print_string"valide " else print_string"non-valide ";;

(*le fonction complet qui verifie que chaque etat et alphabet sont utiliseé dans le transitions*)
let complet auto =
let rec img e a t=
  match t with
      [] -> []
    |x::r -> let (i,j,k) = x  in
          if (i=e && j=a) then k::(img e a r) else img e a r; in
let rec nodef e w t =
  match w with
      [] -> []
    |a::r -> if (img e a t) = [] then (e,a)::(nodef e r t) else nodef e r t;in
let rec complet_cond e w t =
  match e with 
      [] -> []
    |e1::en -> (nodef e1 w t)@(complet_cond en w t); in
if ((valide auto=true)&&((complet_cond auto.ensemble_des_etats auto.alphabets auto.transitions)=[])) then true else false;;



let affichage = if complet a1=true then print_string"complet " else print_string"non-complet ";;


(*le fonction deterministe qui verifie que l'image d'une etat et un alphabet est unique*)
let deterministe auto=
let h a =
let rec membre a l = match l with
[]-> false
|x::r -> a=x||membre a r; in
let rec g a = match a with
[] -> false
|x::r ->(membre x r)||(g r); in
let rec f a = match a with
[] -> []
|(x,y,z)::r -> (x,y)::(f r); in
let l = (f a) in (g l) ;in 
let a=auto.transitions in
let rec s l=
let rec m (x,y,z) l=
match l with 
[]->false
|(a,b,c)::r -> ((x,y,z)=(a,b,c))||(m (x,y,z) r); in
match l with
[] -> []
|(x,y,z)::r -> if (m (x,y,z) r) then (s r) else (x,y,z)::s r; in
let v =s a in 
if h v then false else true;;
let affichage = if ((valide (a1))&&(deterministe(a1)))=true then print_string"deterministe" else print_string"non-deterministe";;
;;
print_newline()
