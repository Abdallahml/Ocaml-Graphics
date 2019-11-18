(*vouz povez simplemen copies tout ce code et le paster dans cygwin aprai avoir ovrie la ocaml*)



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
open Graphics;;
Graphics.open_graph "";;

(*le fonction draw_auto prend un automate et dessine cet automate *)
let draw_auto auto=
(*le fonction cet prendre une list des ints et renvoi une lise de ces ints avec leurs coordonnees cree (a,(xa,ya))*)
let cet auto =
let rec coor l = match l with
[] -> []
|x::r -> let n=List.length l in if (n mod 2=0)then let a=((n*100)+100) and  b=200 in let c=(x,(a,b)) in c::(coor r)
                                            else let a=((n*100)+100) and  b=100 in let c=(x,(a,b)) in c::(coor r);in
coor auto.ensemble_des_etats;

in
(*le fonction ctr prendre une automate et avec l'aide de list de coordonnees des etats elle retourne les list de transitions avec le coordonnees chaque etat*)
let ctr auto=
let rec bring x l = match l with
[] -> (0,(0,0))
|(c,(a,b))::r -> if c=x then (c,(a,b)) else bring x r; in
let rec cooetrans coreta transitions=match transitions with
[] -> []
|(x,y,z)::r -> let a=bring x coreta and b=bring z coreta in (a,y,b)::cooetrans coreta r ;in
cooetrans (cet auto) auto.transitions;

in
(*le foction etat prend l'etat coordonneer et dessin une cercle avec son coordonnees et dessin ce char dedans *)
let etat (c,(x,y))=draw_circle x y 10 ;let a=(x-5) and b=(y-5) in moveto a b ;let s=string_of_int c in draw_string s;in
(*le foction etati prend l'etat coordonneer et dessin double cercle avec son coordonnees et dessin ce char dedans *)
let etati (c,(x,y))=draw_circle x y 15 ;draw_circle x y 10 ;let a=(x-5) and b=(y-5) in moveto a b ;let s=string_of_int c in draw_string s;
in
(*le fonction flech qui prend un etat coordonneer et dessin un flech diriger vers le bas en tete de ce coordonnees*)
let flech (c,(x,y))=let a= (x-8) and b=(y+30) in moveto a b;
let d=x and h=(y+15) in lineto d h;
let r=(x+2) and z=(y+15) in lineto r z;
let s=(x+8) and w=(y+30) in lineto s w;let a=(x-8) and b=(y+30) in lineto a b;
in
(*ce fonction prandre une transitions  et dessin arce en tete de ce premieur coordonnees ce pour le transition de mm etat de depart et arrive  *)
let bon ((i,(xi,yi)),p,(j,(xj,yj)))=let xc=xi and yc=yi+44 in moveto xc yc; draw_char p;
let xa=xi and ya=yi+20 in draw_arc xa ya 17 25 320 220;
let xv=xi-20 and yv=yi+5 in moveto xv yv ;draw_char 'V';
in
(*mon fonction arrow prendre un transition coordonneer et dessin une flech de la premieur etat coordonneer et diriger vers la deuxieme etat coordonneer et dessine l'alphabet enter ce deux*)
let arrow ((i,(xi,yi)),p,(j,(xj,yj))) =
if ((xi<xj)&&(yi=yj)) then
        (let x1=xi+15 and y1=yi+5 in moveto x1 y1;
        let x2=xj-15 and y2=yj+5 in lineto x2 y2;
        let a=(x2-8) and b=(y2-8) in lineto a b;
        let c=(y2+8) in moveto a c;lineto x2 y2;
        let i=((x1+x2)/2) and j=((y1+y2+5)/2) in
        moveto i j ;draw_char p;)
else if ((xi>xj)&&(yi=yj)) then(let x1=xi-15 and y1=yi-5 in moveto x1 y1;
       let x2=xj+15 and y2=yj-5 in lineto x2 y2;
                let a=(x2+8) and b=(y2+8) in lineto a b;
                let c=(y2-8) in moveto a c;lineto x2 y2;
                let i=((x1+x2)/2) and j=((y1+y2-22)/2) in
				 moveto i j ;draw_char p;)
			 else if ((xi=xj)&&(yi<yj)) 
			 then(let x1=xi-5 and y1=(yi+15) in moveto x1 y1;
			let x2=xj-5 and y2=(yj-15) in lineto x2 y2;
			let a=(x2+8) and b=(y2-8) in lineto a b;
			let c=(x2-8) and d=(y2-8) in moveto c d;
			lineto x2 y2;
			let i=((x1+x2-11)/2) and j=((y1+y2)/2) in
			moveto i j ;draw_char p;)
                else if ((xi=xj)&&(yi>yj)) then(let x1=xi+5 and y1=(yi-15) in moveto x1 y1;
                let x2=xj+5 and y2=(yj+15) in lineto x2 y2;
                let a=(x2+8) and b=(y2+8) in lineto a b;
                let c=(x2-8) and d=(y2+8) in moveto c d;
                lineto x2 y2;
                let i=((x1+x2+11)/2) and j=((y1+y2)/2) in
                moveto i j ;draw_char p;)
				else if ((xi<xj)&&(yi<yj)) then (let x1=xi+5 and y1=(yi+15) in moveto x1 y1;
                        let x2=xj-15 and y2=(yj-5) in lineto x2 y2;
                        let a=(y2-8) in lineto x2 a;
                        let c=(x2-8) in moveto c y2;
                        lineto x2 y2;
                        let i=((x1+x2-5)/2) and j=((y1+y2+5)/2) in
                        moveto i j ;draw_char p;)
                                else if ((xi>xj)&&(yi>yj)) then (let x1=xi-5 and y1=(yi-15) in moveto x1 y1;
                                let x2=xj+15 and y2=(yj+5) in lineto x2 y2;
                                let a=(x2+8) in lineto a y2;
                                let c=(y2+8) in moveto x2 c;
                                lineto x2 y2;
                                let i=((x1+x2+5)/2) and j=((y1+y2-5)/2) in
                                moveto i j ;draw_char p;)
								else if ((xi<xj)&&(yi>yj)) then (let x1=xi+15 and y1=(yi-5) in moveto x1 y1;
                                        let x2=xj-5 and y2=(yj+15) in lineto x2 y2;
                                        let a=(x2-8) in lineto a y2;
                                        let c=(y2+8) in moveto x2 c;
                                        lineto x2 y2;
                                        let i=((x1+x2+5)/2) and j=((y1+y2+5)/2) in
                                        moveto i j ;draw_char p;)
                                                else (let x1=xi-15 and y1=(yi+5) in moveto x1 y1;
                                                let x2=xj+5 and y2=(yj-15) in lineto x2 y2;
                                                let a=(x2+8) in lineto a y2;
                                                let c=(y2-8) in moveto x2 c;
                                                lineto x2 y2;
                                                let i=((x1+x2-5)/2) and j=((y1+y2-5)/2) in
                                                moveto i j ;draw_char p;);in
(*ce fonction verifie l'appartenance de une etat coordonneer a une list en verifient par sont premiere element*)
let fin l (c,(a,b))=let rec membre a l =match l with
| [] -> false
| x::rl -> x=a || membre a rl;in if membre c l then true else false;
in
(*la fonction fik verefie que un etat coordonneer et egal a une autre etat par verifiant son premier nombre*)
let fik i (c,(a,b))=if i=c then true else false;
in
(*mon foction draw_atra qui prendre un auto et applique les fonctions pressedentes un verifiant des condition*)
let draw_atra auto =
let rec drawtr l k h= match l with
[] ->moveto 100 100;
|(x,y,z)::r -> if fin k x then etati x else etat x ;if fik h x then flech x else etat x;
 if x=z then bon (x,y,z) else arrow (x,y,z);if fin k z then etati z else etat z ;if fik h x then flech x else etat x;drawtr r k h;
 in
drawtr (ctr auto) auto.etats_finaux auto.etat_initial;in
draw_atra auto ;;
draw_auto a1;;

read_line();;
(*si vous copier tout ce code appier sur entrai deux foit*)