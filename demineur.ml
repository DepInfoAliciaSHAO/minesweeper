#load "graphics.cma";;
open Graphics;;

(* Contrôles : z,q,s,d pour se déplacer sur la grille, f pour poser/enelever un drapeau, e pour dévoiler une case
NB: ne pas changer la taille de la fenêtre au cours du jeu sinon ça casse tout *)

(* ---------------------- Implémentation de la grille -------------------------- *) 

(* chaque case de la grille prend deux valeurs : 
	1 -- 0 (decouvert), 1 (couvert) ou 2 (drapeau)
	2 -- 0 à 8 (nombres de bombes adjacentes) ou 9 (bombe sur la case) *)

(* ----- Nombre de bombes adjacentes à une case ---- *)

(* Renvoie la liste des coordonnées des cases adjacentes d'une case *)
let adjacents coord i j = match coord with
	|(n,m) when n = (i-1) && m = (j-1)-> [(n-1,m); (n-1,m-1); (n, m-1)]
	|(0,0) -> [(0,1); (1,1); (1,0)]
	|(0,n) when n = (j-1) -> [(0,n-1); (1,n-1); (1,n)]
	|(n,0) when n = (i-1) -> [(n-1,0); (n-1,1); (n,1)]
	|(0,n) -> [(0,n-1); (0,n+1); (1,n-1); (1,n); (1,n+1)]
	|(n,0) -> [(n-1,0); (n-1,1); (n,1); (n+1,1); (n+1,0)]
	|(n,m) when m = (j-1) -> [(n-1,m); (n-1,m-1); (n,m-1); (n+1,m-1); (n+1, m)]
	|(n,m) when n = (i-1)-> [(n,m-1); (n-1,m-1); (n-1,m); (n-1,m+1); (n,m+1)]
	|(m,n) -> [(m-1,n-1); (m-1,n); (m-1,n+1); (m,n+1); (m+1,n+1); (m+1,n); (m+1,n-1); (m,n-1)];;

(* A une liste de coordoonées de cases, ajoute 1 au nombre de bombes des cases adjacentes *)
let rec increment grid l = match l with
	|[] -> grid
	|(x,y)::q when snd(grid.(x).(y))<9-> 
		let (a,b) = grid.(x).(y) in grid.(x).(y) <- (a,b+1);
		increment grid q
	|_::q -> increment grid q;;

(* Crée des mines aléatoirement sur une grille et incrémente de un le nombre de bombes aux cases adjacentes à la bombes placée *)
let rec mine l c grid x y = match grid.(x).(y) with
	|(_,9) -> mine l c grid (Random.int l) (Random.int c)
	|(_,_) -> let (a,b) = grid.(x).(y) in grid.(x).(y) <- (a,9);
		increment grid (adjacents (x,y) l c);;
		
(* ---- Grille de départ ---- *)

(* Renvoie la grille initiale *)
let new_game i j n = 
	let grille = Array.make_matrix i j (1,0) in
		let rec placer_bombes l c k grid = match k with
			|0 -> grid
			|_ -> placer_bombes l c (k-1) (mine l c grid (Random.int l) (Random.int c))
			in placer_bombes i j n grille;;

(* ---- Interactionss avec la grille ---- *)
(* -- Révélation de case: si on clique sur une case couverte, elle doit être révélée,
si la case dévoilée n'a pas de bombe ajacente, il faut révéler toutes les cases adjacentes automatiquement,
enfin, si on clique sur une case dévoilée et que le nombre de drapeaux adjacents correspond au nombre de bombes
adjacentes, on dévoile les cases où il n'y a pas de drapeaux. Si la case dévoilée est une bombe, on révèle la
la totalité de la grille *)

(* Liste d'adjacents couverts de x,y *)
let adj_couverts (x,y) i j grid = 
	let rec aux liste grid = match liste with
		|[] -> []
		|(x,y)::q when fst(grid.(x).(y)) = 1 -> (x,y)::(aux q grid)
		|t::q -> aux q grid
		in aux (adjacents (x,y) i j) grid;;

(* Liste de toutes les cases couvertes (ou avec un drapeau) de la grille *)
let all_couverts grid x y =
		let couverts = ref [] in 
			for i = 0 to x-1 do
				for j = 0 to y-1 do
					if fst (grid.(i).(j)) != 0 then
						couverts := (i,j) :: (!couverts);
				done;
			done;
			!couverts;;
			
(* Nombre de drapeaux adjacents *)
let rec n_flags (x,y) i j grid =
	let rec aux liste grid = match liste with
	|[] -> 0
	|(x,y)::q when fst(grid.(x).(y)) = 2 -> 1 + (aux q grid)
	|t::q -> aux q grid
	
	in aux (adjacents (x,y) i j) grid;;

(* Fonction qui renvoie la liste des cases à dévoiler *)	

let rec is_not_in x l = match l with
        |[]-> true
        |h::t when h = x -> false
        |h::t -> is_not_in x t;;

let rec click liste i j grid = let l = ref liste in
        let cases = ref [] in
        while !l <> [] do
                let (x,y) = List.hd(!l) in
                l := List.tl(!l);
                if is_not_in (x,y) !cases then
                match grid.(x).(y) with
                        |(0,n) when ((n_flags (x,y) i j grid) = n) -> l := !l @ (adj_couverts (x,y) i j grid);
                        |(1,9) -> cases := all_couverts grid i j;
                        l := [];
                        |(1,0) -> cases := (x,y)::(!cases);
									l :=  !l @ (adj_couverts (x,y) i j grid);
                        |(1,_) -> cases := (x,y)::(!cases);
                        |_ -> ();
        done;
        !cases;;

(* -- Placement de drapeaux : possible que sur un case couverte, si une case possède déjà un drapeau,
on l'enlève *)

(* Renvoie la liste des cases où le statut du drapeau change *)

let put_flag x y grid = match fst(grid.(x).(y)) with
	|1 -> grid.(x).(y) <- (2,snd(grid.(x).(y)))
	|2 -> grid.(x).(y) <- (1,snd(grid.(x).(y)))
	|_ -> ();;
	
(* -- Modification de la matrice *)

(* Prend un argument une liste de cases à modifier*)
let change_cases liste grid = let l = ref liste in
	while !l != [] do
	let (x,y) = List.hd(!l) in
		l := List.tl(!l);
		grid.(x).(y) <- (0,snd(grid.(x).(y)));
	done;
;;

(* ------------------------------ Graphismes --------------------------------- *)

(* ---- Couleurs ---- *)
let darkerBlue = 0x0392cf;;
let basicBlue = 0x2195f3;;
let lighterBlue = 0x03a8f4;;
let skyBlue = 0x90caf9;;
let darkerSkyBlue = 0x64b4f6;;
let gray1 = 0xcfd8dc;;
let gray2 = 0xfafafa;;
let gray3 = 0x78909c;;
let gray4 = 0x607d8b;;
let gray5 = 0x455a64;;
let gray6 = 0x37474f
let lightRed = 0xe53835;;
let darkRed = 0xb71c1c;;
let oneColor = rgb 0 0 255;;
let twoColor = rgb 0 153 0;;
let threeColor = rgb 255 0 0;;
let fourColor = rgb 77 0 77;;
let fiveColor = rgb 0 0 255;;
let sixColor = rgb 51 204 204;;
let sevenColor = rgb 0 0 0;;
let eightColor = rgb 89 89 89;;

(* Crée une matrice de couleur unie de format l x l *)
(* Base sur laquelle on forme une image : elle représentera un pixel *)
let pixel color l = Array.make_matrix l l color;;

(* -- Dessin des cases, pixel par pixel, format 8x8, taille du pixel de base modulable *)

(* Case couverte *)
let drawCell i j l =
	draw_image (make_image (pixel basicBlue l)) i j;
	draw_image (make_image (pixel basicBlue l)) (i+7*l) (j+7*l);
	for n = 1 to 7 do
		draw_image (make_image (pixel lighterBlue l)) (i+n*l) j;
		draw_image (make_image (pixel lighterBlue l)) (i+7*l) (j+n*l-l);
		draw_image (make_image (pixel darkerBlue l)) i (j+n*l);
		draw_image (make_image (pixel darkerBlue l)) (i+n*l-l) (j+7*l);
	done;
	for n = 1 to 6 do
		for k = 1 to 6 do
		draw_image (make_image (pixel basicBlue l)) (i+n*l) (j+k*l)
		done;
	done;
	;;
	
(* Case découverte, 0 bombe adjacente *)
let drawDiscoveredCell i j l =
	for n = 1 to 6 do
		for k = 1 to 6 do
			draw_image (make_image (pixel skyBlue l)) (i+n*l) (j+k*l)
		done;
	done;
	for n = 1 to 8 do
		draw_image (make_image (pixel darkerSkyBlue l)) (i+n*l-l) j;
		draw_image (make_image (pixel darkerSkyBlue l)) (i+7*l) (j+n*l-l);
		draw_image (make_image (pixel darkerSkyBlue l)) i (j+n*l-l);
		draw_image (make_image (pixel darkerSkyBlue l)) (i+n*l-l) (j+7*l);
	done;
	;;

(* Case où il y a une bombe, fond découvert *)
let drawBomb i j l =
	drawDiscoveredCell i j l;
	draw_image (make_image (pixel gray1 l)) (i+l) (j+5*l);
	draw_image (make_image (pixel gray1 l)) (i+2*l) (j+6*l);
	
	for n = 3 to 5 do 
		draw_image (make_image (pixel gray3 l)) (i+n*l) (j+6*l);
		draw_image (make_image (pixel gray3 l)) (i+l) (j + (n-1)*l);
	done;
	
	for n = 2 to 5 do
		for k = 2 to 5 do
		draw_image (make_image (pixel gray6 l)) (i+n*l) (j+k*l);
		done;
	done;
	draw_image (make_image (pixel gray2 l)) (i+2*l)  (j+4*l);
	draw_image (make_image (pixel gray2 l)) (i+2*l) (j+5*l);
	draw_image (make_image (pixel gray2 l)) (i+3*l) (j+5*l);
	draw_image (make_image (pixel gray4 l)) (i+5*l) (j+5*l);
	draw_image (make_image (pixel gray4 l)) (i+6*l) (j+5*l);
	draw_image (make_image (pixel gray4 l)) (i+6*l) (j+4*l);
	draw_image (make_image (pixel gray4 l)) (i+2*l) (j+2*l);
	draw_image (make_image (pixel gray4 l)) (i+2*l) (j+l);
	draw_image (make_image (pixel gray4 l)) (i+3*l) (j+l);
	for n = 4 to 5 do
		draw_image (make_image (pixel gray5 l)) (i+n*l) (j+l);
		draw_image (make_image (pixel gray5 l)) (i+6*l) (j+(n-2)*l);
	done; 
	draw_image (make_image (pixel gray5 l)) (i+5*l) (j+2*l)
	;;

(* Case où il y a une bombe, explosion si le joueur perd *)
let drawExplodedBomb i j l =
	for n = 0 to 7 do
		for k = 0 to 7 do
			draw_image (make_image (pixel darkRed l)) (i+n*l) (j+k*l)
		done;
	done;
	for n = 3 to 4 do
		draw_image (make_image (pixel lightRed l)) (i) (j+n*l);
		draw_image (make_image (pixel lightRed l)) (i+n*l) (j+7*l);
		draw_image (make_image (pixel lightRed l)) (i+7*l) (j+n*l);
		draw_image (make_image (pixel lightRed l)) (i+n*l) (j);
	done;
	draw_image (make_image (pixel lightRed l)) (i) (j);
	draw_image (make_image (pixel lightRed l)) (i) (j+7*l);
	draw_image (make_image (pixel lightRed l)) (i+7*l) (j);
	draw_image (make_image (pixel lightRed l)) (i+7*l) (j+7*l);
	
	draw_image (make_image (pixel gray1 l)) (i+l) (j+5*l);
	draw_image (make_image (pixel gray1 l)) (i+2*l) (j+6*l);
	
	for n = 3 to 5 do 
		draw_image (make_image (pixel gray3 l)) (i+n*l) (j+6*l);
		draw_image (make_image (pixel gray3 l)) (i+l) (j + (n-1)*l);
	done;
	
	for n = 2 to 5 do
		for k = 2 to 5 do
		draw_image (make_image (pixel gray6 l)) (i+n*l) (j+k*l);
		done;
	done;
	draw_image (make_image (pixel gray2 l)) (i+2*l)  (j+4*l);
	draw_image (make_image (pixel gray2 l)) (i+2*l) (j+5*l);
	draw_image (make_image (pixel gray2 l)) (i+3*l) (j+5*l);
	draw_image (make_image (pixel gray4 l)) (i+5*l) (j+5*l);
	draw_image (make_image (pixel gray4 l)) (i+6*l) (j+5*l);
	draw_image (make_image (pixel gray4 l)) (i+6*l) (j+4*l);
	draw_image (make_image (pixel gray4 l)) (i+2*l) (j+2*l);
	draw_image (make_image (pixel gray4 l)) (i+2*l) (j+l);
	draw_image (make_image (pixel gray4 l)) (i+3*l) (j+l);
	for n = 4 to 5 do
		draw_image (make_image (pixel gray5 l)) (i+n*l) (j+l);
		draw_image (make_image (pixel gray5 l)) (i+6*l) (j+(n-2)*l);
	done; 
	draw_image (make_image (pixel gray5 l)) (i+5*l) (j+2*l)
	;;
	
(* numéros*)

(* 1 *)
let drawOne i j l = 
	drawDiscoveredCell i j l;
	for n = 3 to 5 do
		draw_image (make_image (pixel oneColor l)) (i+n*l) (j+l)
	done;
	for n = 2 to 5 do
		draw_image (make_image (pixel oneColor l)) (j+4*l) (i+n*l)
	done;
	for n = 3 to 4 do
		draw_image (make_image (pixel oneColor l)) (j+n*l) (i+6*l)
	done;;

(* 2 *)
let drawTwo j i l =
	drawDiscoveredCell i j l;	
	for n = 2 to 5 do
		draw_image (make_image (pixel twoColor l )) (i+n*l) (j+l);
		draw_image (make_image (pixel twoColor l)) (i+n*l) (j+6*l);
	done;
	for n = 2 to 4 do
		draw_image (make_image (pixel twoColor l )) (i+2*l) (j+n*l)
	done;
	for n = 3 to 5 do
		draw_image (make_image (pixel twoColor l)) (i+n*l) (j+4*l)
	done;
	draw_image (make_image (pixel twoColor l)) (i+5*l) (j+5*l);;

(* 3 *)
let drawThree j i l =
	drawDiscoveredCell i j l;
	for n = 2 to 5 do
		draw_image (make_image (pixel threeColor l)) (i+n*l) (j+l);
		draw_image (make_image (pixel threeColor l)) (i+n*l) (j+4*l);
		draw_image (make_image (pixel threeColor l)) (i+n*l) (j+6*l);
	done;
	for n = 2 to 3 do
		draw_image (make_image (pixel threeColor l)) (i+5*l) (j+n*l)
	done;
		draw_image (make_image (pixel threeColor l)) (i+5*l) (j+5*l);;

(* 4 *)
let drawFour j i l =
	drawDiscoveredCell i j l;
	for n = 1 to 6 do
		draw_image (make_image (pixel fourColor l)) (i+5*l) (j+n*l)
		done;
	for n = 4 to 6 do
		draw_image (make_image (pixel fourColor l)) (i+2*l) (j+n*l)
 	done;
	draw_image (make_image (pixel fourColor l)) (i+3*l) (j+4*l);
	draw_image (make_image (pixel fourColor l)) (i+4*l) (j+4*l);;

(* 5 *)
let drawFive j i l =
	drawDiscoveredCell i j l;
	for n = 2 to 5 do
		draw_image (make_image (pixel fiveColor l)) (i+n*l) (j+l);
		draw_image (make_image (pixel fiveColor l)) (i+n*l) (j+6*l);
	done;
	for n = 2 to 4 do
		draw_image (make_image (pixel fiveColor l)) (i+5*l) (j+n*l);
		draw_image (make_image (pixel fiveColor l)) (i+n*l) (j+4*l);
	done;
	draw_image (make_image (pixel fiveColor l)) (i+2*l) (j+5*l);;

(* 6 *)
let drawSix j i l =
	drawDiscoveredCell i j l;
	for n = 2 to 5 do
		draw_image (make_image (pixel sixColor l)) (i+n*l) (j+l);
		draw_image (make_image (pixel sixColor l)) (i+n*l) (j+6*l);
		draw_image (make_image (pixel sixColor l)) (i+2*l) (j+n*l);
	done;
	for n = 3 to 5 do
		draw_image (make_image (pixel sixColor l)) (i+n*l) (j+4*l)
	done;
	for n = 2 to 3 do
		draw_image (make_image (pixel sixColor l)) (i+5*l) (j+n*l)
	done;
	;;

(* 7 *)
let drawSeven j i l =
	drawDiscoveredCell i j l;
	for n = 1 to 6 do
		draw_image (make_image (pixel sevenColor l)) (i+5*l) (j+n*l)
	done;
	for n = 2 to 4 do
		draw_image (make_image (pixel sevenColor l)) (i+n*l) (j+6*l)
	done;
	draw_image (make_image (pixel sevenColor l)) (i+2*l) (j+5*l);;

(* 8 *)
let drawEight i j l =
	drawDiscoveredCell i j l;
	for n = 1 to 6 do
		draw_image (make_image (pixel eightColor l)) (i+2*l) (j+n*l);
		draw_image (make_image (pixel eightColor l)) (i+5*l) (j+n*l);
	done;
	for k = 3 to 4 do
		draw_image (make_image (pixel eightColor l)) (i+k*l) (j+1*l);
		draw_image (make_image (pixel eightColor l)) (i+k*l) (j+4*l);
		draw_image (make_image (pixel eightColor l)) (i+k*l) (j+6*l);
	done;
	;;
	
(* Drapeau *)
let drawFlag i j l =
	drawCell i j l;
	for k = 4 to 6 do 
		draw_image (make_image (pixel eightColor l)) (i+k*l) (j+1*l)
	done;
	for n = 2 to 6 do 
		draw_image (make_image (pixel eightColor l)) (i+5*l) (j+n*l)
	done;
	for k = 3 to 4 do
		for n = 4 to 6 do
			draw_image (make_image (pixel threeColor l)) (i+k*l) (j+n*l)
		done;
	done;
	draw_image (make_image (pixel threeColor l)) (i+2*l) (j+5*l);;

(* -- Matrices images des cases *)
(* On récupèrera la matrice image de chaque case au début du jeu pour les réutiliser lors de la partie *)

(* Renvoie la matrice image d'une fonction de tracé qui trace l'image en 0 0 *)
let get_cell drawcellfun l =
	drawcellfun 0 0 l;
	let cell = get_image 0 0 (8*l) (8*l) in
	cell;;

(* Fonctions qui donnent les matrices images de taille de pixel l *)
let zeroBombe l = get_cell drawDiscoveredCell l;;
let uneBombe l = get_cell drawOne l;;
let deuxBombes l = get_cell drawTwo l;;
let troisBombes l = get_cell drawThree l;;
let quatreBombes l = get_cell drawFour l;;
let cinqBombes l = get_cell drawFive l;;
let sixBombes l = get_cell drawSix l;;
let septBombes l = get_cell drawSeven l;;
let huitBombes l = get_cell drawEight l;;
let bombe l = get_cell drawBomb l;;
let bombeExplosee l = get_cell drawExplodedBomb l;;
let caseCouverte l = get_cell drawCell l;;
let caseDrapeau l = get_cell drawFlag l;;

(* -- Tracé de la grille de départ *)

(* Met en valeur graphiquement la case selectionnée par le joueur par un contour noir *)
let selection coordonnees l =
	let i,j = !coordonnees in
	draw_rect (8*l*i) (8*l*j) (8*l-1) (8*l-1);;
	
(* Tracé de la grille de départ dans la fenêtre graphique *)
let draw_grid2 a b l = 
	let cell = (get_cell drawCell l) in
	for i=0 to ((max a b) - 1) do
		for j=0 to ((min a b) -1) do
			draw_image cell (i*8*l) (j*8*l)
		done;
	done;
	selection (ref(0,0)) l
	;;

(* -- Tracé dynamique des cases *)
(* Prend en argument une liste de cases à dévoiler, les matrices images des différentes cases et la taille de pixel de base *)
let rec draw tab liste_change one two three four five six seven eight bomb e_bomb covered uncovered flag l =
	match liste_change with
	|[] -> ()
	|_ -> let (x,y) = List.hd(liste_change) in let (a,b) = tab.(x).(y) in
				match (a,b) with
					|(1,_) -> draw_image covered (x*8*l) (y*8*l); 
						draw tab (List.tl(liste_change)) one two three four five six seven eight bomb e_bomb covered uncovered flag l
					|(2,_) -> draw_image flag (x*8*l) (y*8*l);
						draw tab (List.tl(liste_change)) one two three four five six seven eight bomb e_bomb covered uncovered flag l
					|(_,0) -> draw_image uncovered (x*l*8) (y*l*8);
						draw tab(List.tl(liste_change)) one two three four five six seven eight bomb e_bomb covered uncovered flag l
					|(_,1) -> draw_image one (x*l*8) (y*l*8);
						draw tab (List.tl(liste_change)) one two three four five six seven eight bomb e_bomb covered uncovered flag l
					|(_,2) -> draw_image two (x*l*8) (y*l*8);
						draw tab (List.tl(liste_change)) one two three four five six seven eight bomb e_bomb covered uncovered flag l
					|(_,3) -> draw_image three (x*l*8) (y*l*8);
						draw tab (List.tl(liste_change)) one two three four five six seven eight bomb e_bomb covered uncovered flag l
					|(_,4) -> draw_image four (x*l*8) (y*l*8);
						draw tab (List.tl(liste_change)) one two three four five six seven eight bomb e_bomb covered uncovered flag l
					|(_,5) -> draw_image five (x*l*8) (y*l*8);
						draw tab (List.tl(liste_change)) one two three four five six seven eight bomb e_bomb covered uncovered flag l
					|(_,6) -> draw_image six (x*l*8) (y*l*8);
						draw tab (List.tl(liste_change)) one two three four five six seven eight bomb e_bomb covered uncovered flag l
					|(_,7) -> draw_image seven (x*l*8) (y*l*8);
						draw tab (List.tl(liste_change)) one two three four five six seven eight bomb e_bomb covered uncovered flag l
					|(_,8) -> draw_image eight (x*l*8) (y*l*8);
						draw tab (List.tl(liste_change)) one two three four five six seven eight bomb e_bomb covered uncovered flag l
					|(_,9) -> draw_image e_bomb (x*l*8) (y*l*8);
						draw tab (List.tl(liste_change)) one two three four five six seven eight bomb e_bomb covered uncovered flag l;;

(* ----------------------------- Commandes ------------------------------- *)
(*Déplacement de la sélection *)

(* Déplacement vers la droite *)
let move_right coordinates max l tab one two three four five six seven eight bomb e_bomb covered uncovered flag =
	let i_0, j_0 = !coordinates in
	draw tab [(i_0,j_0)] one two three four five six seven eight bomb e_bomb covered uncovered flag l;
		if i_0 = max then
		begin 
			coordinates := (0,(j_0));
			selection coordinates l;
		end
		else
			coordinates := (i_0+1),(j_0);
			selection coordinates l;;

(* Déplacement vers le haut *)
let move_up coordinates max l tab one two three four five six seven eight bomb e_bomb covered uncovered flag =
	let i_0, j_0 = !coordinates in
	draw tab [(i_0,j_0)]  one two three four five six seven eight bomb e_bomb covered uncovered flag l;
		if j_0 = max then
		begin 
			coordinates := (i_0),(0);
			selection coordinates l;
		end
		else 
			coordinates := (i_0),(j_0+1);
			selection coordinates l;;

(* Déplacement vers le bas *)
let move_down coordinates max l tab one two three four five six seven eight bomb e_bomb covered uncovered flag =
	let i_0, j_0 = !coordinates in
	draw tab [(i_0,j_0)] one two three four five six seven eight bomb e_bomb covered uncovered flag l;
		if j_0 = 0 then
		begin 
			coordinates := i_0,max;
			selection coordinates l;
		end
		else 
			coordinates := i_0,(j_0-1);
			selection coordinates l;;

(* Déplacement vers la gauche *)
let move_left coordinates max l tab one two three four five six seven eight bomb e_bomb covered uncovered flag =
	let i_0, j_0 = !coordinates in
	draw tab [(i_0,j_0)]  one two three four five six seven eight bomb e_bomb covered uncovered flag l;
		if i_0 = 0 then
		begin 
			coordinates := max,j_0;
			selection coordinates l;
		end
		else 
			coordinates := (i_0-1),j_0;
			selection coordinates l;;

(* -- Révéler les cases *)
let reveal liste_cases_changer tab one two three four five six seven eight bomb e_bomb covered uncovered flag l =
	change_cases liste_cases_changer tab;
	draw tab liste_cases_changer one two three four five six seven eight bomb e_bomb covered uncovered flag l;;
	
(* -- Fonction des commandes *)
	
let controls key coordinates a b l grid one two three four five six seven eight bomb e_bomb covered uncovered flag = 
	match key with
		|'z' -> move_up coordinates (a-1) l
			grid one two three four five six seven eight bomb e_bomb covered uncovered flag
		|'d' -> move_right coordinates (b-1) l
			grid one two three four five six seven eight bomb e_bomb covered uncovered flag
		|'q' -> move_left coordinates (b-1) l
			grid one two three four five six seven eight bomb e_bomb covered uncovered flag
		|'s' -> move_down coordinates (a-1) l
			grid one two three four five six seven eight bomb e_bomb covered uncovered flag
		|'e' -> let i,j = !coordinates in
						reveal (click [(i,j)] a b grid) grid one two three four five six seven eight bomb e_bomb covered uncovered flag l
		|'f' -> let i,j = !coordinates in
						put_flag i j grid;
						draw grid [(i,j)] one two three four five six seven eight bomb e_bomb covered uncovered flag l;
						selection coordinates l;
		|'l' -> close_graph()
		| _ -> ();;
	let perdu = fun () -> ();;


(* Boucle à moitié finie, mais fonctionnelle, parfois *)
#exception End;;
let skel a b n f_end f_key  = 
	let taille_voulue = min (600/a) (600/b) in
		let l = taille_voulue/8 in
			let tableau = ref (new_game a b n) and
			coordinates = ref (0,0) and
			zero = zeroBombe l and
			une = uneBombe l and
			deux = deuxBombes l and
			trois = troisBombes l and
			quatre = quatreBombes l and
			cinq = cinqBombes l and
			six = sixBombes l and
			sept = septBombes l and
			huit = huitBombes l and
			bomb = bombe l and
			explodedBomb = bombeExplosee l and
			couvert = caseCouverte l and
			drapeau = caseDrapeau l in
			draw_grid2 a b l;
  try 
      while true do 
        try 
          let s = Graphics.wait_next_event 
                    [Graphics.Key_pressed] 
          in if s.Graphics.keypressed 
          then f_key s.Graphics.key coordinates a b l (!tableau) 
          une deux trois quatre cinq six sept huit bomb explodedBomb couvert zero drapeau
        with 
				|_  -> failwith "oops"
				
      done
  with 
      |_  -> f_end ();;

(* Réssayer si ça ne fonctionne pas *)
(* a,b format de la grille souhaité, n le nombre de bombes, strictement inférieur au nombre de cases totales *)
let start a b n =
	open_graph " 600x600";
	set_window_title "Démineur de Alicia et Adèle xD";
	skel a b n perdu controls;;

start 20 20 50;;