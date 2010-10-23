type vec = { x : float ; y : float }
type rect = {
  ul : vec ;
  lr : vec
}

let inside point rect =
  point.x >= rect.ul.x &&
    point.x <= rect.lr.x &&
    point.y >= rect.ul.y &&
    point.y <= rect.lr.y
    (* ie : l’origine est en haut à gauche *)

let center rect = { x = (rect.ul.x +. rect.lr.x) /. 2. ;
		    y = (rect.ul.y +. rect.lr.y) /. 2. }

type 'a quadtree =
  | Leaf of ('a * vec) list * rect
  | Node of 'a quadnode
and 'a quadnode = {
  center : vec ;
  surf : rect ;
  nw : 'a quadtree ;
  ne : 'a quadtree ;
  sw : 'a quadtree ;
  se : 'a quadtree ;
}

let surf = function
  | Leaf (_,surf) -> surf
  | Node n -> n.surf

let superposition r s =
  ( r.ul.x < s.lr.x && r.lr.x > s.ul.x )
  && ( r.ul.y < s.lr.y && r.lr.y > s.ul.y )
    (* true quand les rectangles r et s se recouvrent *)

let empty v = Leaf ([],v)

(* ne conserve que les feuilles dont la surface et rect se recouvrent, et les seules branches qui
   les soutiennent *)
let elagage tree rect =
  let rec f tree =
    let surf = surf tree in
      match superposition surf rect, tree with
	  false, _ -> empty surf
	| true,  Node node -> Node { node with nw=f node.nw ; sw=f node.sw ; se=f node.se ; ne=f node.ne }
	| true,  leaf -> leaf
  in
    f tree


let is_inside pos n =
  let surf = surf n in
    if not (inside pos surf)
    then raise (Invalid_argument "insert")

let rec insert nlimit e = function
  | Leaf (values, surf) ->
      if List.length values + 1 > nlimit 
      then
	let c = center surf in
	let n = Node 
	  { center = c ;
            surf = surf ;
            nw = empty { ul = surf.ul ; lr = c } ;
            sw = empty { ul = { x = surf.ul.x ; y = c.y } ; lr = { x = c.x ; y = surf.lr.y } };
            se = empty { ul = c ; lr = surf.lr } ;
            ne = empty { ul = { x = c.x ; y = surf.ul.y } ; lr = { x = surf.lr.x ; y = c.y } } }
	in List.fold_right (insert nlimit) values (insert nlimit e n)
      else Leaf (e :: values, surf)
  | Node n ->
      let pos = snd e in
      let node =
	match (pos.x < n.center.x, pos.y < n.center.y) with
	    (true, true) -> { n with nw = insert nlimit e n.nw }
	  | (true, false) -> { n with sw = insert nlimit e n.sw }
	  | (false, false) -> { n with se = insert nlimit e n.se }
	  | (false, true) -> { n with ne = insert nlimit e n.ne }
      in Node node

let xdim = 600.
and ydim = 400.

let map_rect = {
  ul = { x = 0. ; y = 0. } ;
  lr = { x = xdim ; y = ydim }
}


let random_points n =
  Array.init n (fun _ -> { x = Random.float xdim ; y = Random.float ydim })

let random_quadtree n =
  Array.fold_right
    (fun p -> insert 10 ((),p))
    (random_points n)
    (empty map_rect)
    

#load "graphics.cma";;

let _ = Graphics.open_graph "";;

let display_rect r =
  Graphics.draw_rect
    (int_of_float r.ul.x)
    (int_of_float r.ul.y)
    (int_of_float (r.lr.x -. r.ul.x))
    (int_of_float (r.lr.y -. r.ul.y))

let rec display_tree = function
  | Leaf (values,surf) ->
      display_rect surf ;
      List.iter
	(fun (_,pos) ->
	   Graphics.draw_circle
	     (int_of_float pos.x)
	     (int_of_float pos.y)
	     1)
	values
  | Node n ->
      display_rect n.surf ;
      display_tree n.nw ;
      display_tree n.ne ;
      display_tree n.se ;
      display_tree n.sw

let tree = random_quadtree 4000;;

let selection = { ul = { x = 172.; y=257.; } ;
                  lr = { x = 366.; y=322.; } }

let arbre_elague = elagage tree selection

let _ =
  Graphics.clear_graph () ;
  Graphics.set_color Graphics.black ;
  display_tree tree ;
  read_line();;

let _ =
  Graphics.clear_graph () ;
  display_tree arbre_elague;
  Graphics.set_color Graphics.red;
  display_rect selection ;
  read_line();;

