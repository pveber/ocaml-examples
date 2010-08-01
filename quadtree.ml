let a = 1

type 'a list = Empty | Cons of 'a * 'a list;;

Empty;;
[];;

Empty = [];;

let rec map f = function 
    Empty -> Empty
  | Cons (h, t) -> Cons (f h, map f t)
;;

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;

let rec map f = function
    Empty -> Empty
  | Node (n,l,r) -> Node (f n,map f l, map f r)
;;

type ('a,'b) tree = Leaf of 'b | Node of 'a * ('a,'b) tree * ('a,'b) tree


type ('a, 'b) tree = 
    Leaf of 'b
  | Node of ('a,'b) node
and ('a,'b) node = {
  value : 'a ;
  left : ('a, 'b) tree ;
  right : ('a, 'b) tree
}

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

let center rect = { x = (rect.ul.x +. rect.lr.x) /. 2. ;
		    y = (rect.ul.y +. rect.lr.y) /. 2. }

type 'a quadtree = 
  | Empty of rect
  | Leaf of 'a * vec * rect
  | Node of 'a quadnode
and 'a quadnode = {
  center : vec ;
  surf : rect ;
  nw : 'a quadtree ;
  sw : 'a quadtree ;
  se : 'a quadtree ;
  ne : 'a quadtree 
}


let surf = function
  | Empty surf -> surf
  | Leaf (_,_,surf) -> surf
  | Node n -> n.surf

let is_inside pos n =
  let surf = surf n in
    if not (inside pos surf) 
    then raise (Invalid_argument "insert")

let empty v = Empty v


let rec insert value pos = function
    Empty surf -> Leaf (value, pos, surf)
  | Leaf (value', pos', surf) ->
      let c = center surf in 
      let n = Node { center = c ;
		     surf = surf ;
		     nw = Empty { ul = surf.ul ; lr = c } ;
		     sw = Empty { ul = { x = surf.ul.x ; y = c.y } ; lr = { x = c.x ; y = surf.lr.y } };
		     se = Empty { ul = c ; lr = surf.lr } ;
		     ne = Empty { ul = { x = c.x ; y = surf.ul.y } ; lr = { x = surf.lr.x ; y = c.y } } }
      in insert value' pos' (insert value pos n)
  | Node n ->
      let node = 
      match (pos.x < n.center.x, pos.y < n.center.y) with
	  (true, true) -> { n with nw = insert value pos n.nw }
	| (true, false) -> { n with sw = insert value pos n.sw }
	| (false, false) -> { n with se = insert value pos n.se }
	| (false, true) -> { n with ne = insert value pos n.ne }
      in Node node

let xdim = 600.
and ydim = 400.

let map_rect = {
  ul = { x =   0. ; y =   0. } ;
  lr = { x = xdim ; y = ydim } 
}


let random_points n = 
  Array.init n (fun _ -> { x = Random.float xdim ; y = Random.float ydim })

let random_quadtree n = 
  Array.fold_right
    (insert ())
    (random_points n)
    (empty map_rect)
    

#load "graphics.cma";;

let _ = Graphics.open_graph "";;

let tree = random_quadtree 20;;

let display_rect r = 
  Graphics.draw_rect 
    (int_of_float r.ul.x)
    (int_of_float r.ul.y)
    (int_of_float (r.lr.x -. r.ul.x))
    (int_of_float (r.lr.y -. r.ul.y))

let rec display_tree = function
    Empty surf -> display_rect surf
  | Leaf (_,pos,surf) ->
      display_rect surf ;
      Graphics.draw_circle 
	(int_of_float pos.x) 
	(int_of_float pos.y) 
	3 
  | Node n -> 
      display_rect n.surf ;
      display_tree n.nw ;
      display_tree n.ne ;
      display_tree n.se ;
      display_tree n.sw

let _ = 
  Graphics.clear_graph () ;
  display_tree tree;;
