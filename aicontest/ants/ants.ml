
let out_chan = stderr (*open_out "mybot_err.log"*) ;;

let get_time () = Unix.gettimeofday ();;

let ddebug s = 
   output_string out_chan s; 
   flush out_chan

module CoordSet = Set.Make (
  struct 
    let compare = Pervasives.compare 
    type t = (int*int)
end)

module PlayerCoordSet = Set.Make (
  struct 
    let compare = Pervasives.compare 
    type t = int*(int*int)
end)

let sum_list xs = List.fold_left (+.) 0.0 xs

let sum_array xs = Array.fold_left (+.) 0.0 xs

let (|>) x f = f x 

let rec times n f =
 if n = 0 then () else f ; times (n-1) f

let maxBy f xx =
   let rec loop p (m,fm)  = 
     match p with 
      | [] -> (m,fm)
      | h::tail -> let fh = f h in 
                   if fh > fm then loop tail (h,fh) 
                   else loop tail (m,fm)
   in 
   match xx with 
    | [] -> failwith "invalid arg: empty list"
    | h::tail -> loop tail (h,(f h))


type game_setup =
 {
   loadtime : int;
   turntime : int;
   rows : int;
   cols : int;
   turns : int;
   viewradius2 : int;
   attackradius2 : int;
   spawnradius2 : int;
   player_seed : int;
 }

type dir = [ `N | `E | `S | `W | `Stop ];;
type tile = [ `Water | `Land | `AllyHill | `EnemyHill | `Unseen];;
type ocupator = [ `Food | `Ally | `Enemy | `DeadAlly | `DeadEnemy | `Nothing | `Unknown];;

type mapb = 
 {
  whatis : tile;
  whatson : ocupator;
  seen : int;
  visible : bool;
  taken : bool;
}

type ant = {
    loc: (int*int);
    owner: int;
    busy:bool;
    food_a_la_vista: bool
  }

type food = {loc1:(int*int); collected : bool}

type random_var = float array

type tgame_state =
  { 
    setup : game_setup;
    turn : int;
    my_ants : ((int*int),ant) Hashtbl.t;
    mutable my_ants_number : int;
    enemy_ants : ((int*int),ant) Hashtbl.t;
    mutable enemy_ants_number : int;
    my_hills : ((int * int) * int) list;
    enemy_hills : ((int * int) * int) list;
    dead_ants : ant list;
    food : (int * int, food) Hashtbl.t;
    tmap: mapb array array; 
    agents: float array array array;
    attack: (int,int) Hashtbl.t array array;
    probabilities: (int,random_var) Hashtbl.t array array;
    prob_sum: (int,random_var) Hashtbl.t array array;
    my_hills_essence: (int*int,float) Hashtbl.t array array;
    go_time: float;
    mutable reset_enemy_hills : bool;
    view_area : (int*int) list;
    
 }

type event_type = [ `Food |`Enemy |`Explore];;

type event = { typ: event_type; direction: dir; steps: int; target: int*int}

type order = ((int * int) * dir);;

let directions_array = [| `N; `E; `S; `W ; `Stop |]
let directions_list  = [ `N; `E; `S; `W ; `Stop ]

let proto_tile =
 {
  whatis = `Unseen;
  whatson = `Unknown;
  seen = 0;
  visible = false;
  taken  = false;
}

let string_of_dir d = 
   match d with
    | `N -> "N" 
    | `E -> "E"
    | `S -> "S"
    | `W -> "W"
    | `Stop -> "Stop"


let int_of_tile t =
   match t with
    | `Unseen -> 0
    | `Land -> 1
    | `Water -> 2
    | `AllyHill -> 3
    | `EnemyHill -> 4


(* Begin input processing stuff *)

let set_turn gstate v =
   {gstate with turn = v}

let set_loadtime gstate v =
   {gstate with setup = {gstate.setup with loadtime = v}}

let set_turntime gstate v = 
   {gstate with setup = {gstate.setup with turntime = v}}

let set_rows gstate v = 
   {gstate with setup = {gstate.setup with rows = v}}

let set_cols gstate v = 
   {gstate with setup = {gstate.setup with cols = v}}

let set_turns gstate v = 
   {gstate with setup = {gstate.setup with turns = v}}

let set_viewradius2 gstate v = 
   {gstate with setup = {gstate.setup with viewradius2 = v}}

let set_attackradius2 gstate v = 
   {gstate with setup = {gstate.setup with attackradius2 = v}}

let set_spawnradius2 gstate v = 
   {gstate with setup = {gstate.setup with spawnradius2 = v}}

let set_player_seed gstate v = 
   {gstate with setup = {gstate.setup with player_seed = v}}

let uncomment s =
  try String.sub s 0 (String.index s '#')
  with Not_found -> s

let sscanf_cps fmt cont_ok cont_fail s =
  try Scanf.sscanf s fmt cont_ok
  with _ -> cont_fail s

let add_food gstate row col =
  gstate.tmap.(row).(col) <- 
    {gstate.tmap.(row).(col) with whatson = `Food ; whatis = `Land };
  Hashtbl.add gstate.food (row,col) { loc1 = (row,col) ; collected = false };
  gstate

let add_water gstate row col =
   gstate.tmap.(row).(col) <- 
      {gstate.tmap.(row).(col) with whatis = `Water};
   gstate

(* Note that this clears previously seen food. *)

let clear_tile t =
   match t.whatis with
    | `Water | `Unseen -> t
    | _ -> {t with whatis = `Land}

let clear_gstate gs =
 if gs.turn < 1 then gs else
  (
   for count_row = 0 to (Array.length gs.tmap - 1) do
      let test_row = gs.tmap.(count_row) in
      for count_col = 0 to (Array.length test_row - 1) do
        test_row.(count_col) <- { (clear_tile test_row.(count_col)) with visible = false; taken = false; whatson = `Nothing; };
	
	Hashtbl.clear gs.my_hills_essence.(count_row).(count_col);
	Hashtbl.clear gs.attack.(count_row).(count_col);
	Hashtbl.clear gs.probabilities.(count_row).(count_col);
	Hashtbl.clear gs.prob_sum.(count_row).(count_col);
	
	Array.iter (fun i -> gs.agents.(count_row).(count_col).(i) <- 0.0) [|0;1;2|]
   
      done
   done;
   Hashtbl.clear gs.my_ants;
   Hashtbl.clear gs.enemy_ants;
   Hashtbl.clear gs.food;
   {gs with 
    my_ants_number = 0; 
    enemy_ants_number = 0;
    dead_ants = []; 
    my_hills = []; enemy_hills = [];
 }
  )


let add_hill gstate row col owner =
   try
     (
      match owner with
       | 0 ->
	    gstate.tmap.(row).(col) <- 
               {gstate.tmap.(row).(col) with whatis = `AllyHill };
            {gstate with my_hills = (((row, col), owner) :: gstate.my_hills);}
       | n ->
	    gstate.tmap.(row).(col) <- 
               {gstate.tmap.(row).(col) with whatis = `EnemyHill };
            {gstate with enemy_hills = 
               (((row, col), owner) :: gstate.enemy_hills)}
     )
   with _ -> gstate

let add_ant gstate row col owner =
   try
     (
      let new_ant = { loc = (row,col) ; owner = owner; busy = false ; food_a_la_vista = false} in
      match owner with
       | 0 ->
	   gstate.tmap.(row).(col) <- 
	     {gstate.tmap.(row).(col) with whatson = `Ally; whatis = `Land; taken = true };
	    Hashtbl.replace gstate.my_ants (row,col) new_ant;
            {gstate with my_ants_number = gstate.my_ants_number + 1 }
       | n ->
	   gstate.tmap.(row).(col) <- 
             {gstate.tmap.(row).(col) with whatson = `Enemy; whatis = `Land };
	   Hashtbl.replace gstate.enemy_ants (row,col) new_ant;
            {gstate with  enemy_ants_number = gstate.enemy_ants_number }
     )
   with _ -> gstate

let add_dead_ant gstate row col owner =
   try
     (
      gstate.tmap.(row).(col) <- 
                {gstate.tmap.(row).(col) with whatson = 
                         match owner with
	                  | 0 ->`DeadAlly 
                          | _ -> `DeadEnemy };
      let new_ant = { loc = ( row, col); owner= owner; busy = false; food_a_la_vista = false } in
      {gstate with dead_ants = (new_ant :: gstate.dead_ants)}
     )
   with _ -> gstate


let rec range i f = if f i then i::(range (i + 1) f) else []

let (>>) lst f = List.concat(List.map f lst )
let one x = [x]
let guard b e = if b then e else []
let square_less_than p  x = x * x <= p

let unique lst = 
  lst
  |> List.fold_left (fun acc e -> CoordSet.add e acc) CoordSet.empty 
  |> CoordSet.elements 

let unique2 lst = 
  lst
  |> List.fold_left (fun acc e -> PlayerCoordSet.add e acc) PlayerCoordSet.empty 
  |> PlayerCoordSet.elements 


let circle_zone (x,y) radius2 = 
  range 0 (square_less_than radius2)
    >> ( fun dx -> range 0 (square_less_than radius2)
    >> ( fun dy -> guard ((dx * dx) + (dy * dy) <= radius2) 
                   (match dx,dy with
                     | 0, 0 -> one (x, y) 
                     | 0, _ -> [ (x, y + dy) ;
                               (x, y - dy) ]  
                     | _, 0 -> [  (x + dx, y)  ;
                                (x - dx, y) ] 
                     | _, _ -> [ (x + dx, y + dy);
                                (x + dx, y - dy);
                                (x - dx, y + dy);
                                (x - dx, y - dy)]
                               )))

let attack_area (x,y) radius2 = 
  let a0 = circle_zone (x,y) radius2 in
  let a1 = circle_zone (x+1,y) radius2 in
  let a2 = circle_zone (x-1,y) radius2 in
  let a3 = circle_zone (x,y+1) radius2 in
  let a4 = circle_zone (x,y-1) radius2 in
  List.flatten [a0;a1;a2;a3;a4]

let initialize_map gstate =
  let new_map = 
    Array.make_matrix gstate.setup.rows gstate.setup.cols proto_tile in
  let agent_matrix =  
    Array.make_matrix gstate.setup.rows gstate.setup.cols (Array.make 3 0.0) in
  let force_matrix =
    Array.make_matrix gstate.setup.rows gstate.setup.cols (Hashtbl.create 10) in
  let attack_matrix =
    Array.make_matrix gstate.setup.rows gstate.setup.cols (Hashtbl.create 10) in
  let prob_matrix =
    Array.make_matrix gstate.setup.rows gstate.setup.cols (Hashtbl.create 10) in
  let prob_sum_matrix =
    Array.make_matrix gstate.setup.rows gstate.setup.cols (Hashtbl.create 10) in
  let hills_essence_matrix = 
    Array.make_matrix gstate.setup.rows gstate.setup.cols (Hashtbl.create 10)
   in
   for r = 0 to gstate.setup.rows - 1 do
     for c = 0 to gstate.setup.cols - 1 do
       new_map.(r).(c) <-proto_tile;
       agent_matrix.(r).(c) <- Array.make 3 0.0;
       force_matrix.(r).(c) <- Hashtbl.create 10;
       attack_matrix.(r).(c) <- Hashtbl.create 10;
       prob_matrix.(r).(c) <- Hashtbl.create 10;
       prob_sum_matrix.(r).(c) <- Hashtbl.create 10;
       hills_essence_matrix.(r).(c) <- Hashtbl.create 10
     done
   done;
  {gstate with 
   tmap = new_map;
   agents = agent_matrix;
   attack = attack_matrix;
   probabilities = prob_matrix;
   prob_sum = prob_sum_matrix;
   my_hills_essence = hills_essence_matrix;
   view_area = circle_zone (0,0) gstate.setup.viewradius2 |> unique ; 
 }

(* This add_line function is a bit tricky to modify (make sure you get 
the parentheses in the right places if you change it). *)

let add_line gstate line =
   sscanf_cps "%s %d %d %d"
    (fun ad row col owner ->
       match ad with
        | "a" -> add_ant gstate row col owner
        | "d" -> add_dead_ant gstate row col owner
        | "h" -> add_hill gstate row col owner
        | bd -> gstate)
    (sscanf_cps "%s %d %d"
      (fun fw row col ->
         match fw with
          | "f" -> add_food gstate row col
          | "w" -> add_water gstate row col
          | _ -> gstate)
      (sscanf_cps "%s %d"
        (fun key v ->
            match key with
             | "turn" -> set_turn gstate v
             | "loadtime" -> set_loadtime gstate v
             | "turntime" -> set_turntime gstate v
             | "rows" -> set_rows gstate v
             | "cols" -> set_cols gstate v
             | "turns" -> set_turns gstate v
             | "viewradius2" -> set_viewradius2 gstate v
             | "attackradius2" -> set_attackradius2 gstate v
             | "spawnradius2" -> set_spawnradius2 gstate v
             | "player_seed" -> set_player_seed gstate v
             | _ -> gstate
        )
        (fun (line : string) ->
          gstate
(* swap this for the above line if you want it to fail on bad input
          if line = "" then
            gstate
          else
            failwith (Printf.sprintf "unable to parse '%s'" line)
*)
        )))
    (uncomment line)

let update gstate lines =
   let cgstate =
      if gstate.turn = 0 then
         gstate
      else
         clear_gstate gstate
   in
   let ugstate =
      List.fold_left add_line cgstate lines 
   in if ugstate.turn = 0 then
      if ugstate.setup.rows < 0
      || ugstate.setup.cols < 0 then
        (
         ddebug "\nBad setup info! Expect crashes!\n\n";
         ugstate
        )
      else initialize_map ugstate
   else ugstate

let read_lines () =
  let rec read_loop acc =
    let line = read_line () in
    if String.length line >= 2 && String.sub line 0 2 = "go" 
    || String.length line >= 3 && String.sub line 0 3 = "end"
    || String.length line >= 5 && String.sub line 0 5 = "ready" then
     (
      List.rev acc
     )
    else
      read_loop (line :: acc)
  in
  try Some (read_loop []) with End_of_file -> None
;;

let read gstate =
  let ll = read_lines () in
  let go_time = get_time () in
  match ll with
  | Some lines -> Some {(update gstate lines) with go_time = go_time}
  | None -> None
;;

(* End input section *)

(* Begin output section *)

let issue_order ((row, col), cdir) =
   let os = Printf.sprintf "o %d %d %s\n" row col (string_of_dir cdir)
   in
   Printf.printf "%s" os;
;;

(* Print go, newline, and flush buffer *)
let finish_turn () = Printf.printf "go\n%!";;

(* End output section *)

(* Helper functions *)

let step_unbound d (row, col) =
   match d with
    | `N -> (row - 1), col
    | `S -> (row + 1), col
    | `W -> row, (col - 1)
    | `E -> row, (col + 1)
    | `Stop -> row, col
;;

let rec wrap0 bound n =
   if bound < 0 then 
      (ddebug (Printf.sprintf "wrap0 below zero not allowed%!"); 0)
   else if n < 0 then wrap0 bound (n + bound)
   else if n >= bound then wrap0 bound (n - bound)
   else n
;;

let wrap_bound (rows, cols) (row, col) =
   wrap0 rows row, 
   wrap0 cols col
;;

(* tell me my target co-ordinates when I step in a direction *)
let step_dir d bounds (row, col) =
   let new_loc = step_unbound d (row, col) in
   wrap_bound bounds new_loc
;;

(*return the tile type at a location *)

let get_tile tmap (row, col) =
   try
      tmap.(row).(col).whatis
   with e -> ddebug (Printf.sprintf 
         "\nocaml Ants warning: exception getting tile %d, %d: %s\n" 
               row col (Printexc.to_string e));
         `Unseen
;;

(* shortest distance from point 1 to point 2: is it "inside", and how far? *)
let shorter_dist w p1 p2 =
   let d1 = abs (p2 - p1) in
   let d2 = w - d1 in
      (d1 < d2), (min d1 d2)
;;

(* see distance_and_direction below *)
let stepdistance_ndirection (rows, cols) (row1, col1) (row2, col2) =
   let row_si, row_dist =
      shorter_dist rows row1 row2
   in
   let col_si, col_dist =
      shorter_dist cols col1 col2
   in
   let row_dir =
     if row1 = row2 then `Stop
     else
      match row_si with true ->
         if row1 < row2 then `S
         else `N
      | false ->
         if row1 < row2 then `N
         else `S
   in
   let col_dir =
     if col1 = col2 then `Stop
     else
      match col_si with true ->
         if col1 < col2 then `E
         else `W
      | false ->
         if col1 < col2 then `W
         else `E
   in (row_dir, col_dir), (row_dist, col_dist)
;;

(* returns d, has a type declaration for some reason *)
let direction bounds p1 p2 =
   let d, _ = stepdistance_ndirection bounds p1 p2 in
      (d: (dir * dir)) 
;;

let fsquare_int i =
   let f = float_of_int i in f *. f
;;

(* distance squared *)
let distance2 (rows, cols) (src_row, src_col) (dst_row, dst_col) =
   let d1 = abs (src_row - dst_row) in
   let d2 = abs (src_col - dst_col) in
   let dr = min d1 (rows - d1) in
   let dc = min d2 (cols - d2) in
      (dr * dr) + (dc * dc)

(* returns the distance and the two directions you might travel in to 
get from p1 to p2 ignoring water, with `Stop(s) for none *)
let distance_and_direction bounds p1 p2 =
   let d, (r, c) = stepdistance_ndirection bounds p1 p2 in
      d, (sqrt ((fsquare_int r) +. (fsquare_int c)))


let update_spot spot gstate loc =
   let bounds = gstate.setup.rows, gstate.setup.cols in
   let p, q = wrap_bound bounds spot in
   let tmap = gstate.tmap in
   if not(tmap.(p).(q).visible) then
     begin
       tmap.(p).(q) <- { tmap.(p).(q) with seen = gstate.turn; visible = true };
       if tmap.(p).(q).whatis = `Unseen then
	 tmap.(p).(q) <- { tmap.(p).(q) with whatis = `Land }
       else
	 begin
	   if tmap.(p).(q).whatis = `EnemyHill  && not(List.exists (fun ((r,c),_) -> r = p && c = q) gstate.enemy_hills) then
	   begin
	     gstate.reset_enemy_hills <- true; 
	     tmap.(p).(q) <- { tmap.(p).(q) with whatis = `Land }
	   end;
	   if tmap.(p).(q).whatson =`Food then 
	     let ant = Hashtbl.find gstate.my_ants loc in 
	     Hashtbl.replace gstate.my_ants loc {ant with food_a_la_vista = true ;}
	 end
     end
   else if tmap.(p).(q).whatson =`Food then 
	     let ant = Hashtbl.find gstate.my_ants loc in 
	     Hashtbl.replace gstate.my_ants loc {ant with food_a_la_vista = true ;}

let update_ant_vision gstate ant =
  let (c_row,c_col) = ant.loc in
  let update_squares (dx, dy) =
    update_spot (c_row + dx,c_col + dy) gstate ant.loc
  in List.iter update_squares gstate.view_area

let look_up hash element default  =
 if Hashtbl.mem hash element then Hashtbl.find hash element else default

let translate bounds area loc =
   let r,c = loc in
   area |> List.map ( fun (dx,dy) -> wrap_bound bounds (r + dx, c + dy ))

let binomial_coefficient n p =
  let p = if p< n -. p then p else n -. p in
  let rec cm res  num denum =
    if denum <= p then cm ((res *. num) /. denum)(num -. 1.) (denum +. 1.)
    else res in 
  cm 1. n 1.

let binomial n =
  let p = 1.0 /. 5.0 in  
  n  *. p *. ((1.0 -. p) ** (n -. 1.0))

let distribution p = [| 1.0 -. p; p |]

let convolution a b =
  let n = Array.length a in 
  let m = Array.length b in 
  let conv = Array.make (n + m - 1) 0.0 in
  for i = 0 to (n-1) do
    for j = 0 to (m-1) do 
      conv.(i+j) <- conv.(i+j) +. (a.(i) *. b.(m-j-1))
    done
  done;
  conv

let calculate_occupancy_probabilities st all_ants =
  let bounds = (st.setup.rows,st.setup.cols) in
  let envelope = 
    all_ants 
 |> List.map ( fun x -> List.map (fun y -> x.owner,step_dir y bounds (x.loc)) directions_list)
 |> List.flatten
 |> List.filter ( fun (_,(a,b)) -> st.tmap.(a).(b).whatis<>`Water) in
(* assign binomials random variables to every cell an ant can move to*)
  envelope 
 |> List.iter (fun (owner,(r,c)) -> 
     let hash = st.attack.(r).(c) in 
     let oldval = look_up hash owner 0 in 
     Hashtbl.replace hash owner (oldval + 1));
  envelope 
 |> List.iter (fun (owner,(r,c)) -> 
     let hash = st.probabilities.(r).(c) in 
     if not(Hashtbl.mem hash owner) then 
       begin
	 let n = look_up (st.attack.(r).(c)) owner 0 in
         let dd = binomial (float n) in
	 let prob = dd |> distribution in
	 Hashtbl.add hash owner prob
      end);
(* calculate the sum distribution by convolution *)
  let temp = envelope |> unique2 in
  temp |> List.iter ( fun (o1,(a,b)) ->
    let hash = st.prob_sum.(a).(b) in
    let in_range = 
      List.filter (fun (player,(x,y)) -> player <> o1 && distance2 bounds (a,b) (x,y) <= st.setup.attackradius2) envelope in
    match in_range with 
    |[] -> Hashtbl.add hash o1 [| 1.0 |]
    |_  ->
	List.iter (fun (o2,(x,y)) -> 
	  let probs = st.probabilities.(x).(y) in
	  if Hashtbl.mem probs o2 then 
	    begin
	      let random_var = Hashtbl.find probs o2 in
	      if not(Hashtbl.mem hash o1) then 
		begin
		  Hashtbl.add hash o1 (random_var);
		end
	      else 
		begin
		  let old_val = Hashtbl.find hash o1 in
		  Hashtbl.replace hash o1 (convolution old_val random_var);
		end
	    end) in_range)
    
let update_map my_ants enemy_ants gstate =
  List.iter (update_ant_vision gstate) my_ants;
  calculate_occupancy_probabilities gstate (my_ants @ enemy_ants)

(* How many milliseconds remain? *)
let time_remaining state = 
   let turn_time = if state.turn = 0 then (float_of_int state.setup.loadtime)
   else (float_of_int state.setup.turntime) in
      1000. *. 
      ((turn_time /. 1000.) -. ((get_time ()) -. state.go_time))

(* End helper functions *)

(* swrap wraps the game state. One of these is passed to the AI. *)

class swrap state =
 object (self)
   val mutable state = state
   method bounds = state.setup.rows, state.setup.cols
   method issue_order (o:order) = issue_order o
   method finish_turn () = finish_turn ()
   method direction p1 p2 = ((direction self#bounds p1 p2): (dir * dir))
   method step_dir loc (d:dir) = step_dir d self#bounds loc
   method get_tile loc = ((get_tile state.tmap loc): tile)
   method distance2 p1 p2 = distance2 self#bounds p1 p2
   method distance_and_direction p1 p2 =
      ((distance_and_direction self#bounds p1 p2): ((dir * dir) * float))
   method update_map = 
     update_map (Hashtbl.fold ( fun k v acc -> v :: acc ) state.my_ants []) (Hashtbl.fold ( fun k v acc -> v :: acc ) state.enemy_ants []) state
   method time_remaining = time_remaining state
   method set_state s = state <- s
   method get_state = state
   method turn = state.turn
   method my_ants = state.my_ants
   method enemy_ants = state.enemy_ants
   method my_hills = state.my_hills
   method enemy_hills = state.enemy_hills
   method get_map = state.tmap
   method get_player_seed = state.setup.player_seed
 end

let loop engine =
  let proto_setup =
     {
      loadtime = -1;
      turntime = -1;
      rows = -1;
      cols = -1;
      turns = -1;
      viewradius2 = -1;
      attackradius2 = -1;
      spawnradius2 = -1;
      player_seed = 932463947;
     }
  in
  let proto_gstate =
     {
      setup = proto_setup;
      turn = 0;
      my_ants = Hashtbl.create 1000;
      my_ants_number = 0 ;
      enemy_ants = Hashtbl.create 500;
      enemy_ants_number = 0;
      my_hills = [];
      enemy_hills = [];
      dead_ants = [];
      food = Hashtbl.create 100;
      tmap = Array.make_matrix 1 1 proto_tile; 
      agents = Array.make_matrix 1 1 (Array.make 3 0.0);
      attack = Array.make_matrix 1 1 (Hashtbl.create 1); 
      probabilities = Array.make_matrix 1 1 (Hashtbl.create 1); 
      prob_sum = Array.make_matrix 1 1 (Hashtbl.create 1); 
      my_hills_essence = Array.make_matrix 1 1 (Hashtbl.create 1); 
      go_time = 0.0;
      reset_enemy_hills = false;
      view_area = [];
     }
  in
  let wrap = new swrap proto_gstate in
  let rec take_turn i gstate =
    match read gstate with
    | Some state ->
        begin try 
         (
          wrap#set_state state;
          engine wrap; (*pass time*)
          flush stdout;
         )
        with exc ->
         (
          ddebug (Printf.sprintf 
             "Exception in turn %d :\n" i);
          ddebug (Printexc.to_string exc);
          ddebug "\n";
          ddebug (Printexc.get_backtrace());
          raise exc
         )
        end;
        take_turn (i + 1) wrap#get_state
    | None ->
        ()
  in
     Printexc.record_backtrace(true);
     take_turn 0 proto_gstate
;;
