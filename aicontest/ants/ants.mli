val ddebug : string -> unit

val sum_list : float list -> float
val sum_array : float array -> float
val (|>) : 'a -> ('a -> 'b) -> 'b


type game_setup = {
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

type dir = [ `N | `E | `S | `W | `Stop  ];;
type tile = [ `Water | `Land | `AllyHill | `EnemyHill | `Unseen];;
type ocupator = [ `Food | `Ally | `Enemy | `DeadAlly | `DeadEnemy | `Nothing | `Unknown];;

type mapb = { 
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
    food_a_la_vista: bool;
  }

type food = {loc1:(int*int); collected : bool}

type random_var = float array

type tgame_state = {
    setup : game_setup;
    turn : int;
    my_ants : ((int*int),ant) Hashtbl.t;
    mutable my_ants_number : int;
    enemy_ants : ((int*int),ant) Hashtbl.t;
    mutable enemy_ants_number : int;
    my_hills : ((int * int) * int) list;
    enemy_hills : ((int * int) * int) list;
    dead_ants : ant list;
    food : (int * int,food) Hashtbl.t;
    tmap : mapb array array;
    agents : float array array array;
    attack: (int,int) Hashtbl.t array array;
    probabilities: (int,random_var) Hashtbl.t array array;
    prob_sum: (int,random_var) Hashtbl.t array array;
    my_hills_essence: (int*int,float) Hashtbl.t array array;
    go_time : float;
    mutable reset_enemy_hills : bool;
    view_area : (int*int) list;
}

type order = (int * int) * dir

class swrap :
  tgame_state ->
  object
    val mutable state : tgame_state
    method bounds : int * int
    method direction : int * int -> int * int -> dir * dir
    method distance2 : int * int -> int * int -> int
    method distance_and_direction :
      int * int -> int * int -> (dir * dir) * float
    method finish_turn : unit -> unit
    method get_map : mapb array array
    method get_state : tgame_state
    method get_tile : int * int -> tile
    method issue_order : order -> unit
    method my_ants : (int*int,ant) Hashtbl.t
    method enemy_ants : (int*int,ant) Hashtbl.t
    method my_hills : ((int * int) * int) list
    method enemy_hills : ((int * int) * int) list
    method set_state : tgame_state -> unit
    method step_dir : int * int -> dir -> int * int
    method time_remaining : float
    method turn : int
    method update_map : unit
    method get_player_seed : int
  end

val loop : (swrap -> 'a) -> unit
val string_of_dir : dir -> string 
val int_of_tile : tile -> int
val wrap_bound : (int*int) -> (int*int) -> (int*int)
val maxBy : ('a -> 'b) -> 'a list -> ('a*'b)
val look_up : ('a, 'b) Hashtbl.t -> 'a -> 'b -> 'b

type event_type = [ `Food |`Enemy |`Explore];;

type event = { typ: event_type; direction: dir; steps: int; target: int*int}

val convolution : float array -> float array -> float array
