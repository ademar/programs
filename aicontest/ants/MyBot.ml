open Ants;;

let _explore = 0
let _enemy_hill = 1
let _enemy_ant = 2

let max_explore    =   10000000.0
let max_enemy_hill =   10000000.0
let max_enemy_ant  =     200000.0
let max_ally_hill  =   10000000.0
let max_ally_ant   =   10000000.0

let beta  = [| 0.1; 1.08; 1.1 |](*how ally ants diffuse other agents*)

let directions_array = [| `N; `E; `S; `W ; `Stop |]
let directions_list  = [ `N; `E; `S; `W ; `Stop ]

let random_sort arr =
  let n = ref (Array.length arr) in
  while !n>1 do
    let k = Random.int !n in
    n := !n -1 ;
    let temp = arr.(!n) in
    arr.(!n) <- arr.(k);
    arr.(k) <- temp
  done

let neighbours p st = 
  Array.map (fun x -> st#step_dir p x) [| `N; `E; `S; `W |]

(* diffuse map *)

let diffuse st state (r,c as p) agent = 
  let neighbours = neighbours p st in
  let sum = (Array.map (fun (row,col) -> state.agents.(row).(col).(agent)) neighbours) |> sum_array in
  let cell = state.tmap.(r).(c) in
  if cell.whatson = `Ally then beta.(agent) *. 0.25 *. sum 
  else 0.25 *. sum 

let hill_essence' st hill (a,b) = 
  let hash = st.my_hills_essence.(a).(b) in 
  if Hashtbl.mem hash hill then 
    Hashtbl.find hash hill 
  else 
    0.0

let hill_essence st hill ((a,b),_) = 
  hill_essence' st hill (a,b)

let diffuse_hill (hill,_) ((r,c) as pos)  state st = 
  if hill = pos then
     Hashtbl.replace st.my_hills_essence.(r).(c) hill max_ally_hill
  else
    begin
      let neighbours = neighbours pos state in
      let d = (Array.map (hill_essence' st hill) neighbours) |> sum_array in
      Hashtbl.replace st.my_hills_essence.(r).(c) hill (d *. 0.25) 
    end

let diffuse_hills state st pos = 
   let rec loop hills =
     match hills with
     | [] -> ()
     | p::tail -> diffuse_hill p pos state st;
                  loop tail
   in loop st.my_hills

let is_frontier st state (r,c) =
  let cell = state.tmap.(r).(c) in
  if cell.whatis = `Unseen then false
  else 
     List.exists (fun x -> 
                     let (row,col) = st#step_dir (r,c) x in 
                     let new_spot = state.tmap.(row).(col) in 
                     new_spot.whatis = `Unseen) [ `N; `E; `S; `W ]

let diffuse_point st state ((r,c) as p)  =
   let cell = state.tmap.(r).(c) in
   if not(cell.whatis = `Unseen || cell.whatis = `Water) then
   begin
     if is_frontier st state p then 
	 state.agents.(r).(c).(0) <- 2.0 *. max_explore
     else if not(cell.visible) then
	 state.agents.(r).(c).(0) <- float(state.turn - cell.seen)
     else 
	 state.agents.(r).(c).(0) <- diffuse st state p 0;
     if cell.whatis = `EnemyHill then
       state.agents.(r).(c).(1) <- max_enemy_hill
     else 
       state.agents.(r).(c).(1) <- diffuse st state p 1;
     if cell.whatson = `Enemy then
       state.agents.(r).(c).(2) <- max_enemy_ant
     else 
       state.agents.(r).(c).(2) <- diffuse st state p 2;
     diffuse_hills st state p
     end

let diffuse_map state =
   let st = state#get_state in
   let s = st.setup in
   let nrows = s.rows - 1 in
   let ncols = s.cols - 1 in
      for r = 0 to nrows do
         for c = 0 to ncols do
	   diffuse_point state st (r,c)
         done
      done;
      for r = nrows downto 0 do
         for c = 0 to ncols do
	   diffuse_point state st (r,c)
         done
      done;
      for r = 0 to nrows do
         for c = ncols downto 0 do
	   diffuse_point state st (r,c)
         done
      done;
      for r = nrows downto 0 do
         for c = ncols downto 0 do
	   diffuse_point state st (r,c)
         done
      done

let in_range state p q =
  let st = state#get_state in 
  let r2 = st.setup.attackradius2 in
  (state#distance2 p q) < r2

let expected_value arr =
  let vals = Array.mapi (fun i a -> (i,a)) arr in 
  Array.fold_left (fun acc (i,x) -> acc +. float(i) *. x ) 0.0 vals 

let eval_expectation state p player =
  let st = state#get_state in
  let (r,c) = p in
  let hash = st.prob_sum.(r).(c) in
  if Hashtbl.mem hash player then 
    expected_value (Hashtbl.find hash player)
  else
    0.0

let select_enemy_with_min_enemies state enemies_in_range =  
 let _,p = 
   enemies_in_range 
   |> maxBy ( fun (x,(p,q)) -> 
       let enemy_force = eval_expectation state (p,q) x in
       let v = -1.0 *. (enemy_force) in 
        v) 
 in -1.0 *. p

let eval_best_enemy state enemies (player,(r,c)) =
  let st = state#get_state in 
  let enemies_in_range =  
    enemies 
    |> List.filter (fun (x,(a,b)) -> x<>player &&  state#distance2 (a,b) (r,c) <= st.setup.attackradius2) in
  let best_value = 
    match enemies_in_range with
    | [] -> 1000.0 (* big number here *)
    | _  -> select_enemy_with_min_enemies state enemies_in_range 
  in  best_value

let negate p = Array.map (fun x -> 1.0 -. x) p

let integral p = 
  Array.mapi ( fun i _ -> Array.fold_left ( fun acc y -> acc +. y) 0.0 (Array.sub p 0 (i+1)) ) p

let normalize p i =
  if i >= (Array.length p) then 1.0 
  else p.(i)

let max a b = if a > b then a else b

let rec range m n =
  if m > n then [] 
  else m::(range (m+1) n)

let is_safe p state envelope =
  let expectation = eval_expectation state p 0 in 
  let best_enemy = eval_best_enemy state envelope (0,p) in
  expectation <= best_enemy

let agent_essence st agent ((row,col),_) = 
  st.agents.(row).(col).(agent)

let submit_order state (r,c) (a,b) move  =
  let st = state#get_state in 
  state#issue_order ((r,c), move);
  st.tmap.(r).(c) <- { st.tmap.(r).(c) with taken = false; };
  st.tmap.(a).(b) <- { st.tmap.(a).(b) with taken = true; };
  if st.tmap.(a).(b).whatis = `EnemyHill then 
    st.reset_enemy_hills <- true

let proceed state st (r,c) neighbours func = 
  let ((a,b),best_move),best_value = maxBy func neighbours in
  if best_value > 0.0 then
    submit_order state (r,c) (a,b) best_move

let compare_hills st x y pos = 
  let fx = hill_essence' st x pos in 
  let fy = hill_essence' st y pos in 
  if fx = fx then 0 
  else 
    if fx > fy then -1 else 1

let step_ant state ant hills_under_siege all_ants envelope =
   let (r,c) = ant.loc in
   let st = state#get_state in
   let explore = st.agents.(r).(c).(_explore) in
   let enemy_hill = st.agents.(r).(c).(_enemy_hill) in
   let enemy_ant = st.agents.(r).(c).(_enemy_ant) in
   random_sort directions_array; 
   let neighbours = 
     Array.map (fun x -> state#step_dir (ant.loc) x, x) directions_array
     |> Array.to_list
     |> List.filter ( fun ((row,col),_) ->  let cell = st.tmap.(row).(col) in not(cell.taken || cell.whatson = `Ally || cell.whatis = `AllyHill || cell.whatis = `Water || cell.whatson = `Food) ) in 
   match neighbours with
   | [] -> ()
   | _  ->
       	 match hills_under_siege with
	 | [] ->
	     begin
             let candidate = 
       	       if enemy_hill > 1.0 then
		   _enemy_hill 
               else 
		 if (explore > 1.0) then _explore
		 else
		   if(enemy_ant > 1.0) then _enemy_ant
		       else _explore
	     in
	     let filtered = (List.filter ( fun ((x,y),_)->  is_safe (x,y) state envelope) neighbours) in 
	     match filtered with 
	      | [] -> proceed state st (r,c) neighbours (agent_essence st candidate)
	      | _  -> proceed state st (r,c) filtered (agent_essence st candidate)
	     end
         | _ -> (* our hills are threaten*)
            let ordered =   List.sort (fun x -> compare_hills st x (r,c)) hills_under_siege  in
            let closest_hill = List.hd ordered in
	    if enemy_ant > 1.0 then 
		proceed state st (r,c) neighbours (agent_essence st _enemy_ant)
	    else
	      (* go towards the hill *)
		proceed state st (r,c) neighbours (hill_essence st closest_hill)
      
let rec step_ants state my_l all_ants envelope =
   let st = state#get_state in
   let hills_under_siege = 
     List.filter (fun ((a,b),_) -> st.agents.(a).(b).(_enemy_ant) > 1.0 ) st.my_hills 
	 |> List.map (fun (a,_) -> a) in

   let rec loop queue = 
     match queue with 
     | [] -> ()
    | _ when state#time_remaining < 50.0 -> ()
    | head :: tail ->
	let ants = st.my_ants in
	let ant = Hashtbl.find ants (head.loc) in
         if not(ant.busy) then 
	   step_ant state ant (List.filter (fun c -> hill_essence' st c (head.loc) > 1.0) hills_under_siege) all_ants envelope;

         loop tail
   in loop my_l

let reverse d = 
  match d with 
  |`N -> `S
  |`S -> `N
  |`W -> `E
  |`E -> `W
  |`Stop -> `Stop

let discover state ant all_ants envelope =
 let (r,c) = ant.loc in
 let visited = Hashtbl.create 100 in
 let st = state#get_state in
 let already s = Hashtbl.mem visited s in
 let neightbours (p,q) d n = 
   List.map (fun x -> ((state#step_dir (p,q) x),d,n)) [ `N;`S;`E;`W ]
  |> List.filter (fun ((a,b) as x,_,_) -> let dist = state#distance2 x (r,c) in (dist  <= st.setup.viewradius2) && not (already x) && st.tmap.(a).(b).whatis <> `Unseen && st.tmap.(a).(b).whatis <> `Water) in
 let rec loop queue = 
   match queue with 
   |[] -> None
   |((a,b),d,n)::tail -> 
       if already (a,b) then loop tail 
       else 
	 begin
	   Hashtbl.add visited (a,b) ();
	   let cell = st.tmap.(a).(b) in 
	   if cell.whatson = `Food then 
	     Some({typ = `Food; direction = d; steps = n; target = (a,b)})
	   else
	     begin
	       let neight = neightbours (a,b) d (n+1) in
	       loop (tail @ neight)
	     end
	 end
 in
let cand = 
  (List.map (fun x -> ((state#step_dir (ant.loc) x),x,1)) [`N;`S;`E;`W] )
  |> List.filter (fun ((a,b),_,_) -> st.tmap.(a).(b).whatis <> `Water && is_safe (a,b) state envelope) in
loop cand

let select_some2 l = List.fold_right ( fun o xs -> match o with 
|a,Some x -> (a,x)::xs
|_,None -> xs) l []

let scout_food state my_ants all_ants envelope =
  let st = state#get_state in 
  let sorted_events =
    List.filter (fun x -> x.food_a_la_vista ) my_ants
    |> List.map (fun x -> x,discover state x all_ants envelope)
    |> select_some2
    |> List.sort (fun (_,x) (_,y)-> compare x.steps y.steps) in
  let rec loop queue =
    match queue with 
    |[] -> ()
    |(a,ev)::tail -> 
	let ants = st.my_ants in
	let ant = Hashtbl.find ants (a.loc) in 
	let food = Hashtbl.find st.food ev.target in 
	if not(ant.busy) && not(food.collected) then 
	  begin
	    (*go and get the food*)
	    Hashtbl.replace ants a.loc { ant with busy = true };
	    Hashtbl.replace st.food ev.target { food with collected = true };
	    let new_loc = state#step_dir a.loc (ev.direction) in 
	    submit_order state a.loc new_loc (ev.direction)
	  end;
	loop tail
  in loop sorted_events
    
(* The bot checks whether it's Turn 0 (setting up turn, no orders 
allowed) and finishes the turn immediately if it is; otherwise it calls 
step_ants. *)

(* The update_vision function is optional; with a lot of ants, it could 
chew up a fair bit of processor time. If you don't call it, the visible 
function won't work. *)

let mybot_engine state =
   if state#turn = 0 then state#finish_turn ()
   else
    (
     state#update_map;
     diffuse_map state;
     diffuse_map state;
     let st = state#get_state in 
     let ants = st.my_ants in
     let my_ants = Hashtbl.fold ( fun x y acc -> y::acc ) ants [] in
     let enemy_ants = Hashtbl.fold ( fun x y acc -> y::acc ) st.enemy_ants [] in
     let all_ants = my_ants @ enemy_ants in
     let envelope = 
       all_ants
       |> List.map (fun x -> List.map (fun z -> x.owner, state#step_dir x.loc z) [`N;`S;`E;`W;`Stop])
       |> List.flatten 
       |> List.filter (fun (_,(a,b)) -> (st.tmap.(a).(b).whatis <> `Water (* BUG: missing && <>`Food *))) in 
     scout_food state my_ants all_ants envelope;
     step_ants state my_ants all_ants envelope;
     state#finish_turn ()
    )
;;

loop mybot_engine;;
