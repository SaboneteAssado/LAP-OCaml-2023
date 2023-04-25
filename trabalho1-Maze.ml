type room = int
type rooms = room list

type path = room list
type island = room list

type passage = room * room
type passages = passage list

type maze = {
  rooms: rooms;
  entrances: rooms;
  exits: rooms;
  passages: passages
}


    (* [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13]; *)
let myMaze = {
  rooms = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13];
  entrances = [1;4;11];
  exits = [6;12];
  passages = [(1,2);(1,4);(1,5);(2,5);(3,6);(4,5);(4,7);(5,6);(5,8);(6,9);(7,8);(8,9);(10,11);(11,12)]
} 

let simpleMaze = {
  rooms = [1;2;3;4;5];
  entrances = [1];
  exits = [5];
  passages = [(1,2);(2,3);(4,5)]
}  


let setMake l = List.sort_uniq compare l;;

    (* reachable, islands, shortest 1 e 2, paths e hasLoop  *) 

    (* 
reachable : maze -> rooms
(* pre: isValid m, not (hasLoop m) *)
(* post: isCanonical result *)
let reachable m = ... 
Given a maze m, determine all the rooms that are reachable (possibly in several steps) from the entrance rooms. Note the entrance rooms belong to the result because they are reachable from themselves.
# reachable myMaze;;
- : room list = [1; 2; 4; 5; 6; 7; 8; 9; 11; 12]
*)

let rec exploreAux passages x =
  match passages with
  | [] -> []
  | (a,b)::xs -> 
      if a = x then
        b::exploreAux xs x
      else
        exploreAux xs x
      
let rec explore passages rooms =
  match rooms with
  | [] -> []
  | x::xs -> 
      exploreAux passages x @ explore passages xs

let rec getReachable m sol =
  let newSol = setMake (explore m.passages sol @ sol) in
  if sol = newSol then
    sol
  else
    getReachable m newSol

let reachable m = getReachable m m.entrances ;;
    
reachable myMaze;;

(*
  islands : maze -> island list
(* pre: isValid m, not (hasLoop m) *)
(* post: isCanonical result *)
  let islands m = ... 
    Given a maze m, determine all its islands (i.e. connected components). A island I is a maximal subset of the rooms such that: if a room r is adjacent to some element of I, then r also belongs to I. For example, there are three islands in our main example.
                                                                                                                                                                                                                                                             # islands myMaze;;
- : room list list = [[1; 2; 3; 4; 5; 6; 7; 8; 9]; [10; 11; 12]; [13]]
*)

let rec shouldCouple island1 island2 =
  match island1 with
  | [] -> false
  | x::xs -> 
      if List.mem x island2 then
        true
      else
        shouldCouple xs island2
          
let rec couple island islands hasAdded = 
  match islands with
  | [] -> 
      if hasAdded then
        []
      else
        [island]
  | x::xs ->
      if shouldCouple island x then
        setMake (island@x)::couple island xs true
      else
        x::couple island xs hasAdded

let rec getIslands passages =
  match passages with
  | [(a,b)] -> [[a;b]]
  | (a,b)::xs ->
      couple [a;b] (getIslands xs) false

let rec belongsIslands room islands =
  match islands with
  | [] -> false
  | x::xs ->
      if List.mem room x then
        true
      else 
        belongsIslands room xs

let rec completeMissingIslands rooms islands =
  match rooms with 
  | [] -> islands
  | x::xs ->
      if belongsIslands x islands then
        completeMissingIslands xs islands
      else
        [x]::completeMissingIslands xs islands

let islands m = 
  if m.passages = [] then
    []
  else
    let newIslands = setMake (getIslands m.passages) in
    setMake (completeMissingIslands m.rooms newIslands)
;;
  
islands myMaze;;

  (*
    shortest : maze -> path
(* pre: isValid m, not (hasLoop m) *)
(* post: none *)
    let shortest m = ... 
      Given a maze m, find the shortest path from an entrance room to an exit room. Is there are several tied best paths, return any of them. If there is no path connecting an entrance room to an exit room, this situation is represented by returning the empty list (the constant _NO_PATH).
                                                                                                                                                                                                                                                                           # shortest myMaze;;
- : room list = [11; 12]
*) 

let rec buildNewPaths rooms path =   
  match rooms with
  | [] -> []
  | x::xs ->
      (x::path)::buildNewPaths xs path

let rec explorePaths passages paths =
  match paths with
  | [] -> []
  | x::xs ->
      let newRooms = explore passages [(List.hd x)] in 
      buildNewPaths newRooms x@explorePaths passages xs

let rec initialPaths entrances =
  match entrances with
  | [] -> []
  | x::xs ->
      [x]::initialPaths xs

let rec pathWithExit exits paths =
  match paths with
  | [] -> []
  | x::xs ->
      if List.mem (List.hd x) exits then
        x
      else
        pathWithExit exits xs

let rec getShortest passages exits paths =
  let newPaths = explorePaths passages paths  in 
  if newPaths = [] then
    []
  else (
    let shortest = pathWithExit exits newPaths in
    if shortest != [] then
      List.rev shortest
    else
      getShortest passages exits newPaths
  )

let shortest m = getShortest m.passages m.exits (initialPaths m.entrances);;

shortest myMaze;;

  (*

    paths : maze -> path list
(* pre: isValid m, not (hasLoop m) *)
(* post: none *)
    let paths m = ... 
      Given a maze m, find all the paths starting at an entrance room and stopping at: (1) an exit room, or (2) a leaf room.
                                                                                                                         # paths myMaze;;
    - : room list list =
    [[1; 2; 5; 6]; [1; 2; 5; 6; 9]; [1; 2; 5; 8; 9]; [1; 4; 5; 6];
     [1; 4; 5; 6; 9]; [1; 4; 5; 8; 9]; [1; 4; 7; 8; 9]; [1; 5; 6]; [1; 5; 6; 9];
     [1; 5; 8; 9]; [4; 5; 6]; [4; 5; 6; 9]; [4; 5; 8; 9]; [4; 7; 8; 9]; [11; 12]]
*)

let rec pathsWithExit exits newPaths =
  match newPaths with
  | [] -> []
  | x::xs ->
      pathWithExit exits [x]:: pathsWithExit exits xs
    
let rec revInvertedPaths paths =
  match paths with
  | [] -> []
  | x::xs ->
      List.rev x :: revInvertedPaths xs
  
let rec getlostPathsAux passages path =
  match passages with
  | [] -> [path]
  | (a,b)::xs -> 
      if a = List.hd path then
        []
      else
        getlostPathsAux xs path
      
let rec getLostPaths passages paths =
  match paths with
  | [] -> []
  | x::xs -> 
      getlostPathsAux passages x @ getLostPaths passages xs  
                

let rec getPaths passages exits paths sols =
  let lostPaths = (getLostPaths passages paths) in
  let newPaths = explorePaths passages paths in 
  let newSols = List.filter (fun x -> x <> []) (pathsWithExit exits newPaths) in
  if newPaths = [] then
    setMake(revInvertedPaths (sols @ paths))
  else
    getPaths passages exits newPaths (lostPaths @ newSols  @ sols)

let paths m = getPaths m.passages m.exits (initialPaths m.entrances) [] ;;
paths myMaze;;

  (*
    hasLoop : maze -> bool
(* pre: isValid m *)
(* post: none *)
    let hasLoop m = ... 
      Given a maze m, check if it contain any loop. A loop is a path linking a room to itself, passing through one or more intermediate nodes. [Actually, the standard name for this concept is cycle, but now is too late to fix this.]
                                                                                                                                        # hasLoop myMaze;;
- : bool = false

*) 
          
let rec existsRepeatedElemAux path =
  match path with
  | [] -> false
  | x::xs ->
      if List.mem x xs then
        true
      else
        existsRepeatedElemAux xs

let rec existsRepeatedElem paths =
  match paths with
  | [] -> false
  | x::xs ->
      if existsRepeatedElemAux x then
        true
      else
        existsRepeatedElem xs

let rec getHasLoop passages paths = 
  let newPaths = explorePaths passages paths  in 
  if newPaths = [] then
    false
  else if existsRepeatedElem newPaths then
    true
  else
    getHasLoop passages newPaths
  
let hasLoop m = getHasLoop m.passages (initialPaths m.entrances);;
hasLoop myMaze;;

  (*
    shortest2 : maze -> path
(* pre: isValid m *)
(* post: none *)
    let shortest2 m = ... 
      Given a maze m that can contain loops, find the shortest path from an entrance room to an exit room. Is there are several tied best paths, return any of them. If there is no path connecting an entrance room to an exit room, this situation is represented by returning the empty list (the constant _NO_PATH).
                                                                                                                                                                                                                                                                                                  # shortest2 myMaze;;
- : room list = [11; 12]
                *) 

let rec buildNewPaths rooms path =   
  match rooms with
  | [] -> []
  | x::xs ->
      (x::path)::buildNewPaths xs path

let rec explorePaths passages paths =
  match paths with
  | [] -> []
  | x::xs ->
      let newRooms = explore passages [(List.hd x)] in 
      buildNewPaths newRooms x@explorePaths passages xs

let rec initialPaths entrances =
  match entrances with
  | [] -> []
  | x::xs ->
      [x]::initialPaths xs

let rec pathWithExit exits paths =
  match paths with
  | [] -> []
  | x::xs ->
      if List.mem (List.hd x) exits then
        x
      else
        pathWithExit exits xs

let rec getShortest2 passages exits paths =
  let newPaths = List.filter (fun x -> Bool.not (existsRepeatedElemAux x)) (explorePaths passages paths)  in 
  if newPaths = [] then
    []
  else (
    let shortest = pathWithExit exits newPaths in
    if shortest <> [] then
      List.rev shortest
    else
      getShortest2 passages exits newPaths
  )

let shortest2 m = getShortest2 m.passages m.exits (initialPaths m.entrances);;
shortest2 myMaze;;