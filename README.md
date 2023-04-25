# LAP-OCaml-2023

Finished P1 from lap 2023. Should replace some functions with the given functions and structs.

EX:

Linguagens e Ambientes de Programação (2021/2022) [Eng.Inf. - DI/FCT/UNL]
Enunciado do 1º Projeto Prático (OCaml)
Artur Miguel Dias

A-maze-ing mazes

This project is about mazes (labyrinths) where we want to find a way from an entrance to an exit. And if we can find the shortest path, that would be ideal.
There is the graphical representation of one maze on the right. It contains several rooms, represented by circles, and several oriented passages, represented by arrows. The blue rooms are the entrances and the green rooms the exits.

This maze does not contain loops. When dealing with maze with loops, the infinite paths become a concern.

This project is mostly about mazes with no loops. But also includes, at the end, a little taste of mazes with loops.

Module "Maze"
The aim of this project is to write a closed OCaml module named "Maze" containing data representations and functions concerning the notion of maze.
The module interface has already been fully written and you are not allowed to change it: Maze.mli. As you can see, the data representation is public and there is also a small number of public functions declared. All the other entities you might define in the module body will become private. The representation is public to allow Mooshak to check the code of your module.

Use this file as a starting point to write your module body: Maze.ml.

Representation of the mazes
In this project, we will represent the mazes, and associated concepts, using the following OCaml types:

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
Each room is represented by a non-negative integer and each passage is represented by a pair of two distinct rooms (the two rooms that passage connects).
Each maze if characterized by:

A non-empty list of rooms.
A non-empty list of entrances.
A list of exits, that can be empty.
A list of passages, that can be empty.
For a maze to be valid, obviously all the rooms referred in "entrances", "exits" and "passages" must be previously declared in "rooms". Another restriction is that a room cannot be at the same time an entrance and an exit.
Furthermore, all the lists must be in the so called canonical form, that is they must be sorted and do not include any duplicated elements.

Here is the representation of our main example:

let myMaze = {
    rooms = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13];
    entrances = [1;4;11];
    exits = [6;12];
    passages = [(1,2);(1,4);(1,5);(2,5);(3,6);(4,5);(4,7);(5,6);(5,8);(6,9);(7,8);(8,9);(10,11);(11,12)]
}
Inductive method over mazes
Note that in a maze without loops, the part of the maze starting at some room n is actually a n-ary tree. The room n itself is the root of the said tree and we can get the children of n from the maze using an auxiliary function next:
next : maze -> room -> rooms
To exemplify, there is a function that determines, in a maze m without loops, the maximum distance from the room n to a leaf room. A leaf room is any room without any way-out passage.
Note the use of the usual auxiliary function that processes the children.

let rec depth m r =
    let rs = next m r in
        if rs = [] then
            0
        else
            1 + ldepth m rs
and ldepth m rs =
    match rs with
    | [] -> failwith "ldepth"
    | [r] -> depth m r
    | r::rs -> max (depth m r) (ldepth m rs)
It is easy to describe this function. If the room is a leaf room, then the result is 0. Otherwise, the result is 1 plus the maximum depth of the various children.
In any case, note that some of the requested functions do not need to analyze mazes structurally, and they should be programmed using different styles.

Another observation. Some of the requested functions that analyze mazes structurally, happen to build and return a list. In most of these cases, it is more compact and legible (so, a better style) to replace the auxiliary function by a direct call to the provided function flatMap. This function flatMap also has the extra advantage of keeping all the lists sorted and without repetition, as required in the description of many of the requested functions.

The public functions of the module
There are 14 public functions to implement. Please, write the functions in good functional style, avoiding any kind of code based on imperative reasoning. Anyway, note that within the functional style, there are several possibilities you might want to consider, depending on the problem at hand. The possibilities include: direct use of the inductive method; decomposition into simpler functions; taking advantage of the offered functions over sorted list with no repetitions; a mixture of all these.
In the descriptions below, the preconditions of each function are clearly stated. Please, do not check the preconditions in your code; simply assume them, as usual.

The task of each function is to calculate the result according to the specification and also to ensure the stated postconditions.

It is provided a simple example for each function.

isValid : maze -> bool
(* pre: none *)
(* post: none *)
let isValid m = ... 
Check if the maze m is valid.
# isValid myMaze;;
- : bool = true
makeLineMaze : room -> room -> maze
(* pre: a < b *)
(* post: isValid result *)
let makeLineMaze a b = ... 
Build a maze with the following characteristics: The rooms are those in the succession a, a+1, a+2, ..., b. There is only one entrance room: a. There is only one exit room: b. For each pair of consecutive rooms x and x+1, there is the passage (x, x+1).
# makeLineMaze 1 9;;
- : maze =
{ rooms = [1; 2; 3; 4; 5; 6; 7; 8; 9];
  entrances = [1];
  exits = [9];
  passages = [(1, 2); (2, 3); (3, 4); (4, 5); (5, 6); (6, 7); (7, 8); (8, 9)]
}
combine : maze -> maze -> maze
(* pre: isValid m1, isValid m2 *)
(* post: isValid result *)
let combine m1 m2 = ... 
Combines the two mazes m1 and m2 into a single maze by doing this: join the rooms, join the entrances, join the exits, join the passages. After that, remove from the exits the rooms that might be already in the entrances, to ensure that the new maze is valid.
# combine myMaze myMaze;;
- : maze =
{ rooms = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13];
  entrances = [1; 4; 11];
  exits = [6; 12];
  passages = [(1, 2); (1, 4); (1, 5); (2, 5); (3, 6); (4, 5); (4, 7); (5, 6);
              (5, 8); (6, 9); (7, 8); (8, 9); (10, 11); (11, 12)]
}

next : maze -> room -> rooms
(* pre: isValid m *)
(* post: isCanonical result *)
let next m r = ... 
Given a maze m, gather all the rooms that can be reach from the room r in one single step. The function will be essential for inductive reasoning over mazes.
# next myMaze 5;;
- : room list = [6; 8]
next2 : next2 : maze -> rooms -> rooms
(* pre: isValid m *)
(* post: isCanonical result *)
let next2 m rs = ... 
Given a maze m, apply next to all rooms in the list rs and collect all the partial results in a single list.
# next2 myMaze [5; 11; 13];;
- : room list = [6; 8; 12]
prev : maze -> room -> rooms
(* pre: isValid m *)
(* post: isCanonical result *)
let prev m r = ... 
Given a maze m, gather all the rooms from which the room r can be reach in one single step.
# prev myMaze 5;;
- : room list = [1; 2; 4]
adjacent : maze -> room -> rooms
(* pre: isValid m *)
(* post: isCanonical result *)
let adjacent m r = ... 
Given a maze m, joins the results of next and prev applied to the room r.
# adjacent myMaze 5;;
- : room list = [1; 2; 4; 6; 8]
reachable : maze -> rooms
(* pre: isValid m, not (hasLoop m) *)
(* post: isCanonical result *)
let reachable m = ... 
Given a maze m, determine all the rooms that are reachable (possibly in several steps) from the entrance rooms. Note the entrance rooms belong to the result because they are reachable from themselves.
# reachable myMaze;;
- : room list = [1; 2; 4; 5; 6; 7; 8; 9; 11; 12]
solitary : maze -> rooms
(* pre: isValid m *)
(* post: isCanonical result *)
let solitary m = ... 
Given a maze m, determine all the rooms that are completely disconnected from the other rooms, with zero adjacent rooms.
# solitary myMaze;;
- : room list = [13]
islands : maze -> island list
(* pre: isValid m, not (hasLoop m) *)
(* post: isCanonical result *)
let islands m = ... 
Given a maze m, determine all its islands (i.e. connected components). A island I is a maximal subset of the rooms such that: if a room r is adjacent to some element of I, then r also belongs to I. For example, there are three islands in our main example.
# islands myMaze;;
- : room list list = [[1; 2; 3; 4; 5; 6; 7; 8; 9]; [10; 11; 12]; [13]]
shortest : maze -> path
(* pre: isValid m, not (hasLoop m) *)
(* post: none *)
let shortest m = ... 
Given a maze m, find the shortest path from an entrance room to an exit room. Is there are several tied best paths, return any of them. If there is no path connecting an entrance room to an exit room, this situation is represented by returning the empty list (the constant _NO_PATH).
# shortest myMaze;;
- : room list = [11; 12]
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
hasLoop : maze -> bool
(* pre: isValid m *)
(* post: none *)
let hasLoop m = ... 
Given a maze m, check if it contain any loop. A loop is a path linking a room to itself, passing through one or more intermediate nodes. [Actually, the standard name for this concept is cycle, but now is too late to fix this.]
# hasLoop myMaze;;
- : bool = false
shortest2 : maze -> path
(* pre: isValid m *)
(* post: none *)
let shortest2 m = ... 
Given a maze m that can contain loops, find the shortest path from an entrance room to an exit room. Is there are several tied best paths, return any of them. If there is no path connecting an entrance room to an exit room, this situation is represented by returning the empty list (the constant _NO_PATH).
# shortest2 myMaze;;
- : room list = [11; 12]
Evaluation and grades
You will submit the file "Maze.ml" via Mooshak.
Around 80% of the grade of your group is automatically assigned by Mooshak. The remaining 20% is assigned manually by the teachers, who will analyze the quality of your code.

A special case: In case of code of extremely bad quality, or code that uses the forbidden imperative mechanisms of OCaml, or code that constantly simulates imperative mechanisms and concepts, a special rule will be used so that the grade will be always below 50%, even if the program works well.

To develop the project, we strongly recommend you use the OCaml interpreter, probably inside the Eclipse IDE.

However, you should know that Mooshak will compile your module using the following command:

ocamlc -c Maze.mli Maze.ml
After the compilation, Mooshak will test the module in the interpreter like this:
$ ocaml
    Objective Caml version 4.02.3
# #load "Maze.cmo";;
# open Maze;;
...
...
Please, make a backup of the file "Maze.ml" once a while, because the file can disappear as result of human error or as result of a software/hardware malfunction.
