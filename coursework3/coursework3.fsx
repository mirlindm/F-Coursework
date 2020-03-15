(*

  ITT8060 -- Advanced Programming 2019
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 3: Discriminated unions, higher-order functions

  ------------------------------------
  Name:Mirlind Murati
  Tallinn University of Technology Student ID: 195329IVSM
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the https://gitlab.cs.ttu.ee
  repository itt8060-2019 under your name, into a file coursework3/coursework3.fsx.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.

  Your solution should not contain the keywords 'rec' and 'mutable'.

  Deadline for submission is Sunday, October 6.
*)


// A list can contain items of the same type.
// We use a Cell to collect things that can be slightly different.

type Cell = Empty | Value of int | Pair of (int * int)




// 1. Define a function
//
// noEmptyCells : Cell list -> Cell list
//
// that discards all cells that are Empty.
//
// Use List.filter

(*let isEmpty (cell: Cell): bool =
    if cell = Empty then true else false
    ;;

let noEmptyCells (x1: Cell list) : Cell list =
     List.filter (not << isEmpty) x1;;

noEmptyCells [Empty; Value 3; Pair(3,4)];;*)

let noEmptyCells (xs: Cell list) : Cell list =
    List.filter (function
    |Empty -> false
    | _ -> true
    ) xs;;


//let noEmptyCells2 (x1: Cell list) : Cell list =
    //List.filter(fun x -> if x = Empty then false else true) x1;;
noEmptyCells [Empty; Value 3; Pair(3,4)];;

// 2. Define a function
//
// filterGreaterThan : int -> Cell list -> Cell list
//
// that discards all cells with value less than or equal to n.
// This means that all Empty cells should be discarded,
// Value v should be discarded when v is less than or equal to n,
// Pair (x, y) should be discarded when both x and y are less than or equal to n.
//
// Use List.filter

(*let filterGreaterThan (n:int) (x1: Cell list) : Cell list =
    match x1 with 
    | [Empty] -> []
    | [Value v] -> if v <= n then [] else [Value v] 
    | [Pair(x,y)] -> if x <= n && y <= n then [] else [Pair(x,y)];;

   filterGreaterThan 0 [Empty; Value 2; Pair(2,0)];;*)

let filterGreaterThan (n: int) (x1: Cell list) : Cell list = 
    List.filter (function 
    | Empty       -> false
    | Value v     -> v > n
    | Pair (x, y) -> x > n || y > n
    ) x1;;

    filterGreaterThan 0 [Empty; Value 0; Value 2; Pair(0,1)];;

// 3. Define a function
//
// increaseCells : int -> Cell list -> Cell list
//
// that increases the values in the cells by the given amount.
// Empty should stay Empty and for Pairs you should increase
// both of the values in the pair.
//
// Use List.map

let increaseCells (n: int) (x1: Cell list) : Cell list = 
    List.map (function
     |Empty      -> Empty
     |Value v    -> Value (v + n)
     |Pair(x, y) -> Pair ((x+n), (y+n))   
     ) x1;;

    increaseCells 2 [Empty; Value 0; Value 2; Pair(0,1)];;


// 4. Define a function
//
// transformPairs : (int -> int -> Cell) -> Cell list -> Cell list
//
// that replaces the Pair cells in the list
// with the result of applying the given operation to the two integer values in the pair.
//
// 'transformPairs f xs' should replace a 'Pair (x, y)' with 'f x y'.
//
// Use List.map
let transformPairs (f: int -> int -> Cell) (xs: Cell list) : Cell list =
    List.map (fun a -> 
    match a with
    |Pair (x, y) -> f x y 
    |_ -> a
    ) xs;;

    //transformPairs (fun x y -> Value (x + y)) [Empty; Value 0; Value 2; Pair(3,2)];;

    List.collect(fun x -> [x*2]) [2;3;4;5];;
    //List.reduce(fun x -> x + 3) [2;3];;

// 5. Define a function
//
// pairsToEmpty : Cell list -> Cell list
//
// that replaces all Pairs with Empty cells.
    
let pairsToEmpty (x1: Cell list) : Cell list =
    List.map(function
    |Empty -> Empty
    |Value v -> Value v
    |Pair (x,y) -> Empty 
    ) x1;;

    pairsToEmpty [Empty; Value 0; Value 2; Pair(0,1)];;




// 6. Define a function
//
// replicateCells : Cell list -> Cell list
//
// that replicates each cell in the list n times, where n is
// 0             for Empty
// max 0 v       for Value v
// max 0 (x + y) for Pair (x, y)
//
// Use List.collect

let replicateCells (xs: Cell list) : Cell list =
    List.collect(function
    |Empty       ->  [] 
    |Value v     -> List.replicate (max 0 v) (Value v)
    |Pair (x, y) ->  List.replicate (max 0 (x + y)) (Pair(x, y))
    )xs;;

    replicateCells [Empty; Value 0; Value 2; Pair(2,2)];;

//List.replicate (max 4 3) (3);;


// 7. Define a function
//
// flattenPairs : Cell list -> Cell list
//
// that replaces a Pair (x, y) with Value x followed by Value y.
//
// Use List.collect.
    
let flattenPairs (xs: Cell list) : Cell list =
    List.collect (fun a ->
    match a with
    |Pair (x, y) -> [Value x; Value y]
    |_ -> [a]
    ) xs;;

    flattenPairs [Pair(0,1); Value 2];;

(*let list12 = [2;3;4;5];;
List.map(fun x -> x*2)list12;;

List.collect(fun x-> [x*2])list12;;*)



// 8. Define a function
//
// countCells : Cell list -> int * int * int
//
// which counts the number of Empty, Value, and Pair cells in the list.
//
// If 'countCells xs' is (1, 2, 3) then
// 1 is the number of Empty cells
// 2 is the number of Value cells
// 3 is the number of Pair  cells
//
// Use List.fold
    
let countCells (xs: Cell list) : (int * int * int) =
    List.fold (fun (emptyCells: int, valueCells: int, pairCells: int) (c: Cell) ->
    match c with
    | Empty -> (emptyCells+1, valueCells, pairCells)
    | Value v -> (emptyCells, valueCells+1, pairCells)
    | Pair (x,y) -> (emptyCells, valueCells, pairCells+1)
    ) (0, 0, 0) xs;;    

    countCells [Empty; Empty; Value 2; Pair(0,1)];;




// 9. Define a function
//
// cellsToString : Cell list -> string
//
// that constructs a string representation of the cells.
//
// Empty       is represented as "."
// Value v     is represented as "v"
// Pair (x, y) is represented as "x,y"
//
// Use "|" as the separator symbol.
// The result must start and end with the separator.
// You can convert an int to a string by the function 'string'.
//
// Use List.fold

  

let oneCellIntoString (oneCell: Cell) : string =
//List.fold(fun a -> string(a) oneCell;;
//string(oneCell);
//oneCell.ToString();;
    match oneCell with
    | Empty -> "."
    | Value v -> string(v)
    | Pair(x, y) -> string(x) + "," + string(y);;

    
let cellsToString (xs: Cell list) : string =
    List.fold(fun str cell -> str + oneCellIntoString(cell) + "|" ) "|" xs ;;

    cellsToString [Empty; Value 2; Pair(2, 4)];;


//List.fold(fun acc lst -> acc @ lst ) [] [[2; 4];[1; 3];[5;8]];;

