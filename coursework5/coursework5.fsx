(*

  ITT8060 -- Advanced Programming 2019
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 5: Tail recursion

  ------------------------------------------------
  Name: Mirlind Murati
  Student ID: 195329IVSM
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework6.fsx in directory coursework6.

  The deadline for completing the above procedure is Sunday, November 3, 2019.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:

  Write a function
  forAllInList : (int -> int -> bool) -> (int * int) list ->  bool
  that returns true if the predicate passed as the first argument holds
  for all pairs in the list passed as the second argument to the function.
  Make sure your implementation uses explicit tail recursion.

  e.g. 

  forAllInList (fun _ _ -> true) [(1,2);(3,4)] should return true.
*)
let forAllInList (function1: int -> int -> bool) (intTupleList1: (int*int) list) =
    let rec recFunction function2 intTupleList2 check =
        match intTupleList2 with
        |[] -> check 
        |(x,y)::tail -> recFunction function2 tail (check && (function2 x y))
    recFunction function1 intTupleList1 true
forAllInList (fun x y -> x < y) [(1,2);(3,4)] 
(*
  Task 2:

  Write a function
  filterAndCreatePairs : 'a -> ('a -> bool) -> 'a list -> ('a * 'a) list
  that takes a list of 'a-s (as third argument), filters them with the predicate given as the second argument
  and returns a list of pairs of 'a-s that are taken
  from the start of the list passed as the second argument
  to the function.
  In case the list has odd number of elements make the first argument of the
  function be the second element in the last pair. 
  Make sure your implementation uses explicit tail recursion.

  e.g. evaluating
  filterAndCreatePairs 0 (fun _ -> true) [1;2;3] should produce
  [(1,2);(3;0)]

  and evaluating
  createPairsOfList "a" (fun _ -> true) ["b";"c";"d";"e"] should produce
  [("b","c");("d","e")]
*)
(*let filterAndCreatePairs (a) (func:'a -> bool) (aL:'a list) =
    let rec sub_rec a func al accum state carrier =
        match (al,state) with
        |([],false) -> printfn "here1" ;accum
        |([],true) -> printfn "here2" ;if func a = true then accum@[(carrier,a)] else accum
        |(x::t,true) ->printfn "here3" ; if (func x) then sub_rec a func t (accum@[(carrier,x)]) false carrier else sub_rec a func t accum true carrier
        |(x::t,false) -> printfn "here4" ;if (func x) then sub_rec a func t accum true x else sub_rec a func t accum false carrier
    sub_rec a func aL [] false a

filterAndCreatePairs "a" (fun _ -> true) ["b";"c";"d";"e"] *)

let filterAndCreatePairs (a: 'a) (function1: ('a -> bool)) (list1: 'a list) = 
    let rec tailRec (a: 'a) (function1: ('a -> bool)) (list1: 'a list) acc =
        match list1 with 
        | [] -> List.rev acc
        | head1::head2::tail -> if function1 head1 = true && function1 head2 = true then tailRec a function1 tail ((head1, head2)::acc)
                                elif function1 head1 = true && function1 head2 = false then tailRec a function1 (head1::tail) acc
                                elif function1 head1 = false && function1 head2 = true then tailRec a function1 (head2::tail) acc
                                else tailRec a function1 tail acc 
        | h::[] -> List.rev((h, a)::acc)
    tailRec a function1 list1 []

filterAndCreatePairs "a" (fun _ -> true) ["b";"c";"d";"e";"f"] 

(*
  Task 3:

  Write a function
  filterAndCreatePairsFold : 'a -> ('a -> bool) -> 'a list -> ('a * 'a) list
  that behaves similarly to the function defined in Task 2.
  Make sure your implementation uses List.fold or List.foldBack or its multiple
  argument conterparts appropriately.
  Test yourself if this implementation appears to be tail recursive.
*)
(*let filterAndCreatePairsFold (a) (func:'a -> bool) (aL:'a list ) =
    let check_list = List.map func aL
    let checker accum elem_check elem_a = 
        if elem_check then elem_a::accum else accum
    let all = List.fold2 checker [] check_list aL
    let rec makePairs ls =
        match ls with
        |x::[] -> []
        |[] -> []
        |x::y::tail -> (x,y)::makePairs tail
    let paired = makePairs all 
    let last_item = all |> List.rev |> List.head 
    if all.Length % 2 = 0 then paired else paired@[(last_item,a)]*)
let filterAndCreatePairsFold (argument1: 'a) (function1: 'a -> bool) (list1: 'a list) : ('a * 'a) list =
    let recFunction (first : (('a*'a) list)* 'a option) (x: 'a) :((('a * 'a) list)* 'a option) =
        match first, function1 x with
        |_, false              -> first
        |(list, None), true    -> match list1 with
                               | [] -> (list @ [(x, argument1)]), None
                               | _  -> list, Some(x)
        |(list, Some(y)), true -> (list @ [y,x]), None
    let (result1, option1) = List.fold recFunction ([], None) list1
    match option1 with
    |Some(x) -> result1@[x, argument1]
    |None    -> result1

filterAndCreatePairsFold 0 (fun _ -> true) [1;2;3;4]
(*
  Task 4:

  Below you find the definition of a type Tree of leaf-labeled trees. Write a
  function minAndMaxInTree : int Tree -> int * int that returns the the min and max elements
  of the tree.
  Use continuation-passing style in your implementation.
*)

type 'a Tree =
  | Tip   of 'a
  | Node of 'a Tree * 'a Tree

let minAndMaxInTree (tree1: int Tree) : int * int = 
    let rec recFunction tree2 cont minMax =
        match tree2 with
        | Tip(a) -> cont a
        | Node (left, right) -> 
            recFunction left (fun vl -> recFunction right (fun vr -> cont (minMax vl vr)) minMax) minMax
    (recFunction tree1 id Operators.min), (recFunction tree1 id Operators.max)

(*let yl4_test1 = minAndMaxInTree (Node(Tip(2), Node(Tip(1),Tip(3)))) // (1,3)
let yl4_test2 = minAndMaxInTree (Node(Tip(1), Node(Tip(3),Tip(2)))) // (1,3)
let yl4_test3 = minAndMaxInTree (Node(Node(Tip(0), Tip(5)), Node(Tip(1),Tip(3)))) // (0,5)*)