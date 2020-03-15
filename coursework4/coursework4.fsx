(*

  ITT8060 -- Advanced Programming 2019
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: Working with HTML trees

  ------------------------------------
  Name: Mirlind Murati
  Tallinn University of Technology Student ID
  or Uni-ID: 195329IVSM
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the https://gitlab.cs.ttu.ee
  repository itt8060-2019 under your name, into a file coursework4/coursework4.fsx.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.
*)

// The following type declarations are given:

type Kind = Div | Par | Hdg

type Class = string

type Identifier = string


// Task 0
// Define a data type HTree for representing an html tree. The data type needs to support Div, Par and Hdg 
// elements, style classes and optional identifiers. Content of each element can contain either 0 or more elements.
// Please note that you will use the type definition in futher tasks, but task 0 will not be tested on its own.

type HTree = Element list
and Element = El of Kind * Class list * (Identifier option) * HTree
              |Content of string

              //another way of defining the data structure
(*type HTree' = El' of Kind * Class list * (Identifier option) * HTree list
              |Content' of string*)


              // |Child of HTree
              
// Task 1
// Define a function
// makeElement : Kind -> Identifier option -> HTree
// that can be used to create HTree elements given kind and optional identifier.
// Examples:
// makeElement Div None
// makeElement Hdg (Some "id1")

let makeElement (kind: Kind) (id: Identifier option) : HTree =
    [El(kind, [], id, [])];

//testing Task1
makeElement Div None;;
makeElement (Div) (Some "firstID");;
makeElement Div (None : Identifier option);;
    
// Task 2
// Define a function
// addClass : Class -> HTree -> HTree
// Such that the function could be used to add a class to an HTree element.
//
let rec addClass (cls:Class) (htr: HTree) : HTree = 
    match htr with 
    |[] -> []
    |(Content m)::t -> [Content m] @ addClass cls t
    |(El(k, c, id, tree))::t -> [El(k,cls::c,id,tree)] @ addClass cls t
    

addClass ("class1") [(El(Div,["class"], None,[]))];;
    
// and a function
// addContent : string -> HTree -> HTree
// that will add appropriate string content to the particular element
 
let rec addContent (s: string) (partelem: HTree) : HTree =
     match partelem with
     |[] -> []
     |(Content str)::tail -> [Content (str + " " + s)] @ addContent s tail
     |(El(a,b,c,children))::t -> [El (a,b,c,children)] @ addContent s t

addContent ("hello") [Content "hello1"];;
addContent ("Hello") [(El(Div,["class"], None,[El(Par, ["class2"], Some "id2", [])]))];;


// and a function
// addChild : HTree -> HTree -> HTree
// that could be used to add child to a HTree. The first argument is the child,
// the second element is the one to which the child gets added to.

let rec addChild (child: HTree) (parent:HTree) : HTree  =
    match parent with
    |[] -> []
    //|[Content m] -> [Content m]
    |(Content s)::tail -> [Content s] @ addChild child tail
    |(El (k,cl, id, tree)) :: t -> [El(k,cl,id,tree@child)] @ addChild child t

addChild [(El(Par, ["class1"], None, [] ))] [(El(Div, ["class"], None, [El (Hdg, ["class2"], Some "id1", [])]))] 

// Task 3
// Define the function
// countElems : HTree -> int
// that will count the number of all elements in the tree.

let rec countElems (tree: HTree) : int = //too simple 
    match tree with //when the head of the list is a content
    | [] -> 0
    |(Content s)::tail -> countElems tail
    | El(_,_,_,children)::t -> 1 + (countElems children) + (countElems t);;

let exampleHTML = [El(Div,["Class"], Some "id", [El(Div, ["Class2"], None, [])])]
countElems exampleHTML

// Task 4
// Define a function of type 
// listElems : HTree -> Kind list list
// that will return a full path of each element in the tree.
let rec listElems (fullPath: HTree)  : Kind list list =
    match fullPath with
    |[] -> []
    //|(Content m)::t -> []
    |el::tail -> (namesElement el) @ (listElems tail)
and namesElement el =
    match el with
    |El(k,_,_,children) -> [k] :: listElems children
    |Content(n) -> []

// And a function
// showContent : HTree -> string
// That will concatenate all string content in the tree following
// in order traversal pattern.

//commented code below
let rec showContent (cnt: HTree) : string =
    match cnt with
    | [] -> ""
    | (Content s)::tail -> s + showContent tail
    | (El(_,_,_,tree))::tail -> showContent tree + showContent tail

let test11  = [(Content "hey");El(Div,["class1"], None,[Content "Content1"; El(Par, ["class2"], Some "id1",[])])];;
//let test1 = [El(Div,["class1";"class2"],Some "id1",[Content "Content 1"; El(Par,[],None, [Content "Content 2"]); Content "Content 3"])]
let resc1 = showContent test11
printfn "%A" resc1
// Given the following type declarations

type Select = ByKind   of Kind
            | ById     of Identifier
            | ByClass  of Class

type Selector = Any
              | Select   of Select
              | Sequence of Selector * Selector
              // | ZeroOrMore of Selector


// The instances of the types can be used to
// define queries, e.g.
// the parent-child relation of x y : Select would be:
// Sequence ( ZeroOrMore Any, Sequence (Select x, Select y))
//
// and descendant relation of x y : Select would be:
// Sequence ( ZeroOrMore Any, Sequence (Select x, Sequence (ZeroOrMore Any, Select y)))



// Task 5
// Define a function that finds the first element that satisfies the selector property (if such an element exists).
// It should be the first element in the order in which the children were added to the tree.
// (Kind list * HTree) will be the path to the element and the appropriate element.
//
// first : Selector -> HTree -> (Kind list * HTree) option  //start with Htree 
let rec first (sel: Selector) (tree: HTree) : (Kind list * HTree) option =
    match (sel, tree) with
    | (_, []) -> None
    | (_, Content _::tail) -> first sel tail
    | (Any, El(k,cls,id,cld)::_) -> Some ([k], [El(k,cls,id,cld)])
    | (Select s, El(k,cls,id,cld)::tail) -> match s with
                                            | ByKind kind when kind = k -> Some ([k], [El(k,cls,id,cld)])
                                            | ByClass c when List.contains c cls -> Some ([k], [El(k,cls,id,cld)])
                                            | ById ids -> match id with
                                                            | Some id' when id' = ids -> Some ([k], [El(k,cls,id,cld)])
                                                            | _ -> first sel tail
                                            | _ -> first sel tail
    | (Sequence(x,y), tree') -> let f = first x tree'
                                match f with
                                | Some (kindList, (El(_,_,_,cld)::_)) -> firstCld kindList y cld
                                | _ -> None
and firstCld kindList y tree = 
    let s = first y tree
    match s with
    | Some (kl, tree') -> Some (kindList @ kl, tree')
    | _ -> None

let test2 = [El(Div,["list"],None,[
                El(Par,[],None, []);
                El(Div,[],None,[
                    El(Hdg, [], None, [])
                ])])]
let resf = first (Sequence(Select(ByClass "list"),Sequence(Select (ByKind Div), Select(ByKind Hdg)))) test2
printfn "%A" resf

// Task 6
// Like previous, but should find all elements satisfying the selector
// select : Selector -> HTree -> (Kind list * HTree) list
let rec select (sel: Selector) (tree: HTree) : (Kind list * HTree) list =
    match (sel, tree) with
    | (_, []) -> []
    | (_, Content _::tail) -> select sel tail
    | (Any, El(k,cls,id,cld)::_) ->  [([k], [El(k,cls,id,cld)])]
    | (Select s, El(k,cls,id,cld)::tail) -> match s with
                                            | ByKind kind when kind = k ->  [([k], [El(k,cls,id,cld)])]
                                            | ByClass c when List.contains c cls ->  [([k], [El(k,cls,id,cld)])]
                                            | ById ids -> match id with
                                                            | Some id' when id' = ids ->  [([k], [El(k,cls,id,cld)])]
                                                            | _ -> select sel tail
                                            | _ -> select sel tail
    | (Sequence(x,y), tree') -> let f = first x tree'
                                match f with
                                | Some (kindList, (El(_,_,_,cld)::_)) -> firstCld1 kindList y cld
                                | _ -> []
and firstCld1 kindList y tree = 
    let s = first y tree
    match s with
    | Some (kl, tree') ->  [(kindList @ kl, tree')]
    | _ -> []


// Task 7
// Define a function that adds a class to the elements that satisfy the selector
// addClass' : Selector -> Class -> HTree -> HTree
(*let rec addClass' (sel: Selector) (cl: Class) (tree: HTree) :  HTree =
    match (sel, tree) with
    | (_, []) -> []
    | (_, Content _::tail) -> addClass' sel cl tail
    | (Any, El(k,cls,id,cld)::_) ->  [El(k,cl::cls,id,cld)]
    | (Select s, El(k,cls,id,cld)::tail) -> match s with
                                            | ByKind kind when kind = k ->  [El(k,cl::cls,id,cld)]
                                            | ByClass c when List.contains c cls ->  [El(k,cl::cls,id,cld)]
                                            | ById ids -> match id with
                                                            | Some id' when id' = ids ->  [El(k,cl::cls,id,cld)]
                                                            | _ -> addClass' sel cl tail
                                            | _ -> addClass' sel cl tail
    | (Sequence(x,y), tree') -> let f = first x tree'
                                match f with
                                | Some (kindList, (El(_,cls,_,cld)::_)) -> firstCld2 kindList y cld
                                | _ -> []
and firstCld2 kindList y tree = 
    let s = first y tree
    match s with
    | Some (kl, tree') ->  []
    | _ -> []*)

// Similarly, define a function that adds a child to a tree satisfying a selector.
// The first HTree is the element to be added and the second HTree is the tree to which to add.
// addChild' : Selector -> HTree -> HTree -> HTree



// Task 8
//
// Define a function that deletes all elements satisfying the selector.
// Do not delete the root element.
// delete : Selector -> HTree -> HTree
