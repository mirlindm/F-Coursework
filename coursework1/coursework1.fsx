(*

  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 1: Basic operations on lists

  ------------------------------------
  Name: Mirlind Murati
  Student ID or Uni-ID: 195329IVSM
  ------------------------------------


  Answer all the questions below.  You answers to questions should be
  correct F# code written after the question in comments. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere and your result will not be evaluated.

  This coursework will be graded.

  To submit the coursework you will be asked to
  
  1) Check out your  GIT repository
  from the server gitlab.cs.ttu.ee using instructions on page
  https://courses.cs.ttu.ee/pages/ITT8060

  2) Put your solution into a file coursework1/coursework1.fsx
  in the repository. Commit it and push it to the server!
  It is your responsibility to make sure you have pushed the solution
  to the repository!

  NB! It is very important to make sure you use the exact name using
  only small caps. Files submitted under wrong name may not get a grade.

  Also, use the exact function and identifier names with precise types as 
  specified in the question.

  The F# interpreter should be able to load your solution without errors.

  NB! In this coursework you are not allowed to use functions from the
  List module. List processing and recursion has to be done explicitly.
  It is also not allowed to use mutable values, i.e. variables.
*)


// 1. Associate an identifier "myFirstList" with an empty list of floats.
let myFirstList : float list = [];;




// 2. Define a function
//
// elem : int -> 'a list -> 'a
//
// so that 'elem i xs' evaluates to the i-th element in the list xs.
//
// The function should throw an exception when i is less than zero or greater than
// the number of elements in xs. You should use 'failwith' to throw an exception.
//
// The function must be implemented via explicit recursion.
//
// Keep in mind that a list can either be empty ([]) or non-empty (x :: xs)
// and you need to define what is the i-th element of the list in both
// cases.
//
// When you have completed this definition then think about the time complexity
// of your implementation.

let rec elem (i: int) (xs: 'a list): 'a = 
    if i < 0 || i >= xs.Length then failwith  "Constraints not met! 'i' is out of range" 
    match(i, xs) with 
    | (0, x::_) -> x
    | (_, _::xs) -> elem (i - 1) xs
    | (_, []) -> failwith "Complete Pattern Matches. Incorrect!"
    ;;

//elem 2 [2;3;4;5];;


// Here is a type synonym for a tuple for representing information about architects.

type Architect = (string * int * bool * string)

// The components of the tuple are:
// * name of the architect
// * year of birth
// * is the architect alive
// * country of birth


// 3. Define a list
//
// architects : Architect list
//
// which contains information about your favourite architects.
// The list should contain at least five unique architects.
//
// You can use this list in the following exercises to test your code.

let architects : Architect list  = [("Zaha Hadid", 1950, false, "Iraq");
    ("Frank Gehry", 1929, true, "Canada");
    ("Le Corbusier", 1887, false, "Switzerland");
    ("Norman Foster", 1935, true, "United Kingdom");
    ("Antoni Gaudí", 1852, false, "Spain")];;







// 4. Define a function
//
// bornLaterThan : int -> Architect list -> Architect list
//
// so that 'bornLaterThan y xs' evaluates to
// all the architects x in xs satisfying the condition (birth year of x is greater than y)
// and nothing else.
//
// The function must preserve the relative ordering of elements in xs.
//
// The function must preserve duplicates.

let rec bornLaterThan (y: int) (xs: Architect list) : Architect list =
    match xs with
    | [] -> []
    | (a,year,c,d)::[] -> if year > y then [(a,year,c,d)] else [] 
    | (a,year,c,d)::xs -> if year > y then (a, year,c,d) :: bornLaterThan y xs else bornLaterThan y xs;;



    //bornLaterThan 1922 architects;;





// 5. Define a function
//                        
// architectsFrom : string -> Architect list -> Architect list
//                        
// so that 'architectFrom c xs' evaluates to
// all the architects in xs who were born in c
// and nothing else.
//
// The function must preserve the relative ordering of elements in xs.
//
// The function must preserve duplicates.

let rec architectsFrom (c:string) (xs: Architect list) : Architect list =
    match xs with
    | [] -> []
    | (a,b,d,country)::[] -> if country = c then [(a,b,d,country)] else []
    | (a,b,d, country)::xs -> if country = c then (a,b,d, country) :: architectsFrom c xs else architectsFrom c xs
    ;;

    architectsFrom "Iraq" architects;;






// 6. Define a function
//                          
// names : Architext list -> string list
//                          
// so that 'names xs' evaluates to the names of the architects in xs.
//
// 'xs' and 'names xs' must have the same number of elements.
//
// The element at index i in 'names xs' must be the name of the
// architect at index i in 'xs'.

let rec names (xs: Architect list) : string list =
    match xs with
    | [] -> []
    //| (name, b,c,d)::[] -> name :: names xs
    | (name, b,c,d)::xs -> name :: names xs;;

    

    //names architects;;








// 7. Using your solutions to previous exercises define a function
//    
// areFromAndBornLaterThan : string -> int -> Architect list -> string list
//    
// so that 'areFromAndBornLaterThan c y xs' evaluates to the names
// of the architects in xs who are from c and are born later than y.
//
// This should be a one-line definition.

let rec areFromAndBornLaterThan (c:string) (y:int) (xs: Architect list) : string list = architectsFrom c xs|> bornLaterThan y |> names;;

areFromAndBornLaterThan "Iraq" 1920 architects;;