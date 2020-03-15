(*
ITT8060 -- Advanced Programming 2019
Department of Software Science
Tallinn University of Technology
------------------------------------------------------------------------------

Coursework 7: Sequences and computation expressions

------------------------------------------------------------------------------
Name: Mirlind Murati
Student ID: 195329IVSM
------------------------------------------------------------------------------


Answer the questions below. You answers to the questions should be correct F#
code written after the question. This file is an F# script file; it should be
possible to load the whole file at once. If you can't, then you have
introduced a syntax error somewhere.

This coursework will be graded.

Commit and push your script part of the solution to the repository as file
coursework7.fsx in directory coursework7.


The deadline for completing the above procedure is Sunday, December 8, 2019.

We will consider the submission to be the latest version of the appropriate
files in the appropriate directory before the deadline of a particular
coursework.

*)

(*
Task 1:


Define a function returning a sequence

sequenceUsing : int -> (int -> int) -> int -> seq<int>
 
that generates a finite sequence of integers such that:
* the length of the sequence is given by the first argument;

* the elements in the sequence are computed by iteratively
  applying the function given as the second argument to the
  value given as the third argument.

In other words,

sequenceUsing n f b

should generate the sequence

f^0 b, f^1 b, ..., f^(n-1) b

where  f^n b  applies the function f to the value b exactly n times.


*)

let sequenceUsing (count : int) (fn : int -> int) (init : int) : seq<int> =
    let rec sequence1 arg1 = 
        seq {
        yield arg1
        yield! sequence1 (fn arg1)
            }

    sequence1 init |> Seq.take count

sequenceUsing 3 (fun _ -> 1+2) 2

(*
Task 2:


Define a function returning a sequence

pseudoRandom : int -> seq<int> -> seq<int>

that generates pseudo random numbers based on the first argument as seed and 
second argument as an infinite sequence of values of int type.

Use the above sequence nat defined in lecture13.fsx as input for testing.
Use the built in hash function to combine the seed and input integers.
Experiment with ways how to combine the seed and input value in such a way 
that the order of the values in the output sequence differs from that of the input
sequence.

*)

let rec pseudoRandom  (seed:int) (input:seq<int>) : seq<int> = //failwith "pseudoRandom not yet implemented!"
    seq {
        let hashFun = hash(input, seed)
        yield hashFun
        yield! pseudoRandom hashFun (Seq.skip 1 input)
    }

let test2 = Seq.initInfinite (fun i -> i)

pseudoRandom 3 test2

(*
Task 3:

Define a function

cacheObserver : seq<'a> -> seq<'a>

that will cache the values of a sequence and print "Cached"
to standard output every time the value requested from the sequence is actually cached.
*)

let cacheObserver (input:seq<'a>) : seq<'a> = 
    let caching = Seq.map(fun x -> printf "Cached" ; x) input 
    Seq.cache(caching)

cacheObserver [2;3;4]

(*
Task 4:

A function from a type 'env to a type 'a can be seen as a computation that
computes a value of type 'a based on an environment of type 'env. We call such
a computation a reader computation, since compared to ordinary computations,
it can read the given environment. Below you find the following:

  • the definition of a builder that lets you express reader computations
    using computation expressions

  • the definition of a reader computation ask : 'env -> 'env that returns the
    environment

  • the definition of a function runReader : ('env -> 'a) -> 'env -> 'a that
    runs a reader computation on a given environment

  • the definition of a type Expr of arithmetic expressions

Implement a function eval : Expr -> Map<string, int> -> int that evaluates
an expression using an environment which maps identifiers to values.

NB! Use computation expressions for reader computations in your implementation.

Note that partially applying eval to just an expression will yield a function of
type map <string, int> -> int, which can be considered a reader computation.
This observation is the key to using computation expressions.

The expressions are a simplified subset based on
Section 18.2.1 of the F# 4.1 specification:
https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf

*)

(*type ReaderBuilder () =
member this.Bind   (reader, f) = fun env -> f (reader env) env
member this.Return x           = fun _   -> x

let reader = new ReaderBuilder ()

let ask = id

let runReader = (<|)

type Expr =
| Const  of int          // constant
| Ident  of string       // identifier
| Neg    of Expr         // unary negation, e.g. -1
| Sum    of Expr * Expr  // sum 
| Diff   of Expr * Expr  // difference
| Prod   of Expr * Expr  // product
| Div    of Expr * Expr  // division
| DivRem of Expr * Expr  // division remainder as in 1 % 2 = 1
| Let    of string * Expr * Expr // let expression, the string is the identifier.


let eval (e:Expr) : (Map<string, int> -> int) = failwith "not yet implemented"*)

// //Example:
// //keeping in mind the expression: let a = 5 in (a + 1) * 6
// let expr = Let ("a",Const 5, Prod(Sum(Ident("a"),Const 1),Const 6))
// eval expr Map.empty<string,int>
// should return 36     
