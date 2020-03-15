(*

  ITT8060 -- Advanced Programming 2019
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 8: asynchronous programming

  ------------------------------------
  Name: Mirlind Murati
  Student ID: 195329IVSM
  ------------------------------------


  Answer the questions below. Your answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060-2019 under your name, into a file
  coursework8/coursework8.fsx by December 15, 2019.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is
  incorrect it will not be graded.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

  NB! Do not delete the stubs we have provided! If you did not manage
  to complete the implementation then keep the incomplete stub and
  make sure that it still is typeable as described in the question.
*)


(*
   Question 1

   The Mandelbrot set.
   Formal definition: https://en.wikipedia.org/wiki/Mandelbrot_set#Formal_definition

   Define the function

   mandelbrot : int -> Complex -> bool

   so that given

   n : int
   c : Complex

   'mandelbrot n c' evaluates to true when according to n iterations
   c belongs to the Mandelbrot set.

   Start by computing the z_1, z_2, ..., z_n for c. Based on this
   sequence of values decide whether c belongs to the Mandelbrot set
   or not.

   Complex is the Cartesian form of a complex number.

*)

type Complex = double * double
let aVal ((a,b): Complex) = sqrt(a ** 2.0 + b ** 2.0)
let sqr ((a,b): Complex) = (a ** 2.0 - b ** 2.0, double(2.0 * a * b))
let complexAddition ((a,b): Complex)  ((c,d): Complex) = (a + c, b + d) 

//let mandelbrot n c = //failwith "not implemented"

let rec generateSequence (a: Complex) (b: Complex) = 
    seq {
         let c = complexAddition(sqr(a)) b
         yield c
         yield! generateSequence c b
    }

let rec sequenceCheck sequence m a =
    match a with
    | m' when m'=m -> true
    | _ -> let b = aVal(Seq.head sequence)
           if b> (2.0) then false else sequenceCheck (Seq.tail sequence) m (a + 1)

let mandelbrot n c = //failwith "not implemented"
    let cached = Seq.cache (generateSequence (0.0, 0.0)c)
    sequenceCheck cached n 0

(*
   Question 2

   Define the function
 
   divide : int -> int -> (int * int) seq

   so that given

   m : int
   n : int

   'divide m n' evaluates to a sequence of ranges

   (s_1, e_1), ..., (s_m, e_m)

   so that s_1 = 0 and e_m = n - 1
   and all the (s_i, e_i) cover 0, ..., n - 1 without overlap, i.e.,

   s_{i + 1} = e_i + 1

   m is the number of workers we have and thus we create m chunks.
   n is the amount of work we have (the length of the array of complex
   numbers in the next question).

*)

let divide m n = //failwith "not implemented"
    let minimumN = min m n
    let x = n / minimumN
    let y = n % minimumN
    seq {
        for i in 0 .. y-1 do
        yield i*(x+1), (i+1)*(x+1)-1
        for i in y .. minimumN do 
        yield y+i*x, y+(i+1)*x-1
    }




(*
   Question 3

   Define the function
   
   mandelbrotAsync : int -> int -> (int -> unit) -> Complex [] -> Async<bool []>

   so that given

   m  : int
   n  : int
   f  : int -> unit
   cs : Complex []

   'mandelbrotAsync m n f cs' creates an asynchronous computation
   which computes an array of booleans with the same size as cs. An
   element in this array is true precisely when the complex number at
   the corresponding position in cs belongs to the Mandelbrot set
   (according to n iterations).

   Use the 'divide' function to divide the work into m chunks and then
   evaluate those in parallel using the Async mechanism.

   Whenever this function starts computing on a chunk of input (s, e)
   the function must invoke 'f s'. When it has completed working on
   the chunk then it must invoke 'f e'.

*)

let mandelbrotAsync m n f cs = failwith "not implemented"





// The next questions are about observables.
//
// You are not allowed to write any mutable code. Solve these using
// the combinators from FSharp.Control.Observable module.


(*
   Question 4

   Define the function

   accumulate : ('t -> 'a -> 't * 'u option) -> 't -> IObservable<'a> -> IObservable<'u>

   so that given

   f   : 't -> 'a -> 't * 'u option
   t   : 't
   obs : IObservable<'a>

   'accumulate f t obs' accumulates observable events of obs into an
   accumulator of type 't and emits an observable event u when
   'snd (f acc a)' evaluates to 'Some u' for an observed event 'a'.

*)

let accumulate f t obs = //failwith "not implemented"
    obs|> Observable.scan (fun (i, _) a -> f i a) (t, None)
       |> Observable.choose snd




(*
   Question 5

   Define the function

   chunks : int -> IObservable<'a> -> IObservable<'a list>

   so that given

   n   : int
   obs : IObservable<'a>

   'chunks n obs' is the observable of chunks (or pieces) of length n
   of the observable obs.

   If n = 3 and obs emits events a_1, a_2, a_3, ... then

   'chunks n obs' emits [a_1; a_2; a_3], [a_4; a_5; a_6], ...

*)

let chunks n obs = //failwith "not implemented"
    let n' x y = 
        match List.length x with 
        | x' when x' = n-1 -> ([], Some(x@[y]))
        | _ -> (x@[y], None)
    accumulate n' [] obs





(*
   Question 6

   Define the function

   sliding : int -> IObservable<'a> -> IObservable<'a list>

   so that given

   n   : int
   obs : IObservable<'a>

   'sliding n obs' is the observable of sliding windows of length n
   over the observable obs.

   If n = 3 and obs emits events a_1, a_2, a_3, ... then

   'sliding n obs' emits [a_1; a_2; a_3], [a_2; a_3; a_4], ...

*)

let sliding n obs = //failwith "not implemented"
    obs |> Observable.scan (fun x y ->
        match x with
        | _::other when List.length other = n-1 -> other @ [y]
        | _ -> x @ [y]
      ) []
      |> Observable.filter (fun z -> List.length z = n)




(*
   Question 7

   Define the function

   limit : IObservable<unit> -> IObservable<'a> -> IObservable<'a>

   so that given

   clock : IObservable<unit>
   obs   : IObservable<'a>

   'limit clock obs' emits observable events from obs.

   The observable starts from emit mode: the next observed event of
   obs can be emitted. After that the observable transitions to
   discard mode: all observed events of obs are discarded until an
   event from clock is observed after which the observable transitions
   back to emit mode and the process repeats.

*)

let limit clock obs = failwith "not implemented"





(*
   Question 8

   Define the function

   alarm : int -> int -> IObservable<unit> -> IObservable<int> -> IObservable<unit>

   so that given

   n         : int
   threshold : int
   clock     : IObservable<unit>
   obs       : IObservable<int>

   'alarm n threshold clock obs' emits an event when the average value
   of last n events of obs has been greater than the threshold.

   Furthermore, after emitting an event the next event should only be
   emitted after observing the next clock event.

*)

let alarm n threshold clock obs = failwith "not implemented"
