(*
ITT8060 -- Advanced Programming 2019
Department of Software Science
Tallinn University of Technology
------------------------------------
Coursework 0: Getting started
------------------------------------
Name: Mirlind Murati
Student ID: 195329IVSM
------------------------------------
Answer the questions below.  You answers to questions 2--8 should be
correct F# code written after the question. The F# code for question
1 is written for you and serves as an example. This file is an F#
script file, it should be possible to load the whole file at
once. If you can't then you have introduced a syntax error
somewhere.
This coursework will NOT be graded but we encourage you to do it,
you will not succeed in this course if you don't practice, and
there's no time like the present! Also, you may find that parts of
it appear in later courseworks. *)

// 0. Find your way to the fsharp interactive (fsi) command prompt.
// I.e. log in to a lab machine and start Visual Studio, install
// VS/Mono on your laptop, etc.

// 1. Load the  following function into fsi
let greeting (name : string) = printfn "Hello: %s" name;;

// 2. Run the function greeting and  say hello to yourself.
greeting "Mirlind"
// 3. Create a value myName : string that contains your name.
let myName:string = "Mirlind Murati";;

// 4.Define
// splitAtChar : text:string -> sep:char -> list<string>
let splitAtChar (text:string) (ch:char) = text.Split ch |> Array.toList;;
splitAtChar myName ' '

// 5. Write a function splitAtSpaces in such a way that it uses splitAtChar
// Hint: we defined splitAtSpaces in the lecture, now you need to modify it.
let splitAtSpaces (text:string) = splitAtChar text ' '
splitAtSpaces myName
// 6. Define sentenceCount : text:string -> int
let sentenceCount (text:string) =
    let sentence = splitAtChar text '.'
    let count = sentence.Length
    count-1;;

sentenceCount "Hello. I am Mirlind."
// 7. Define stats : text:string -> unit
// which prints the same stats as showWordCount +
// the number of sentences and average length of sentences
// hint: try float: int -> float
let stats(text:string) = 
    let sentences = float(sentenceCount text)
    let words = splitAtSpaces(text)
    let wordSet = Set.ofList words
    let numWords = float (words.Length)
    let dups = words.Length-wordSet.Count
    let avgLength = numWords/sentences
    printfn "numWords: %i" (int(numWords))
    printfn "dups: %i" dups
    printfn "avgLength: %f" avgLength

stats("Hello there too. We are SE students. We live in Tartu as well")
// 8. Use the 'http' function from the lecture to download the file
// http://dijkstra.cs.ttu.ee/~juhan/itt8060/text.txt as a string
// NOTE: you cannot use this function in tryfsharp. Instead you can
// paste the text into your file as a string and process it locally
let text = "F# is a mature, open source, 
functional-first programming language which empowers users and organizations to tackle complex
computing problems with simple, maintainable and robust code. It is used in a wide range of application areas 
and is available across multiple platforms. F# runs on Linux, Mac OS X, Android, iOS,
Windows as well as HTML5 and GPUs. F# is free to use and has an OSI-approved open-source license.
F# is supported by industry leading companies providing professional tools, 
and by an active open source community. The F# Software Foundation exists to promote, protect, and 
advance F#, and to support and foster the growth of a diverse international community of F# users."


// 9. run stats on the downloaded file
stats(text)