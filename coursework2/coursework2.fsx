(*

  ITT8060 -- Advanced Programming 2019
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists and tuples, recursion, combination of functions

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
  repository itt8060-2019 under your name, into a file coursework2/coursework2.fsx.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.
*)

// 1. Create a type BibliographyItem that has the following structure:
// string list * string * (int * int) * int
// The meaning of the tuple elements is as follows:
// * The first field represents the list of author names where each name is in the format
//   "Lastname, Firstname1 Firstname2" (i.e. listing all first names after comma)
// * The second field represents the title of the publication
// * The third field represents a pair containing the starting page number and ending page number of the paper.
// * The fourth field represents the year of publication

type BibliographyItem = string list * string * (int * int) * int

// 2. Create a value bibliographyData : BibliographyItem list that contains
// at least 7 different publications on your favourite topic from https://dblp.uni-trier.de/ 
// Please note that you need not read the papers, just pick 7 papers that sound interesting to you from the database.

let bibliographyData: BibliographyItem List = 
    [
    (["Ochei, Laud Charles";"Petrovski, Andrei";"Bass, Julian"],"Optimal deployment of components of cloud-hosted application for guaranteeing multitenancy isolation",(1,17),2019);

    (["Talia, Domenico"],"A view of programming scalable data analysis: from clouds to exascale",(1,17),2019);

    (["Lavazza, Luigi";"Morasca, Sandro";"Tosi, Davide"],"An Empirical Study on the Factors Affecting Software Development Productivity",(27,49),2018);

    (["Kowalski, Marcin";"Magott, Jan"],"Time coordination of heterogeneous distance protections using a domain specific language",(7,26),2012);

    (["Hotomski, Sofija";"Glinz, Martin"],"An approach for keeping requirements and acceptance tests aligned via automatically generated guidance",(17, 38),2019);

    (["Hotomski, Sofija";"Charrada, Eya Ben";"Glinz, Martin"],"A Qualitative Study on using GuideGen to Keep Requirements and Acceptance Tests Aligned",(29,39),2019);

    (["Bogdanova, Daria";"Snoeck, Monique"],"An educational framework for conceptual data modelling",(92,107),2019);

    (["Sun, Yan";"Wang, Qing";"Yang, Ye"],"Improving the recovery of missing issue-commit links by revisiting file relevance",(33,47),2017);

    (["Gren, Lucas";"Knauss, Alessia";"Stettina, Christoph Johann"],"Non-technical individual skills are weakly connected to the maturity of agile practices",(11,20),2018);
    ];;

//[(["a,b"; "c,d], "title", (1,2), 1988)]; (["a,b"; "c,d], "title", (1,2), 1988)]  

// 3. Make a function compareLists : string list -> string list -> int that takes two string lists and
// returns
// * Less than zero in case the first list precedes the second in the sort order;
// * Zero in case the first list and second list occur at the same position in the sort order;
// * Greater than zero in case the first list follows the second list in the sort order;
// You need to use the support honouring the default culture for ordering strings, i.e. the built in
// compare does not work properly by default. Look at the
// documentation of the .Net comparison for strings: System.String.Compare
// If the first authors are the same
// then the precedence should be determined by the next author.
// A missing author can be considered to be equivalent to an empty string.
// Please note that your implementation should be recursive over the input lists.
//
// The sort order in .Net is defind using System.Globalization.CultureInfo:
// https://docs.microsoft.com/en-us/dotnet/api/system.globalization
// Please note that your solution should not force a particular sort order!

(*open System.Globalization
System.Globalization.CultureInfo.CurrentCulture
//System.Globalization.CultureInfo <- CultureInfo ("en-US")
System.Threading*)

let rec compareLists (x1: string list) (x2: string list) : int = 
    match (x1, x2) with
    | ([],[])          ->  0
    | (_,[])           ->  1 //positive number, doesnt matter which one. It can be 2000
    | ([],_)           -> -1 //negative number. Doesn't matter which one it is
    | (h1::t1, h2::t2) -> let c = System.String.Compare(h1,h2)
                          if c = 0 then compareLists t1 t2 else c
                            
                         
    //let c = System.String.Compare(h1,h2)
    //if c = 0 then 0 else c

(*compareLists [] []
compareLists ["hello"] []
compareLists [] ["hello"]
compareLists ["hello"; "hi"] ["hello"; "hii"]*)
    
    
//System.String.Compare ("hec", "hed");;

// 4. Make a function
// compareAuthors : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors.
// Use solution from task 3. 

let compareAuthors ((authors1,_,_,_): BibliographyItem) ((authors2,_,_,_): BibliographyItem) : int =
      compareLists authors1 authors2;;



//let elem1 = (["Ochei, Laud Charles";"Petrovski, Andrei";"Bass, Julian"],"Optimal deployment of components of cloud-hosted application for guaranteeing multitenancy isolation",(1,17),2019);
//let elem2 = (["Talia, Domenico"; "Andrew, Jacobson" ],"A view of programming scalable data analysis: from clouds to exascale",(1,17),2019);

//compareAuthors elem1 elem2;;

// 5. Make a function
// compareAuthorsYears : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors and if the authors are 
// the same then according to years.

let compareAuthorsYears ((authors1,_,_,year1): BibliographyItem) ((authors2,_,_,year2): BibliographyItem) : int = 
    let a = compareLists authors1 authors2 
    if a = 0 then compare year1 year2 else a;;
  

   let elem4 = (["Ochei, Laud Charles";"Petrovski, Andrei";"Bass, Julian"],"Optimal deployment of components of cloud-hosted application for guaranteeing multitenancy isolation",(1,17),2020);
   let elem5 = (["Ochei, Laud Charles";"Petrovski, Andrei";"Bass, Julian" ],"A view of programming scalable data analysis: from clouds to exascale",(1,17),2019);
compareAuthorsYears elem4 elem5;;    

// 6. Make a function 
// sortBibliographyByYear : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the year in ascending order
let sortBibliographyByYear (x1: BibliographyItem list) : BibliographyItem list = 
    List.sortBy(fun(_,_,_,year) -> year) x1;;

//let elem6 = (["Ochei, Laud Charles";"Petrovski, Andrei";"Bass, Julian"],"Optimal deployment of components of cloud-hosted application for guaranteeing multitenancy isolation",(1,17),2019);
//let elem7 = (["Talia, Domenico"; "Andrew, Jacobson" ],"A view of programming scalable data analysis: from clouds to exascale",(1,17),2018);

// 7. Make a function 
// sortBibliographyByAuthorYear : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the authors and year in ascending order

let sortBibliographyByAuthorYear (x1: BibliographyItem list) : BibliographyItem list =
    //List.sortWith compareAuthorsYears x1;;
    List.sortBy(fun(authorsList,_,_,y) -> authorsList, y) x1;;// |> List.sortBy (fun(_,_,_,year) -> year);;

// 8. Make a function
// groupByAuthor : BibliographyItem list -> (string * BibliographyItem list) list
// where the return list contains pairs where the first element is the name of a single
// author and the second element a list of bibliography items that the author has co-authored.

//let authorsEach = List.collect(fun (authors,_,_,_) -> authors) x1
let groupByAuthor (x1: BibliographyItem list) : (string * BibliographyItem list) list =
    (*let getAuthors (list1:BibliographyItem list) : string list =
        match list1 with
        |[] -> []
        |h::t -> let (authors, _, _, _) = h
                 authors*)
    let getAuthors = List.collect(fun (authors,_,_,_) -> authors) x1

    let authorsIN (authorName: string) ((authors,_,_,_): BibliographyItem) : bool = 
        List.contains (authorName) (authors)
    List.map(fun x -> (x, List.filter(authorsIN x) x1)) getAuthors;;

   
    
    
    //let itemsCoauthoredIN (author:string) (items: BibliographyItem list) : BibliographyItem list =
        
        
        //List.filter (authorsIN author) items

        groupByAuthor bibliographyData;;  
         


    (* let a1 = List.filter(fun x -> authorsIN x) x1
    let a2 = List.map (fun x -> x) getAuthors;;*)
    
    //List.map(fun x -> (x, List.filter(authorsIN x) x1)) getAuthors;;

  

    (*let rec getTitles (list2: BibliographyItem) =
    match list2 with
    |[] -> []
    |h::t -> let (authors, title, pages, year) = h
    title :: (getAuthors t);;*)         
    //List.map(fun x -> (x, List.filter(authorsIN x) x1)) getAuthors;;              