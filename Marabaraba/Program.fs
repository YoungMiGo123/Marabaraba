open System

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type Coordinates =
         {
             X: int
             Y: int
         }
type Player =
         {
           Cows: unit list
         //  Position: Coordinates
           Turn: bool
         }

type Cell = 
| CW
| CB
| Blank



type GameBoard = 
| Board of (Cell*Cell*Cell*Cell*Cell*Cell*Cell)
           *(Cell*Cell*Cell*Cell*Cell*Cell*Cell)
           *(Cell*Cell*Cell*Cell*Cell*Cell*Cell)
           *(Cell*Cell*Cell*Cell*Cell*Cell*Cell)
           *(Cell*Cell*Cell*Cell*Cell*Cell*Cell)
           *(Cell*Cell*Cell*Cell*Cell*Cell*Cell)
           *(Cell*Cell*Cell*Cell*Cell*Cell*Cell)

type Result =
| Ongoing of GameBoard
| Winner of Cell * GameBoard
| Draw

type States =
 |Placing of (char*int) * (char*int)
 |Moving of (char*int) * string list
 |Flying of (char*int) * (char * int)


    
let playerB = {Cows = [();();();();();();();();();();();();();()] ; Turn = false}
let playerW = {Cows = [();();();();();();();();();();();();();()] ; Turn = false}

let printBoard (Board (r1, r2, r3, r4, r5, r6, r7)) =
      //System.Console.Clear()
      let liz = "_____" //5
      let liz2 = "____" //4
      let bk = "     " //5
      let bk2 = "    "//4
      let printSep1 () = printfn  "     |               |               |\n     |               |               |\n     |               |               |"
      let printSep2 () = printfn  "     |         |           |         |\n     |         |           |         |\n     |         |           |         |"
      let cell value = 
          match value with 
          | CB -> "B"
          | CW -> "W"
          | Blank -> " "
 
      
      let (a1,a2,a3,a4,a5,a6,a7)  = r1
      let (b1,b2,b3,b4,b5,b6,b7)  = r2
      let (c1,c2,c3,c4,c5,c6,c7)  = r3
      let (d1,d2,d3,d4,d5,d6,d7)  = r4
      let (e1,e2,e3,e4,e5,e6,e7)  = r5
      let (f1,f2,f3,f4,f5,f6,f7)  = r6
      let (g1,g2,g3,g4,g5,g6,g7)  = r7
           
          (* printfn "The status is %b" test // Test example of input testing method
           printOutValidCoordinates // Test example of whether valid an input is 
           printfn "The coordinates are %A" (actCoardinates)*)
           //let cell = cell offset
           //let sym = "O"
      printfn "     %d%s%d%s%d%s%d%s%d%s%d%s%d " 1 bk2 2 bk2 3 bk 4 bk 5 bk2 6 bk2 7  // prints out the number scale at the top of the board
      printfn "\n"
      // rest of the methods called prints out the board, one line at a time.
      printfn "A    %s%s%s%s%s%s%s%s%s%s%s%s%s " (cell a1 ) liz ("") liz ("") liz (cell a4 ) liz ("") liz ("") liz (cell a7 ) 
      printSep1()
      printfn "B    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") liz (cell b2 ) liz ("") liz (cell b4 ) liz ("") liz (cell b6 ) liz ("") 
      printSep1()
      printfn "C    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") bk ("") bk (cell c3 ) liz (cell c4 ) liz (cell c5 ) bk ("") bk ("")
      printSep2()
      printfn "D    %s%s%s%s%s%s%s%s%s%s%s%s%s " (cell d1 ) liz2 (cell d2 ) liz2 (cell d3 ) bk bk (" ") (cell d5 ) liz2 (cell d6 ) liz2 (cell d7 )
      printSep2()
      printfn "E    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") bk ("") bk (cell e3 ) liz (cell e4 ) liz (cell e5 ) bk ("") bk ("")
      printSep1()
      printfn "F    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") liz (cell f2 ) liz ("") liz (cell f4 ) liz ("") liz (cell f6 ) liz ("")
      printSep1()
      printfn "G    %s%s%s%s%s%s%s%s%s%s%s%s%s " (cell g1 ) liz ("") liz ("") liz (cell g4 ) liz ("") liz ("") liz (cell g7 ) 
      let printSepConners () = printfn "|\%s|%s/|" bk bk    
      printfn ""
         
      

type Game=
       { A : char; B : char; C : char; D : char; E : char; F : char; G : char; H : char; I : char; J : char; K : char; L : char; M : char; N : char; O : char; P : char;
           Q : char; R : char;  S : char; T : char; U : char; V : char; W : char; X : char }

let Game = { A = 'A';  B = 'B'; C = 'C';D = 'D';E = 'E';F = 'F';G = 'G';H = 'H';I = 'I';J = 'J';K = 'K';L = 'L';M = 'M';N = 'N';O = 'O';P = 'P';Q = 'Q';R = 'R';S = 'S';T = 'T';U = 'U';
           V = 'V';W = 'W';X = 'X';}

type results =
| Ongoing of GameBoard
| Winner of Cell * GameBoard
| Draw

let clearboard()=System.Console.Clear()
let blankBoard =
    let blankRow = Blank, Blank, Blank, Blank, Blank, Blank, Blank
    Board (blankRow, blankRow, blankRow, blankRow, blankRow, blankRow, blankRow)

let swapPlayer x= 
      match x with 
      | CB -> CW
      | CW -> CB
      | Blank -> failwith "A FATAL ERROR OCCURED"
let inputCheck (coordinate) = 
      match String.length (coordinate) with
      | 2 -> 
            match Char.IsLetter(coordinate.[0]) && coordinate.[0]<'g' with 
            | true -> 
                  match Char.IsDigit(coordinate.[1]) &&  ((Int32.Parse(string coordinate.[1]) >= 0) && Int32.Parse(string coordinate.[1]) < 8) with
                      | true -> true
                      | _ -> false
            | _ -> false
      | _ -> false
let isBlank game position = 
    match position, game with
    | "A1", Board((Blank,_,_,_,_,_,_),_,_,_,_,_,_) -> true
    | "A4", Board((_,_,_,Blank,_,_,_),_,_,_,_,_,_) -> true
    | "A7", Board((_,_,_,_,_,_,Blank),_,_,_,_,_,_) -> true
    | "B2", Board(_,(_,Blank,_,_,_,_,_),_,_,_,_,_) -> true
    | "B4", Board(_,(_,_,_,Blank,_,_,_),_,_,_,_,_) -> true
    | "B6", Board(_,(_,_,_,_,_,Blank,_),_,_,_,_,_) -> true
    | "C3", Board(_,_,(_,_,Blank,_,_,_,_),_,_,_,_) -> true
    | "C4", Board(_,_,(_,_,_,Blank,_,_,_),_,_,_,_) -> true
    | "C5", Board(_,_,(_,_,_,_,Blank,_,_),_,_,_,_) -> true
    | "D1", Board(_,_,_,(Blank,_,_,_,_,_,_),_,_,_) -> true
    | "D2", Board(_,_,_,(_,Blank,_,_,_,_,_),_,_,_) -> true
    | "D3", Board(_,_,_,(_,_,Blank,_,_,_,_),_,_,_) -> true
    | "D5", Board(_,_,_,(_,_,_,_,Blank,_,_),_,_,_) -> true
    | "D6", Board(_,_,_,(_,_,_,_,_,Blank,_),_,_,_) -> true
    | "D7", Board(_,_,_,(_,_,_,_,_,_,Blank),_,_,_) -> true
    | "E3", Board(_,_,_,_,(_,_,Blank,_,_,_,_),_,_) -> true
    | "E4", Board(_,_,_,_,(_,_,_,Blank,_,_,_),_,_) -> true
    | "E5", Board(_,_,_,_,(_,_,_,_,Blank,_,_),_,_) -> true
    | "F2", Board(_,_,_,_,_,(_,Blank,_,_,_,_,_),_) -> true
    | "F4", Board(_,_,_,_,_,(_,_,_,Blank,_,_,_),_) -> true
    | "F6", Board(_,_,_,_,_,(_,_,_,_,_,Blank,_),_) -> true
    | "G1", Board(_,_,_,_,_,_,(Blank,_,_,_,_,_,_)) -> true
    | "G4", Board(_,_,_,_,_,_,(_,_,_,Blank,_,_,_)) -> true
    | "G7", Board(_,_,_,_,_,_,(_,_,_,_,_,_,Blank)) -> true
    | _ -> false 

let MillBlack (game:GameBoard) =
    match game with
    | Board((CB,_,_,CB,_,_,CB),_,_,_,_,_,_) -> true //Line a
    | Board(_,(_,CB,_,CB,_,CB,_),_,_,_,_,_) ->true //Line/Mill b
    | Board(_,_,(_,_,CB,CB,CB,_,_),_,_,_,_) ->true //Line/Mill c
    | Board(_,_,_,(CB,CB,CB,_,_,_,_),_,_,_) -> true //Line/Mill d
    | Board(_,_,_,(_,_,_,_,CB,CB,CB),_,_,_) -> true//Line/Mill d
    | Board(_,_,_,_,(_,_,CB,CB,CB,_,_),_,_) ->true//Line/Mill e
    | Board(_,_,_,_,_,(_,CB,_,CB,_,CB,_),_) -> true //Line/Mill f
    | Board(_,_,_,_,_,_,(CB,_,_,CB,_,_,CB)) -> true //Line/Mill g
    | Board((CB,_,_,_,_,_,_),_,_,(CB,_,_,_,_,_,_),_,_,(CB,_,_,_,_,_,_)) -> true // Line/Mill Column 1
    | Board(_,(_,CB,_,_,_,_,_),_,(_,CB,_,_,_,_,_),_,(_,CB,_,_,_,_,_),_) -> true //Line/Mill Column 2
    | Board(_,_,(_,_,CB,_,_,_,_),(_,_,CB,_,_,_,_),(_,_,CB,_,_,_,_),_,_) -> true // Line/Mill Column 3
    | Board((_,_,_,CB,_,_,_),(_,_,CB,_,_,_,_),(_,_,CB,_,_,_,_),_,_,_,_) ->true //Line/Mill Column 4
    | Board(_,_,_,_,(_,_,_,CB,_,_,_),(_,_,_,CB,_,_,_),(_,_,_,CB,_,_,_)) ->true//Line/Mill Column 4
    | Board(_,_,(_,_,_,_,CB,_,_),(_,_,_,_,CB,_,_),(_,_,_,_,CB,_,_),_,_) -> true //Line/Mill Column 5
    | Board(_,(_,_,_,_,_,CB,_),_,(_,_,_,_,_,CB,_),_,(_,_,_,_,_,CB,_),_) ->true // Line/Mill Column 6
    | Board((_,_,_,_,_,_,CB),_,_,(_,_,_,_,_,_,CB),_,_,(_,_,_,_,_,_,CB)) -> true // Line/Mill Column 7
    | Board((CB,_,_,_,_,_,_), (_,CB,_,_,_,_,_), (_,_,CB,_,_,_,_),_,_,_,_) -> true // Line/Mill Diagonal A-C 
    | Board((_,_,_,_,_,_,CB), (_,_,_,_,_,CB,_), (_,_,_,_,CB,_,_),_,_,_,_) ->true //Line/Mill diagonal C-A
    | Board(_,_,_,_,(_,_,CB,_,_,_,_),(_,CB,_,_,_,_,_), (CB,_,_,_,_,_,_)) -> true //Line/Mill diagonal G-E
    | Board(_,_,_,_,(_,_,_,_,CB,_,_),(_,_,_,_,_,CB,_), (_,_,_,_,_,_,CB)) -> true //Line/Mill diagonal E-G
    | _ -> false
let MillWhite(game: GameBoard) = 
    match game with
    | Board((CW,_,_,CW,_,_,CW),_,_,_,_,_,_) -> true //Line a
    | Board(_,(_,CW,_,CW,_,CW,_),_,_,_,_,_) ->true //Line/Mill b
    | Board(_,_,(_,_,CW,CW,CW,_,_),_,_,_,_) ->true //Line/Mill c
    | Board(_,_,_,(CW,CW,CW,_,_,_,_),_,_,_) -> true //Line/Mill d
    | Board(_,_,_,(_,_,_,_,CW,CW,CW),_,_,_) -> true//Line/Mill d
    | Board(_,_,_,_,(_,_,CW,CW,CW,_,_),_,_) ->true//Line/Mill e
    | Board(_,_,_,_,_,(_,CW,_,CW,_,CW,_),_) -> true //Line/Mill f
    | Board(_,_,_,_,_,_,(CW,_,_,CW,_,_,CW)) -> true //Line/Mill g
    | Board((CW,_,_,_,_,_,_),_,_,(CW,_,_,_,_,_,_),_,_,(CW,_,_,_,_,_,_)) -> true // Line/Mill Column 1
    | Board(_,(_,CW,_,_,_,_,_),_,(_,CW,_,_,_,_,_),_,(_,CW,_,_,_,_,_),_) -> true //Line/Mill Column 2
    | Board(_,_,(_,_,CW,_,_,_,_),(_,_,CW,_,_,_,_),(_,_,CW,_,_,_,_),_,_) -> true // Line/Mill Column 3
    | Board((_,_,_,CW,_,_,_),(_,_,CW,_,_,_,_),(_,_,CW,_,_,_,_),_,_,_,_) ->true //Line/Mill Column 4
    | Board(_,_,_,_,(_,_,_,CW,_,_,_),(_,_,_,CW,_,_,_),(_,_,_,CW,_,_,_)) ->true//Line/Mill Column 4
    | Board(_,_,(_,_,_,_,CW,_,_),(_,_,_,_,CW,_,_),(_,_,_,_,CW,_,_),_,_) -> true //Line/Mill Column 5
    | Board(_,(_,_,_,_,_,CW,_),_,(_,_,_,_,_,CW,_),_,(_,_,_,_,_,CW,_),_) ->true // Line/Mill Column 6
    | Board((_,_,_,_,_,_,CW),_,_,(_,_,_,_,_,_,CW),_,_,(_,_,_,_,_,_,CW)) -> true // Line/Mill Column 7
    | Board((CW,_,_,_,_,_,_), (_,CW,_,_,_,_,_), (_,_,CW,_,_,_,_),_,_,_,_) -> true // Line/Mill Diagonal A-C 
    | Board((_,_,_,_,_,_,CW), (_,_,_,_,_,CW,_), (_,_,_,_,CW,_,_),_,_,_,_) ->true //Line/Mill diagonal C-A
    | Board(_,_,_,_,(_,_,CW,_,_,_,_),(_,CW,_,_,_,_,_), (CW,_,_,_,_,_,_)) -> true //Line/Mill diagonal G-E
    | Board(_,_,_,_,(_,_,_,_,CW,_,_),(_,_,_,_,_,CW,_), (_,_,_,_,_,_,CW)) -> true //Line/Mill diagonal E-G
    | _ -> false 

(*let makeMove symbol (Board (r1, r2,r3,r4,r5,r6,r7)) pos = 
      let newBoard = 
         let changeCol col (a,b,c,d,e,f,g) = 
            match col with 
            | 0 -> symbol,b,c,d,e,f,g
            | 1 -> a,symbol,c,d,e,f,g
            | 2 -> a,b,symbol,d,e,f,g
            | 3 -> a,b, c, symbol,e,f,g
            | 4 -> a,b,c,d,symbol,f,g
            | 5 -> a,b,c,d,e,symbol,g
            | 6 -> a,b,c,d,e,f,symbol
            | _ -> failwith "Error occured"
         let data = 
             match pos with
             | "A1" -> changeCol 0 r1, r2,r3,r4,r5,r6,r7  //1
             | "A4" -> changeCol 3 r1, r2,r3,r4,r5,r6,r7 //2
             | "A7" -> changeCol 6 r1, r2,r3,r4,r5,r6,r7 //3
             | "B2" -> r1, changeCol 1 r2,r3,r4,r5,r6,r7 //4
             | "B4" -> r1, changeCol 3 r2,r3,r4,r5,r6,r7 //5
             | "B6" -> r1, changeCol 5 r2,r3,r4,r5,r6,r7 //6 
             | "C3" -> r1, r2, changeCol 2 r3,r4,r5,r6,r7 //7
             | "C4" -> r1, r2, changeCol 3 r3,r4,r5,r6,r7 //8
             | "C5" -> r1, r2, changeCol 4 r3,r4,r5,r6,r7 //9
             | "D1" -> r1, r2,r3,changeCol 0 r4,r5,r6,r7 //10
             | "D2" -> r1, r2,r3, changeCol 1 r4,r5,r6,r7 //11
             | "D3" -> r1, r2,r3, changeCol 2 r4,r5,r6,r7 //12
             | "D5" -> r1, r2,r3, changeCol 4 r4,r5,r6,r7 //13
             | "D6" -> r1, r2,r3,changeCol 5 r4,r5,r6,r7 //14
             | "D7" -> r1, r2,r3, changeCol 6 r4,r5,r6,r7 //15
             | "E3" -> r1, r2,r3,r4, changeCol 2 r5,r6,r7 //16
             | "E4" -> r1, r2,r3,r4,changeCol 3 r5,r6,r7 //17
             | "E5" -> r1, r2,r3,r4,changeCol 4 r5,r6,r7 //18
             | "F2" -> r1, r2,r3,r4,r5, changeCol 1 r6,r7 //19
             | "F4" ->  r1, r2,r3,r4,r5,changeCol 3 r6,r7 //20
             | "F6" ->  r1, r2,r3,r4,r5, changeCol 5 r6,r7 //21
             | "G1" ->  r1, r2,r3,r4,r5,r6, changeCol 0 r7 //22
             | "G4" ->  r1, r2,r3,r4,r5, r6, changeCol 3 r7 //23
             | "G7" ->  r1, r2,r3,r4,r5,r6, changeCol 5 r7 //24
             | _ -> failwith "error occured in changing columns"
         Board data
      gamecheck newBoard
*)

let test = inputCheck "f6" 
//let test2 = isBlank ga
let listChars = ["a"; "b"; "c";"d";"e";"f"]
let coardinates index = [for i in 1.. 7-> string (listChars.[index]+string (i))]
//Dynamic List of all the possible coordinates, inputs will thus be matched to this list 
let actCoardinates = [for i in 0.. (listChars.Length-1) -> coardinates i]

// Tester method to run through the 2D array of input values...
let printOutValidCoordinates = 
          let rec innerHelp (input: string list list) index = 
              match input with
              | actCoardinates -> 
                   match (index) < 6 with
                   |true ->
                      let tmp = actCoardinates.[index]
                      for i in tmp do
                        printfn "%s status is: %b\t" i (inputCheck i)
                      printfn "\n"
                      innerHelp input (index+1)
                   | _ -> printfn "Done"
              //| [[]]-> printfn "Not correct input" 
          innerHelp actCoardinates 0  
                      



let makeMove symbol (Board (r1, r2,r3,r4,r5,r6,r7)) pos = 
      let newBoard = 
         let changeCol col (a,b,c,d,e,f,g) = 
            match col with 
            | 0 -> symbol,b,c,d,e,f,g
            | 1 -> a,symbol,c,d,e,f,g
            | 2 -> a,b,symbol,d,e,f,g
            | 3 -> a,b, c, symbol,e,f,g
            | 4 -> a,b,c,d,symbol,f,g
            | 5 -> a,b,c,d,e,symbol,g
            | 6 -> a,b,c,d,e,f,symbol
            | _ -> failwith "Error occured"
         let data = 
             match pos with
             | "A1" -> changeCol 0 r1, r2,r3,r4,r5,r6,r7
             | "A4" -> changeCol 3 r1, r2,r3,r4,r5,r6,r7
             | "A7" -> changeCol 6 r1, r2,r3,r4,r5,r6,r7
             | "B2" -> r1, changeCol 1 r2,r3,r4,r5,r6,r7
             | "B4" -> r1, changeCol 3 r2,r3,r4,r5,r6,r7
             | "B6" -> r1, changeCol 5 r2,r3,r4,r5,r6,r7
             | "C3" -> r1, r2, changeCol 2 r3,r4,r5,r6,r7
             | "C4" -> r1, r2, changeCol 3 r3,r4,r5,r6,r7
             | "C5" -> r1, r2, changeCol 4 r3,r4,r5,r6,r7
             | "D1" -> r1, r2,r3,changeCol 0 r4,r5,r6,r7
             | "D2" -> r1, r2,r3, changeCol 1 r4,r5,r6,r7
             | "D3" -> r1, r2,r3, changeCol 2 r4,r5,r6,r7
             | "D5" -> r1, r2,r3, changeCol 4 r4,r5,r6,r7
             | "D6" -> r1, r2,r3,changeCol 5 r4,r5,r6,r7
             | "D7" -> r1, r2,r3, changeCol 6 r4,r5,r6,r7
             | "E3" -> r1, r2,r3,r4, changeCol 2 r5,r6,r7
             | "E4" -> r1, r2,r3,r4,changeCol 3 r5,r6,r7
             | "E5" -> r1, r2,r3,r4,changeCol 4 r5,r6,r7
             | "F2" -> r1, r2,r3,r4,r5, changeCol 1 r6,r7
             | "F4" ->  r1, r2,r3,r4,r5,changeCol 3 r6,r7
             | "F6" ->  r1, r2,r3,r4,r5, changeCol 5 r6,r7
             | "G1" ->  r1, r2,r3,r4,r5,r6, changeCol 0 r7
             | "G4" ->  r1, r2,r3,r4,r5, r6, changeCol 3 r7
             | "G7" ->  r1, r2,r3,r4,r5,r6, changeCol 6 r7
             | _ -> failwith "error occured in changing columns"
         Board data
      Ongoing newBoard


let Shoot (position:string) (game:GameBoard) player = 
    let black = 
      match (MillBlack game) with 
      | true -> 
            match (isBlank game position) with
            | false -> makeMove player game position 
            | _ -> Ongoing game
      | _ -> Ongoing game
    let white = 
        match (MillWhite game) with 
          | true -> 
            match (isBlank game position) with
            | false -> makeMove player game position 
            | _ -> Ongoing game
          | _ -> Ongoing game
    white
let rec run player game =
    // need to find the blank cells that can be used...
    clearboard()
    printBoard game
    printfn "%A's turn.  Type the number of the cell that you want to play into." player
    let n = System.Console.ReadLine() // Co-ordinate for cow from user
    match n with
    | "A1" | "A4" | "A7" | "B2" | "B4" | "B6" | "C3" | "C4" | "C5"
    | "D1" | "D2" | "D3" | "D5" | "D6" | "D7" | "E3" | "E4" | "E5"
    | "F2" | "F4" | "F6" | "G1" | "G4" | "G7"  ->
           // let i = int (string n)
            match isBlank game n with
             | true -> makeMove player game n
             | _ ->
                  printfn "Invalid Input, Please re-enter position" 
                  run player game
                    
    | _ -> run player game
    
// Prints out the board based on row values
let rules() = printfn ("The game contains 3 stages
Stage 1: Cow placing
•	Each player has 12 pieces known as cows. Player 1 has dark cow and Player 2 has light cows
•	Player one moves first
•	Each turn consists of placing cows on the board 
•	Three cows on any line creates a mill
•	Whenever a player creates a mill they are to shoot any cow of the opponent accept cows in a mill.
•	One cow can be short per turn even if a turn creates more than one mill

Stage 2: Cow moving
•	After players run out of cows to place, each turn consists of moving cows to an empty adjacent intersection.
•	A mill allows a cow that is not in a mill to be short, unless all cows are in a mill
•	A mill can be broken and remade by moving cows back and forth.
•	A mill broken to make another mill can only be remade after the next turn

Stage 3: flying
•	Whenever a player has three cows left, the player’s cows are allowed to fly to any intersection not just the adjacent ones.
Finishing the game
•	A player wins when the opponent cannot make a move
•	A player wins when the opponent is left with 2 cows
•	When both players have three cows, they are allowed ten turns. If no shooting takes place it is declared a draw.
•	One that cheats loses the game. \n")

clearboard ()
rules()
printfn " Please press [P] to play the game!"
let getinput()  = (System.Console.ReadKey true).KeyChar
 


let rec runGame currentPlayer game =
    let playAgain () =
        printfn "Play again? [y/N] "
        match System.Console.ReadLine() with
        | "Y" | "y" -> runGame CB blankBoard
        | _ -> ()
    match run currentPlayer game with
    | Ongoing newBoard -> runGame (swapPlayer currentPlayer) newBoard
    | Winner (player, board) ->
        printBoard board
        printfn "Winner is %A" player
        playAgain ()
    | Draw ->
        printfn "Draaaaw"
        playAgain ()
(*let charcheck c =
     match c  with
     | 'A'| 'B'|'C'|'D'|'E'|'F'|'a'| 'b'|'c'|'d'|'e'|'f' -> true
     | _ -> false 
let b =6

//let placingcows input =

let msgPlacing = "Please enter the letter you want to place your cow at"
let msgMoving = "Please enter the letter you want to move your cow to"
let msgFlying = "Please enter the letter you want to fly your cow to"
let msgError = "Invalid output, please choose a different cell"   *)   
[<EntryPoint>]
let main argv = 
    //printfn "%A" argv
   (* let r = getinput ()
    match r with
       |'p'|'P' -> 
            clearboard()
            printBo*)
    //printBoard blankBoard
    runGame CB blankBoard
   // Console.Read()      

    0 // return an integer exit code