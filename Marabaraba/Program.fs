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
| Cow_White
| Cow_Black
| Blank

type GameBoard = 
| Board of (Cell*Cell*Cell*Cell*Cell*Cell*Cell)
           *(Cell*Cell*Cell*Cell*Cell*Cell*Cell)
           *(Cell*Cell*Cell*Cell*Cell*Cell*Cell)
           *(Cell*Cell*Cell*Cell*Cell*Cell*Cell)
           *(Cell*Cell*Cell*Cell*Cell*Cell*Cell)
           *(Cell*Cell*Cell*Cell*Cell*Cell*Cell)
           *(Cell*Cell*Cell*Cell*Cell*Cell*Cell)

 type States =
 |Placing of (char*int) * (char*int)
 |Moving of (char*int) * string list
 |Flying of (char*int) * (char * int)

let playerB = {Cows = [();();();();();();();();();();();();();()] ; Turn = false}
let playerW = {Cows = [();();();();();();();();();();();();();()] ; Turn = false}

type Game=
       { A : char; B : char; C : char; D : char; E : char; F : char; G : char; H : char; I : char; J : char; K : char; L : char; M : char; N : char; O : char; P : char;
           Q : char; R : char;  S : char; T : char; U : char; V : char; W : char; X : char }

let Game = { A = 'A';  B = 'B'; C = 'C';D = 'D';E = 'E';F = 'F';G = 'G';H = 'H';I = 'I';J = 'J';K = 'K';L = 'L';M = 'M';N = 'N';O = 'O';P = 'P';Q = 'Q';R = 'R';S = 'S';T = 'T';U = 'U';
           V = 'V';W = 'W';X = 'X';}
//type Result 
let swapPlayer x= 
      match x with 
      | Cow_Black -> Cow_White
      | Cow_White -> Cow_Black
      | Blank -> failwith "A FATAL ERROR OCCURED"
let clearboard()=System.Console.Clear()
let blankBoard =
    let blankRow = Blank, Blank, Blank, Blank, Blank, Blank, Blank
    Board (blankRow, blankRow, blankRow, blankRow, blankRow, blankRow, blankRow)
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
let test = inputCheck "f6" 
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
              | [[]]-> printfn "Not correct input" 
          innerHelp actCoardinates 0  
                      
(*let BoardCoordinates = 
               [for i in 0 .. 6 do 
                    for j in 0 .. 6 do ->
                                 let input = string (listChars.[i]+)
                      ]*)

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
printfn" Please press [P] to play the game!"
let getinput()  = (System.Console.ReadKey true).KeyChar
let printBoard (Board (r1, r2, r3, r4, r5, r6, r7)) =
      //System.Console.Clear()
      let liz = "_____" //5
      let liz2 = "____" //4
      let bk = "     " //5
      let bk2 = "    "//4
      let printSep1 () = printfn  "     |               |               |\n     |               |               |\n     |               |               |"
      let printSep2 () = printfn  "     |         |           |         |\n     |         |           |         |\n     |         |           |         |"
      let cell offset value n = 
          match value with 
          | Cow_Black -> "B"
          | Cow_White -> "W"
          | Blank -> string (offset+n)
 
      
      let printRow (c1,c2,c3,c4,c5,c6,c7) offset = 
           
           
           printfn "The status is %b" test // Test example of input testing method
           printOutValidCoordinates // Test example of whether valid an input is 
           printfn "The coordinates are %A" (actCoardinates)
           let cell = cell offset
           //let sym = "O"
           printfn "     %d%s%d%s%d%s%d%s%d%s%d%s%d " 1 bk2 2 bk2 3 bk 4 bk 5 bk2 6 bk2 7  // prints out the number scale at the top of the board
           printfn "\n"
           // rest of the methods called prints out the board, one line at a time.
           printfn "A    %s%s%s%s%s%s%s%s%s%s%s%s%s " (cell c1 "A") liz ("") liz ("") liz (cell c3 "B") liz ("") liz ("") liz (cell c7 "C") 
           printSep1()
           printfn "B    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") liz (cell c2 "D") liz ("") liz (cell c3 "E") liz ("") liz (cell c3 "F") liz ("") 
           printSep1()
           printfn "C    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") bk ("") bk (cell c3 "G") liz (cell c3 "H") liz (cell c3 "I") bk ("") bk ("")
           printSep2()
           printfn "D    %s%s%s%s%s%s%s%s%s%s%s%s%s " (cell c1 "J") liz2 (cell c2 "K") liz2 (cell c3 "L") bk bk (" ") (cell c3 "M") liz2 (cell c3 "N") liz2 (cell c3 "O")
           printSep2()
           printfn "E    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") bk ("") bk (cell c3 "P") liz (cell c3 "Q") liz (cell c3 "R") bk ("") bk ("")
           printSep1()
           printfn "F    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") liz (cell c2 "S") liz ("") liz (cell c3 "T") liz ("") liz (cell c3 "U") liz ("")
           printSep1()
           printfn "G    %s%s%s%s%s%s%s%s%s%s%s%s%s " (cell c1 "V") liz ("") liz ("") liz (cell c3 "W") liz ("") liz ("") liz (cell c7 "X") 
      let printSepConners () = printfn "|\%s|%s/|" bk bk    
      let printSep () = printfn "---+---+---+---+---+---+---+"
      printRow r1 "" 


let charcheck c =
     match c  with
     | 'A'| 'B'|'C'|'D'|'E'|'F'|'a'| 'b'|'c'|'d'|'e'|'f' -> true
     | _ -> false 
let b =6

//let placingcows input =

let msgPlacing = "Please enter the letter you want to place your cow at"
let msgMoving = "Please enter the letter you want to move your cow to"
let msgFlying = "Please enter the letter you want to fly your cow to"
let msgError = "Invalid output, please choose a different cell"      

[<EntryPoint>]
let main argv = 
    //printfn "%A" argv
    let r = getinput ()
    match r with
       |'p'|'P' -> 
            clearboard()
            printBoard blankBoard
       | _ -> ()
    //printBoard blankBoard
    Console.Read()      

    0 // return an integer exit code