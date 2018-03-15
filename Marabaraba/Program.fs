open System
open System.Security.Cryptography.X509Certificates

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
type Player = //Does particular cell on the board hold white cow, black cow or is it black
| CW 
| CB 


type SuperPowerCow = 
|Basic of Player
|SuperSayin of Player
|Blank

type Tile =
      {
           pos: string
           cond: SuperPowerCow
      }

type GameBoard =  {
       Board: Tile list
       bullets: int*int
    }

type results =          //results state of the current game 
|Mill of GameBoard
|Winner of Player * GameBoard
|Ongoing of GameBoard
|Draw





let startBoard =
                 {  Board = [
                              {pos = "a1"; cond = Blank}; {pos = "a3"; cond = Blank}; {pos = "a7"; cond = Blank};
                              {pos = "b2"; cond = Blank}; {pos = "b4"; cond = Blank}; {pos = "b6"; cond = Blank};
                              {pos = "c3"; cond = Blank}; {pos = "c4"; cond = Blank}; {pos = "c5"; cond = Blank};
                              {pos = "d1"; cond = Blank}; {pos = "d2"; cond = Blank}; {pos = "d3"; cond = Blank};
                              {pos = "d5"; cond = Blank}; {pos = "d6"; cond = Blank}; {pos = "d7"; cond = Blank};
                              {pos = "e3"; cond = Blank}; {pos = "e4"; cond = Blank}; {pos = "e5"; cond = Blank};
                              {pos = "f2"; cond = Blank}; {pos = "f4"; cond = Blank}; {pos = "f6"; cond = Blank};
                              {pos = "g1"; cond = Blank}; {pos = "g4"; cond = Blank}; {pos = "g7"; cond = Blank};
                           ]
                    bullets = (12,12)
                 }

let mills = [
             ["a1"; "a2"; "a3"];
             ["b2"; "b4"; "b6"];
             ["c3";"c4";"c5"];
             ["d1";"d2";"d3"];
             ["d5";"d6"; "d7"];
             ["e3";"e4"; "e5"];
             ["f2";"f4"; "f6"];
             ["g1";"g4"; "g7"];
             ["a1";"d1"; "g1"];
             ["b2";"d2"; "f2"];
             ["c3";"d3"; "e3"];
             ["a4";"b4"; "c4"];
             ["e4";"f4"; "g4"];
             ["c5";"d5"; "e5"];
             ["b6";"d6"; "f6"];
             ["a7";"d7"; "g7"];                                                 
             ["a1";"b2"; "c3"];
             ["a7";"b6"; "c5"];
             ["g1";"f2"; "e3"];
             ["g7";"f6"; "e5"];
            ]
let player =  CB

let PlayerToString value = 
       match value with 
          | CB -> "B"
          | CW -> "W"
          | _ -> "O"


let conditionToString value =
    match value with
    | Blank -> "O"
    | Basic CB -> "B"
    | Basic CW -> "W"
    | SuperSayin CB -> "B"
    | SuperSayin CW -> "W"
    | _ -> ""
//FUNCTIONS
//Fix the method to print out the board
let printBoard () = //printing the board onto the screen. as the user will see it.
      //System.Console.Clear()
      let liz = "_____" //5
      let liz2 = "____" //4
      let bk = "     " //5
      let bk2 = "    "//4
      let printSep1 () = printfn  "     |               |               |\n     |               |               |\n     |               |               |" //board design 
      let printSep2 () = printfn  "     |         |           |         |\n     |         |           |         |\n     |         |           |         |"
 
     
      printfn "     %d%s%d%s%d%s%d%s%d%s%d%s%d " 1 bk2 2 bk2 3 bk 4 bk 5 bk2 6 bk2 7  // prints out the number scale at the top of the board
      printfn "\n"
      // rest of the methods called prints out the board, one line at a time.
      printfn "A    %s%s%s%s%s%s%s%s%s%s%s%s%s " (conditionToString (startBoard.Board.[0].cond)) liz ("") liz ("") liz (conditionToString (startBoard.Board.[1].cond)) liz ("") liz ("") liz (conditionToString (startBoard.Board.[2].cond))
      printSep1()
      printfn "B    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") liz (conditionToString (startBoard.Board.[3].cond)) liz ("") liz (conditionToString (startBoard.Board.[4].cond)) liz ("") liz (conditionToString (startBoard.Board.[5].cond)) liz ("") 
      printSep1()
      printfn "C    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") bk ("") bk ((conditionToString (startBoard.Board.[6].cond))) liz (conditionToString (startBoard.Board.[7].cond)) liz (conditionToString (startBoard.Board.[8].cond)) bk ("") bk ("")
      printSep2()
      printfn "D    %s%s%s%s%s%s%s%s%s%s%s%s%s " (conditionToString (startBoard.Board.[9].cond)) liz2 (conditionToString (startBoard.Board.[10].cond)) liz2 (conditionToString (startBoard.Board.[11].cond)) bk bk (" ") (conditionToString (startBoard.Board.[12].cond)) liz2 ((conditionToString (startBoard.Board.[13].cond)) ) liz2 ((conditionToString (startBoard.Board.[14].cond)) )
      printSep2()
      printfn "E    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") bk ("") bk (conditionToString (startBoard.Board.[15].cond)) liz (conditionToString (startBoard.Board.[16].cond)) liz ((conditionToString (startBoard.Board.[17].cond))) bk ("") bk ("")
      printSep1()
      printfn "F    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") liz (conditionToString (startBoard.Board.[18].cond)) liz ("") liz (conditionToString (startBoard.Board.[19].cond)) liz ("") liz (conditionToString (startBoard.Board.[20].cond)) liz ("")
      printSep1()
      printfn "G    %s%s%s%s%s%s%s%s%s%s%s%s%s " (conditionToString (startBoard.Board.[21].cond)) liz ("") liz ("") liz (conditionToString (startBoard.Board.[22].cond)) liz ("") liz ("") liz (conditionToString (startBoard.Board.[23].cond))
      let printSepConners () = printfn "|\%s|%s/|" bk bk    
      printfn ""
         



let clearboard()=System.Console.Clear()

let swapPlayer x= 
      match x with 
      | CB -> CW
      | CW -> CB
      | _ -> failwith "A FATAL ERROR OCCURED"

let inputCheck (coordinate:string) =       //validating user input
      let n = coordinate.ToUpper ()
      match String.length (n) with
      | 2 -> 
            match Char.IsLetter(n.[0]) && n.[0]<'g' with 
            | true -> 
                  match Char.IsDigit(n.[1]) &&  ((Int32.Parse(string n.[1]) >= 0) && Int32.Parse(string n.[1]) < 8)  with
                      | true -> true
                      | _ -> false
            | _ -> false
      | _ -> false

//Check the the opposition 
let getEnemy enemy =
     match enemy  with
     | CB -> CW
     | CW-> CB 

// Write out a method to update a certain tile and return a new board with all values
   //Unlike imperative programming, you can't modify values so just make a new board work with it

let UpdateCell (theBoard: GameBoard) pos conditionState = 
          let mapCelltoCell (obj) =  
                match obj.pos = pos with
                | true -> {obj with cond=conditionState}
                | false -> obj
          let replaceState = (List.map (mapCelltoCell) theBoard.Board)
          {theBoard with Board = replaceState}

let CBList, CWList = List.partition (fun player -> player.cond = Basic CB) startBoard.Board 

let ObtainCurrAvailPlayerSpaces board player = 
     let filteredList = List.filter ( fun input -> input.cond = Basic (player)) board
     filteredList

let rec obtainInput input cleanBoard =                      //not gonna use it
      printfn "Input co-ordinate please: %s" input
      let value = Console.ReadLine ()
      match (inputCheck value) with
      | true -> cleanBoard ()
                 value
      | false -> printfn "Please re-enter co-ordinate"
                 obtainInput input  cleanBoard

let checkCellsAroundPosition pos = 
          match pos with 
          | "a1" -> ["d1"; "b2"; "a4"]
          | "a4" -> ["b3"; "a7";  "a1"]
          | "a7" -> ["d7"; "a4";  "b6"]

          | "b4" -> ["a1"; "c7";  "b4"; "d2"]
          | "b5" -> ["b4"; "b6";  "b2"; "c8"]
          | "b6" -> ["b3"; "c5"; "d6"; "a7" ]

          | "c3" -> ["b2"; "c4"; "d3"]
          | "c4" -> ["c3"; "b4"; "c5"]
          | "c5" -> ["c4";"d5";"b6"]
          
          | "d1" -> ["a1"; "g1";"d2"]
          | "d2" -> ["d1"; "f2"; "d3"; "b2"]
          | "d3" -> ["d2"; "e3"; "c3"]

          | "d5" -> ["e5";"d6"; "c5"]
          | "d6" -> ["d5"; "f6";"b6";"d7"]
          | "d7" -> ["d6"; "g7"; "a7"]

          |"e3" -> ["d3";"f2";"e4" ]
          | "e4"-> ["e3";"f4" ;"e5"]
          | "e5" -> ["e4";"f6";"d5"]

          | "f2" -> ["g1"; "f4";"e3"; "d2"]
          |"f4" -> ["f2"; "g4"; "f6"; "e4"]
          | "f6" -> ["f4"; "g7"; "d6"; "e5"]

          |"g1" -> ["d1"; "g4";"f2"]
          |"g4" -> ["g1"; "f4"; "g7"]
          | "g7" -> ["g4"; "f6"; "d7"]

          | _ -> failwith "Invalid position entered"

let tiletostring (tile : Tile) =
    let ss = tile.pos 
    let s  = conditionToString tile.cond
    ss + s
    

let findState (board: Tile list) position =
          let listGetStateItems = (List.tryFind(fun actItem -> actItem.pos = position) board).Value.cond
          listGetStateItems 
          

//let boardtostring board = 
    
        
let destroycow board =
    printfn "Please enter posision of cow you want to chow" 
    let n = Console.ReadLine()
    match inputCheck n with 
    |true -> 
        let CBList, CWList = List.partition (fun player -> player.cond = Basic CB) startBoard.Board 
        let currentplayer =  player //still to come
        let deslist = 
             match currentplayer with
             |CB -> CWList
             |CW -> CBList
             |_ ->[]
        match List.exists((=) n) (List.map conditionToString (List.map (fun (x:Tile) -> x.cond) (deslist))) with
        |true -> UpdateCell board n Blank 
        |_ -> UpdateCell board n Blank //

    |_ -> UpdateCell board n Blank //
// Write out a method to update a certain tile and return a new board with all values
   //Unlike imperative programming, you can't modify values so just make a new board work with it

let UpdateCell (theBoard: GameBoard) pos conditionState = 
          let mapCelltoCell (obj) =  
                match obj.pos = pos with
                | true -> {obj with cond=conditionState}
                | false -> obj
          let replaceState = (List.map (mapCelltoCell) theBoard.Board)
          {theBoard with Board = replaceState}
     
            
let placeTile (board: GameBoard) pos newState = ""


(*let inMills inPos = 
      let getListOfNeighbours = checkCellsAroundPosition inPos
      let lenOfNeigh = List.length getListOfNeighbours
      match lenOfNeigh = 3 with 
      |  -> true
      | mills.Item 1 -> true
      | mills. *)

let makeMove player pos game =
    let newBoard = UpdateCell (game) pos (Basic player)

    newBoard

let rec run player game  =
   // need to find the blank cells that can be used...
   //clearboard()
   //printBoard game                      
  // run player game// cows
   let playAgain () =
       printfn "Play again? [y/N] "
       match System.Console.ReadLine() with
       | "Y" | "y" -> run CB startBoard
       | _ -> ()   
   printfn "%A's turn.  Type the Co-ordinates [<LETTER><NUMBER>] of the cell that you want to play into." (swapPlayer CW)

   let b = System.Console.ReadLine() // Co-ordinate for cow from user
   let n = b.ToLower ()
       //updateplayer player n
   match n with
   | "a1"  | "a4" | "a7"  | "b2" |"b4"  | "b6"  | "c3" | "c4" | "c5"  | "d1" | "d2" | "d3" | "d5" | "d6" | "d7" | "e3" | "e4" | "e5" | "f2" | "f4" | "f6" | "g1" | "g4" | "g7"  ->
      match (findState game.Board n) with
       | Blank ->
            // printfn "Invalid Input, Please re-enter position"
             let value =  makeMove player n game    //Shoot n game player
             let (WPieces, BPieces) = value.bullets
             let output =
              match player with
                | CW -> ({value with bullets = (WPieces-1,BPieces)}, n)
                | CB -> ({value with bullets = (WPieces,BPieces-1)}, n)
             clearboard()
             let (board,pos) = output
             printBoard()
             run (swapPlayer player) board  
       | Basic player -> 
            match run player game with
            |Mill GameBoard ->
                match checkmill pos GameBoard with
                |true ->
                
                    printfn "Enter position of cow you'd like to destroy [<Letter><Number>]: "
                    let var = destroycow GameBoard
                    run (swapPlayer player) var
                |_ -> Ongoing GameBoard
            |Ongoing GameBoard->
                updateplayer currentPlayer n
                run (swapPlayer player) newBoard

            |Winner (player, Gameboard) ->
               printBoard ()
               printfn "Winner is %A" player
               playAgain ()
            |Draw ->
               printfn "Draaaaw"
               playAgain ()
       | SuperSayin player ->
            printfn "Invalid Input, Please re-enter position"
            run player game //cows
             
   | _ -> run player game //cows
// Prints out the board based on row values
 //Write a method to  print out the board
    // Given the data structure at the top i.e board, generate a board gui
let buildboard Board =
    Console.Clear()
    List.map(fun x updatecell -> updatecell x) Board
    printBoard ()




// Write a method to check if a mill has been formed 
let checkmill pos (board: string list list) =
   let a = List.tryFind (fun xs -> xs = pos::_::_)  board
   let b = List.tryFind ((=) (x::pos::y)) board
   let c = List.tryFind ((=) (x::y::pos)) board
   match (Some a = [] )|| (Some b = [] )|| (Some c = []) with
   |true -> true
   | _ -> false

    
// Write a BoardtoString Method that converts any given board to a string, and return it

// 

// Tester method to run through the 2D array of input values...
(*let printOutValidCoordinates = 
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
          innerHelp mil 0  
*)

(*
let rec run player game  =
    // need to find the blank cells that can be used...
    clearboard()
    printBoard game                      
    run player game// cows 
    printfn "%A's turn.  Type the Co-ordinates [<LETTER><NUMBER>] of the cell that you want to play into." player

    let b = System.Console.ReadLine() // Co-ordinate for cow from user
    let n = b.ToUpper ()
        //updateplayer player n
    match n with
    | "A1"  | "A4" | "A7" | "a7" | "B2" |"B4"  | "B6"  | "C3" | "C4" | "C5"  | "D1" | "D2" | "D3" | "D5" | "D6" | "D7" | "E3" | "E4" | "E5" | "F2" | "F4" | "F6" | "G1" | "G4" | "G7"  ->
       match isBlank game n with
        | true -> 
              printfn "Invalid Input, Please re-enter position" 
              makeMove player game   //Shoot n game player    
        | _ ->
             printfn "Invalid Input, Please re-enter position" 
             run player game //cows
               
    | _ -> run player game //cows
// Prints out the board based on row values*)

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
 


(*let rec runGame currentPlayer game  =
    let playAgain () =
        printfn "Play again? [y/N] "
        match System.Console.ReadLine() with
        | "Y" | "y" -> runGame CB blankBoard
        | _ -> ()
 
    match run currentPlayer game with
    |Ongoing newBoard -> 
         //updateplayer currentPlayer n 
         runGame (swapPlayer currentPlayer) newBoard 
    |Mill newBoard ->
         printfn "Enter position of cow you'd like to destroy [<Letter><Number>]: "
         let var = DestroyPiece newBoard 
         runGame (swapPlayer currentPlayer) var*)
(*let rec runGame currentPlayer game   =
    let playAgain () =
        printfn "Play again? [y/N] "
        match System.Console.ReadLine() with
        | "Y" | "y" -> runGame CB blankBoard //(cows-1)
        | _ -> ()

        match run currentPlayer game  with
        |Ongoing newBoard -> runGame (swapPlayer currentPlayer) newBoard //(cows-1)
        |Mill newBoard ->
             printfn "Enter position of cow you'd like to destroy: "
             let var = DestroyPiece newBoard 
             runGame (swapPlayer currentPlayer) var  //(cows-1)

        |Winner (player, board) ->
            printBoard board
            printfn "Winner is %A" player
            playAgain ()
        |Draw ->
           printfn "Draaaaw"
           playAgain ()
  

//let placingcows input =

let msgPlacing = "Please enter the letter you want to place your cow at"
let msgMoving = "Please enter the letter you want to move your cow to"
let msgFlying = "Please enter the letter you want to fly your cow to"
let msgError = "Invalid output, please choose a different cell"   *)

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let r = getinput ()
    match r with
       |'p'|'P' -> 
            clearboard()
            printBoard ()
            run CB startBoard   //start with 24 cows, 12 for each. when this value reaches 0, go from placing to moving
       | _ -> rules ()
            
    //printBoard blankBoard
    
    Console.Read()      
    0