open System
//open System.Security.Cryptography.X509Certificates

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

//DATA STRUCTURES
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

type GameBoard = {
       Board: Tile list
       bullets: int*int
    }

type results =          //results state of the current game 
|Mill of GameBoard
|Winner of Player * GameBoard
|Ongoing of GameBoard
|Draw

type Mill = 
| Available
| Used

type MillValues = 
  {
    indx: int 
    MillV: string list
 }





 //iNITIAL BOARD, 
let board =
                 {  Board = [
                              {pos = "a1"; cond = Blank}; {pos = "a4"; cond = Blank}; {pos = "a7"; cond = Blank};
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


let player = CB

let PlayerToString value = 
       match value with 
          | CB -> "B"
          | CW -> "W"
         


let conditionToString value =
    match value with
    | Blank -> "O"
    | Basic CB -> "B"
    | Basic CW -> "W"
    | SuperSayin CB -> "B"
    | SuperSayin CW -> "W"
let conditionToString2 value =
    match value with
    | Blank -> "O"
    | Basic CB -> "B"
    | Basic CW -> "W"
    | _ -> ""
//FUNCTIONS
// A function to print out any given board
let printBoard (board:GameBoard) = //printing the board onto the screen. as the user will see it.
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
      printfn "A    %s%s%s%s%s%s%s%s%s%s%s%s%s " (conditionToString (board.Board.[0].cond)) liz ("") liz ("") liz (conditionToString (board.Board.[1].cond)) liz ("") liz ("") liz (conditionToString (board.Board.[2].cond))
      printSep1()
      printfn "B    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") liz (conditionToString (board.Board.[3].cond)) liz ("") liz (conditionToString (board.Board.[4].cond)) liz ("") liz (conditionToString (board.Board.[5].cond)) liz ("") 
      printSep1()
      printfn "C    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") bk ("") bk ((conditionToString (board.Board.[6].cond))) liz (conditionToString (board.Board.[7].cond)) liz (conditionToString (board.Board.[8].cond)) bk ("") bk ("")
      printSep2()
      printfn "D    %s%s%s%s%s%s%s%s%s%s%s%s%s " (conditionToString (board.Board.[9].cond)) liz2 (conditionToString (board.Board.[10].cond)) liz2 (conditionToString (board.Board.[11].cond)) bk bk (" ") (conditionToString (board.Board.[12].cond)) liz2 ((conditionToString (board.Board.[13].cond)) ) liz2 ((conditionToString (board.Board.[14].cond)) )
      printSep2()
      printfn "E    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") bk ("") bk (conditionToString (board.Board.[15].cond)) liz (conditionToString (board.Board.[16].cond)) liz ((conditionToString (board.Board.[17].cond))) bk ("") bk ("")
      printSep1()
      printfn "F    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") liz (conditionToString (board.Board.[18].cond)) liz ("") liz (conditionToString (board.Board.[19].cond)) liz ("") liz (conditionToString (board.Board.[20].cond)) liz ("")
      printSep1()
      printfn "G    %s%s%s%s%s%s%s%s%s%s%s%s%s " (conditionToString (board.Board.[21].cond)) liz ("") liz ("") liz (conditionToString (board.Board.[22].cond)) liz ("") liz ("") liz (conditionToString (board.Board.[23].cond))
      printfn ""
         



let clearboard()=System.Console.Clear()
//Change player
let swapPlayer x= 
      match x with 
      | CB -> CW
      | CW -> CB
     
//validating user input
let inputCheck (coordinate:string) =       
      let n = coordinate.ToUpper ()
      match String.length (n) with
      | 2 -> 
            match Char.IsLetter(n.[0]) && n.[0]<'g' with 
            | true -> 
                  match Char.IsDigit(n.[1]) && ((Int32.Parse(string n.[1]) >= 0) && Int32.Parse(string n.[1]) < 8) with
                      | true -> true
                      | _ -> false
            | _ -> false
      | _ -> false


//let CBList, CWList = List.partition (fun player -> player.cond = Basic CB) board.Board 
     
// Write out a method to update a certain tile and return a new board with all values
   //Unlike imperative programming, you can't modify values so just make a new board work with it
// Update a given cell in the gameboard with that's cell's new codition state and return a new board
let UpdateCell (theBoard: GameBoard) pos conditionState = 
          let mapCelltoCell (obj:Tile) =  
                match obj.pos = pos with
                | true -> {obj with cond=conditionState}
                | false -> obj
          let replaceState = (List.map (mapCelltoCell) theBoard.Board)
          {theBoard with Board = replaceState}
//Functions that filter all the basic pieces into two lists.
let returnListBlack (board:GameBoard) = List.filter (fun player -> player.cond = Basic CB) board.Board 
let returnListWhite (board:GameBoard) = List.filter (fun player -> player.cond = Basic CW) board.Board 

// Update the remaining pieces, of a player! once it reaches 3 pieces 
let updateBoardToSuperSayin (board:GameBoard) player (condition:SuperPowerCow) =
      let stringVal = conditionToString2 player
      let mapCelltoCell (obj) =  
                match (obj.cond <> Blank && stringVal = "B") || (obj.cond <> Blank && stringVal = "W") with
                | true -> {obj with cond=condition}
                | false -> obj
      let newState = List.map(mapCelltoCell) board.Board 
      {board with Board = newState}

 //CHECK ALL THE POSITION THAT COW CAN REACH FROM CURRENT POSITION
let checkCellsAroundPosition pos = 
          match pos with 
          | "a1" -> ["d1"; "b2"; "a4"]
          | "a4" -> ["b3"; "a7"; "a1"]
          | "a7" -> ["d7"; "a4"; "b6"]

          | "b4" -> ["a1"; "c7"; "b4"; "d2"]
          | "b5" -> ["b4"; "b6"; "b2"; "c8"]
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

          | _ -> []


// Find the state of any given position from the board
let findState (board: Tile list) position =
          let listGetStateItems = (List.tryFind(fun actItem -> actItem.pos = position) board).Value.cond
          listGetStateItems 
          
//let findMillState = 
      
//let boardtostring board = 
    
 // Break the white and black cows into two lists   
 // Destroy any given cow, and return a new board
let destroycow board =
    printfn "Please enter posision of cow you want to chow" 
    let n = Console.ReadLine()
    match inputCheck n with 
    |true -> 
        let CBList, CWList = List.partition (fun player -> player.cond = Basic CB) board.Board 
        let currentplayer = player //still to come
        let deslist = 
             match currentplayer with
             |CB -> CWList
             |CW -> CBList
         
        match List.exists((=) n) (List.map conditionToString (List.map (fun (x:Tile) -> x.cond) (deslist))) with
        |true -> UpdateCell board n Blank 
        |_ -> UpdateCell board n Blank //

    |_ -> UpdateCell board n Blank //

// Make the move based on the position and return a new board 
let makeMove player pos game =
    let newBoard = UpdateCell (game) pos (Basic player)

    newBoard
// Destroy a piece based on position and return a new board
let DestroyPiece pos game =
    let newBoard = UpdateCell (game) pos (Blank)
    
    newBoard

      
//sO THAT YOU DO NOT DESTROY YOUR ON COW          
let notown (board : GameBoard) (player: Player) pos =  
    let value = findState board.Board pos  
    let cond = conditionToString (value)
    let playerCon = PlayerToString player
    let funx =         
            match cond = playerCon with
            |true -> false
            |_ -> true
    funx 
  

// Write a method to check if a mill has been formed 
let checkMill1 pos =
    let mapMill1 = 
         match pos with 
         | "d1" -> ["a1"; "d1";"g1"]
         | "a1" -> ["a1";"a4"; "a7"]
         | "a7" -> ["a7"; "b6";"c5"]
         | "c3" -> ["c3";"c4";"c5" ]
         | "d3" -> ["c3";"d3";"e3"]
         | "g1" -> ["g1"; "f2"; "e3"]
         | "b2" -> ["b2";"d2"; "f2"]
         | "e4" -> ["e4";"f4"; "g4"]
         | "e5" -> ["c5";"d5"; "e5"]
         | "g7"-> ["g7";"f6"; "e5"]
         | _ -> []
    mapMill1
let checkMill2 pos = 
        let mapMill2 = 
             match pos with 
             | "a1" -> ["a1"; "b2";"c3"]
             | "a4" -> ["a4";"b4";"c4" ]
             | "b2" -> ["b2"; "b4";"b6"]
             | "b6" -> ["b6"; "d6";"f6"]
             | "d1" -> ["d1";"d2"; "d3"]
             | "d5" -> ["d5";"d6";"d7"]
             | "e3" -> ["e3";"e4";"e5"]
             | "f2" -> ["f2"; "f4"; "f6"]
             | "g1" -> ["g1"; "g4"; "g7"]
             | "g7" -> ["a7";"d7"; "g7"]
             | _ -> []
        mapMill2
let allBoardCoordinates = 
      ["a1"; "a4"; "a7"; "b2"; "b4"; "b5"; "c3";"c4";"c5";"d1";"d2";"d3";"d5";"d6";"d7";
        "e3"; "e4"; "e5"; "f2";"f4"; "f5";"g1";"g4";"g7"]
let allNeededPoints1 = 
      ["d1";"a1";"a7";"c3";"d3";"g1";"b2";"e4";"e5";"g7"]
let allNeededPoints2 = 
       ["a1"; "a4";"b2";"b6";"d1";"d5";"e3"; "f2"; "g1";"g7"]
// This keeps track of all mills that'll be formed, and once it's formed it's status will change to true 
let helperList (listN:string list) (listM:string list list) = 
              match (List.length listM > 0) with
               | true -> listN::listM
               | _ -> listM
       
let rec AccountForMill1 (board:GameBoard) n (listM:string list list) =
     let possibleMill1 = checkMill1 allNeededPoints1.[n]
     //let posssibleMill2 = checkMill2 pos
     
     let a = findState board.Board possibleMill1.[0]
     let b = findState board.Board possibleMill1.[1]
     let c = findState board.Board possibleMill1.[2]
    
      
     match (a,b,c) with
     | Basic CW, Basic CW, Basic CW -> 
                               
                                      match List.exists((=)possibleMill1) listM with //listM.Head = possibleMill1  with 
                                      | true ->  (board, listM)
                                      | _ -> 
                                        printfn "Which CB Enemy would you like to eliminate"
                                        let input = Console.ReadLine()
                                        match (notown board player input) with 
                                        |false -> printfn "Can't destroy own cow, please re-enter which enemy cow you'd like to destroy"
                                                  AccountForMill1 board n (listM:string list list)
                                        |true -> 
                                            (DestroyPiece  input board), (helperList possibleMill1 listM)
                                        

     | Basic CB, Basic CB, Basic CB -> 
                                       match List.exists((=)possibleMill1) listM  with 
                                       | true ->  (board, listM)
                                       | _ -> 
                                        printfn "Which CW Enemy would you like to eliminate"
                                        let input = Console.ReadLine()
                                        match (notown board player input) with 
                                        |false -> printfn "Can't destroy own cow, please re-enter which enemy cow you'd like to destroy"
                                                  AccountForMill1 board n (listM:string list list)
                                        |true -> 
                                            (DestroyPiece  input board), (helperList possibleMill1 listM)

     | _ -> 
            let flag =   (List.length allNeededPoints1-2) >= n
            match (flag) with
            |true -> (AccountForMill1 board (n+1) listM)
            | _ -> (board,listM)

let rec AccountForMill2  (board:GameBoard) n (listM:string list list)=
     let possibleMill1 = checkMill2 allNeededPoints2.[n]
     //let posssibleMill2 = checkMill2 pos
     let a = findState board.Board possibleMill1.[0]
     let b = findState board.Board possibleMill1.[1]
     let c = findState board.Board possibleMill1.[2]
     
     match (a, b,c) with
     | Basic CW, Basic CW, Basic CW -> 
                             match List.exists((=)possibleMill1) listM  with 
                             | true ->  (board, listM)
                             | _ -> 
                               printfn "Which CB Enemy would you like to eliminate"
                               let input = Console.ReadLine()
                               match (notown board player input) with 
                               |false -> printfn "Can't destroy own cow, please re-enter which enemy cow you'd like to destroy"
                                         AccountForMill2 board n (listM:string list list)
                               |true -> 
                                   (DestroyPiece  input board), (helperList possibleMill1 listM)




     | Basic CB, Basic CB, Basic CB -> 
                            match List.exists((=)possibleMill1) listM with 
                            | true ->  (board, listM)
                            | _ -> 
                                 printfn "Which CW Enemy would you like to eliminate"
                                 let input = Console.ReadLine()
                                 match (notown board player input) with 
                                 |false -> printfn "Can't destroy own cow, please re-enter which enemy cow you'd like to destroy"
                                           AccountForMill2 board n (listM:string list list)
                                 |true -> 
                                     (DestroyPiece  input board), (helperList possibleMill1 listM)





     | _ -> 
           let flag =   (List.length allNeededPoints1-2) >= n
           match (flag) with
           |true -> (AccountForMill2 board (n+1) listM)
           | _ -> (board,listM)

//SO THAT YOU DO NOT REMOVE OPPONENT'S COW
let own (board : GameBoard) (player: Player) pos =  
    let value = findState board.Board pos  
    let cond = conditionToString (value)
    let playerCon = PlayerToString player
    let funx =         
            match cond = playerCon with
            |true -> true
            |_ -> false
    funx 
let removeCow board n =
    match inputCheck n with 
    |true -> 
        let CBList, CWList = List.partition (fun player -> player.cond = Basic CB) board.Board 
        let currentplayer =  player //still to come
        let deslist = 
             match currentplayer with
             |CB -> CBList
             |CW -> CWList
         
        match List.exists((=) n) (List.map conditionToString (List.map (fun (x:Tile) -> x.cond) (deslist))) with
        |true -> UpdateCell board n Blank 
        |_ -> UpdateCell board n Blank
    | _ -> board
let rec moveCows (board:GameBoard) posCur posMoveTo player = 
  
    let getNeighbourCells = checkCellsAroundPosition posCur
    match List.exists ((=) posMoveTo) getNeighbourCells with 
    |true -> makeMove player posMoveTo board
    |_-> moveCows (board:GameBoard) posCur posMoveTo player

let rec FlyCows (board:GameBoard) posCur posMoveTo player = 
  
    //let getNeighbourCells = checkCellsAroundPosition posCur
    match List.exists ((=) posMoveTo) allBoardCoordinates with 
    |true -> makeMove player posMoveTo board
    |_-> FlyCows (board:GameBoard) posCur posMoveTo player


 //RUN THE WHOLE GAME     
let rec run player game listVal  = 
   // need to find the blank cells that can be used...
   clearboard()
   printBoard game                      
   let playAgain () =
       printfn "Play again? [y/N] "
       match System.Console.ReadLine() with
       | "Y" | "y" -> run CB board [[]]
       | _ -> ()   
   printfn "%A's turn.  Type the Co-ordinates [<LETTER><NUMBER>] of the cell that you want to play into." (player)
   let getLenB = returnListBlack game
   let getLenW = returnListWhite game
   let b = System.Console.ReadLine() // Co-ordinate for cow from user
   let n = b.ToLower()
       //updateplayer player n
   match n with
   | "a1"  | "a4" | "a7"  | "b2" |"b4"  | "b6"  | "c3" | "c4" | "c5"  | "d1" | "d2" | "d3" | "d5" | "d6" | "d7" | "e3" | "e4" | "e5" | "f2" | "f4" | "f6" | "g1" | "g4" | "g7"  ->
      match (findState game.Board n) with
       | Blank ->
            // printfn "Invalid Input, Please re-enter position"
             let value = makeMove player n game    //Shoot n game player
             let (WPieces, BPieces) = value.bullets
             let output =
              match player with
                | CW -> ({value with bullets = (WPieces-1,BPieces)}, n)
                | CB -> ({value with bullets = (WPieces,BPieces-1)}, n)
            
             let (board,pos) = output
             let (tmpBoard2) = AccountForMill1 board 0 listVal
             let (tmp, tmp2) = tmpBoard2
             let (tmpBoard3,tmpList) = AccountForMill2 (tmp) 0 tmp2
             printBoard(tmpBoard3)
             run (swapPlayer player) tmpBoard3 tmpList
       | Basic player -> 
              match game.bullets with 
              | (0,0) -> 
                       let b = Console.ReadLine() 
                       let flag = inputCheck b
                       match (flag && own board player b) with 
                       | true -> let board = moveCows game n b player
                                 let newBoard = removeCow board n
                                 printBoard(newBoard)
                                 run (swapPlayer player) newBoard listVal
                       | false -> printfn "Invalid Input, please enter a valid input position" 
                                  run (swapPlayer player) board listVal
              | _ -> run (swapPlayer player) game listVal
          
        | SuperSayin player -> 
              match game.bullets with 
              | (0,0) ->
                    match (getLenB.Length,getLenW.Length) with
                    | (3,_) 
                    | (_,3) ->
                            let b = Console.ReadLine() 
                            let flag = inputCheck b
                            match flag with 
                            | true -> let board = moveCows game n b player
                                      let newBoard = removeCow board n
                                      printBoard(newBoard)
                                      run (swapPlayer player) board listVal
                            | false -> run (swapPlayer player) board listVal
                    | (2,_) -> printf "Black lost, you win!"
                               playAgain()
                    | (_,2) -> printf "White lost, you win!"
                               playAgain()
                    | _ -> run (swapPlayer player) board listVal
              | _ -> run (swapPlayer player) board listVal
   
             
   | _ -> run player game listVal

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
let getinput() = (System.Console.ReadKey true).KeyChar
 


//let placingcows input =

let msgPlacing = "Please enter the letter you want to place your cow at"
let msgMoving = "Please enter the letter you want to move your cow to"
let msgFlying = "Please enter the letter you want to fly your cow to"
let msgError = "Invalid output, please choose a different cell"   

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let r = getinput ()
    
    match r with
       |'p'|'P'|_ -> 
            clearboard()
            printBoard (board) 
            run CB board [[]]  //start with 24 cows, 12 for each. when this value reaches 0, go from placing to moving
       | _ -> rules ()
            
    //printBoard blankBoard
    
    Console.Read()      
