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
           Position: Coordinates
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
//type Result 
let swapPlayer x= 
      match x with 
      | Cow_Black -> Cow_White
      | Cow_White -> Cow_Black
      | Blank -> failwith "A FATAL ERROR OCCURED"

let blankBoard =
    let blankRow = Blank, Blank, Blank, Blank, Blank, Blank, Blank
    Board (blankRow, blankRow, blankRow, blankRow, blankRow, blankRow, blankRow)

let printBoard (Board (r1, r2, r3, r4, r5, r6, r7)) =
      System.Console.Clear()
      let cell offset value n = 
          match value with 
          | Cow_Black -> 'B'
          | Cow_White -> 'W'
          | Blank -> char (string (offset+n))
      let printRow (c1,c2,c3,c4,c5,c6,c7) offset = 
           let cell = cell offset
           printfn "%c---%c---%c---%c---%c---%c---%c" (cell c1 1) (cell c2 2) (cell c3 3) (cell c4 4) (cell c5 5) (cell c6 6) (cell c7 7)
      let printSep () = printfn "---+---+---+---+---+---+---+"
      printRow r1 0
      printSep ()
      printRow r2 3
      printSep ()
      printRow r3 6
      printSep ()
      printRow r4 9
      printSep ()
      printRow r5 12
      printSep ()
      printRow r6 15
      printSep ()
      printRow r7 18
      printSep ()

[<EntryPoint>]
let main argv = 
    //printfn "%A" argv
    
          

    0 // return an integer exit code