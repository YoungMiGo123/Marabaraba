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
           let sym = "O"
           printfn "     %d%s%d%s%d%s%d%s%d%s%d%s%d " 1 bk2 2 bk2 3 bk 4 bk 5 bk2 6 bk2 7  // prints out the number scale at the top of the board
           printfn "\n"
           // rest of the methods called prints out the board, one line at a time.
           printfn "A    %s%s%s%s%s%s%s%s%s%s%s%s%s " (cell c1 sym) liz ("") liz ("") liz (cell c3 sym) liz ("") liz ("") liz (cell c7 sym) 
           printSep1()
           printfn "B    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") liz (cell c2 sym) liz ("") liz (cell c3 sym) liz ("") liz (cell c3 sym) liz ("") 
           printSep1()
           printfn "C    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") bk ("") bk (cell c3 sym) liz (cell c3 sym) liz (cell c3 sym) bk ("") bk ("")
           printSep2()
           printfn "D    %s%s%s%s%s%s%s%s%s%s%s%s%s " (cell c1 sym) liz2 (cell c2 sym) liz2 (cell c3 sym) bk bk (" ") (cell c3 sym) liz2 (cell c3 sym) liz2 (cell c3 sym)
           printSep2()
           printfn "E    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") bk ("") bk (cell c3 sym) liz (cell c3 sym) liz (cell c3 sym) bk ("") bk ("")
           printSep1()
           printfn "F    %s%s%s%s%s%s%s%s%s%s%s%s%s " ("") liz (cell c2 sym) liz ("") liz (cell c3 sym) liz ("") liz (cell c3 sym) liz ("")
           printSep1()
           printfn "G    %s%s%s%s%s%s%s%s%s%s%s%s%s " (cell c1 sym) liz ("") liz ("") liz (cell c3 sym) liz ("") liz ("") liz (cell c7 sym)
      let printSepConners () = printfn "|\%s|%s/|" bk bk    
      let printSep () = printfn "---+---+---+---+---+---+---+"
      printRow r1 ""
  
      

[<EntryPoint>]
let main argv = 
    //printfn "%A" argv
    printBoard blankBoard
    Console.Read()      

    0 // return an integer exit code