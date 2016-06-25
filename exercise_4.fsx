//// -------------- ////
//     Exercise 4     //
//// -------------- ////


// Modellierung von endliche Zustandautomaten (FSA)
open System
open System.Collections.Generic

type State = {Name : string; IsEndState : bool}
type StateTransition = {CurrentState: State; CurrentChar: char}

let logger f trans state word =
    if Seq.length word > 0 then
        printfn "Processing [%c]%s ...new state = %s" (Seq.head word) (Seq.tail word |> String.Concat) state.Name
    f trans state word

let changeState (trans:Dictionary<StateTransition, State>) state word =
    trans.[{CurrentState = state; CurrentChar = Seq.head word}]

let rec machine trans state = function 
    | word when Seq.length word > 0 -> machine trans (logger changeState trans state word) (Seq.tail word)
    | _ -> printfn "%s with final state %s \n" (if state.IsEndState then "Accepted" else "Not accepted") state.Name



// Operieren eines FSAs auf ein endliches Wort. Der gewählte FSA akzeptiert Wörter aus der Sprache `x01y` wobei x,y Zeichen aus der Menge {0,1} sind.

// Definition von 3 Zustände, davon 1 akzeptierender Zustand
let q0 = {Name = "q0"; IsEndState = false}
let q1 = {Name = "q1"; IsEndState = false}
let q2 = {Name = "q2"; IsEndState = true}

// Definition der Zustandsübergangsfunkion
let trans = new Dictionary<StateTransition, State>()
trans.Add({CurrentState = q0; CurrentChar = char "1"}, q0)
trans.Add({CurrentState = q0; CurrentChar = char "0"}, q1)
trans.Add({CurrentState = q1; CurrentChar = char "0"}, q1)
trans.Add({CurrentState = q1; CurrentChar = char "1"}, q2)
trans.Add({CurrentState = q2; CurrentChar = char "0"}, q2)
trans.Add({CurrentState = q2; CurrentChar = char "1"}, q2)



// Defintion von Testworter
let wordToAccept = ["01"; "0101"; "11101"; "011111"; "01010101011"]
let wordToDecline = [""; "1"; "10"; "0000"; "1111"; "111111110"]

// Testen der zu akzeptierende Wörter
List.iter (fun word -> machine trans q0 (Seq.toList word)) wordToAccept

// Testen der abzulehnende Wörter
List.iter (fun word -> machine trans q0 (Seq.toList word)) wordToDecline




// Das Ergebnis


// Die zu akzeptierende Wörter:
//
// Processing [0]1 ...new state = q0
// Processing [1] ...new state = q1
// Accepted with final state q2 
//
// Processing [0]101 ...new state = q0
// Processing [1]01 ...new state = q1
// Processing [0]1 ...new state = q2
// Processing [1] ...new state = q2
// Accepted with final state q2 
//
// Processing [1]1101 ...new state = q0
// Processing [1]101 ...new state = q0
// Processing [1]01 ...new state = q0
// Processing [0]1 ...new state = q0
// Processing [1] ...new state = q1
// Accepted with final state q2 
//
// Processing [0]11111 ...new state = q0
// Processing [1]1111 ...new state = q1
// Processing [1]111 ...new state = q2
// Processing [1]11 ...new state = q2
// Processing [1]1 ...new state = q2
// Processing [1] ...new state = q2
// Accepted with final state q2 
//
// Processing [0]1010101011 ...new state = q0
// Processing [1]010101011 ...new state = q1
// Processing [0]10101011 ...new state = q2
// Processing [1]0101011 ...new state = q2
// Processing [0]101011 ...new state = q2
// Processing [1]01011 ...new state = q2
// Processing [0]1011 ...new state = q2
// Processing [1]011 ...new state = q2
// Processing [0]11 ...new state = q2
// Processing [1]1 ...new state = q2
// Processing [1] ...new state = q2
// Accepted with final state q2 
//
// val it : unit = ()


// Die abzulehnende Wörter
// Not accepted with final state q0 
//
// Processing [1] ...new state = q0
// Not accepted with final state q0 
//
// Processing [1]0 ...new state = q0
// Processing [0] ...new state = q0
// Not accepted with final state q1 
//
// Processing [0]000 ...new state = q0
// Processing [0]00 ...new state = q1
// Processing [0]0 ...new state = q1
// Processing [0] ...new state = q1
// Not accepted with final state q1 
//
// Processing [1]111 ...new state = q0
// Processing [1]11 ...new state = q0
// Processing [1]1 ...new state = q0
// Processing [1] ...new state = q0
// Not accepted with final state q0 
//
// Processing [1]11111110 ...new state = q0
// Processing [1]1111110 ...new state = q0
// Processing [1]111110 ...new state = q0
// Processing [1]11110 ...new state = q0
// Processing [1]1110 ...new state = q0
// Processing [1]110 ...new state = q0
// Processing [1]10 ...new state = q0
// Processing [1]0 ...new state = q0
// Processing [0] ...new state = q0
// Not accepted with final state q1 
//
// val it : unit = ()