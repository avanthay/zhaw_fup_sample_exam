//// -------------- ////
//     Exercise 1     //
//// -------------- ////


// A)

//// i)
//// Die Funktion `revMap` wendet die Funktion `f` auf jedes einzelne Element einer gegebenen Liste und invertiert die Liste.
//// Das letzte Element der Liste ist also nach Anwendung von `revMap` das Erste und umgekehrt. 

//// ii)
//// Diese Implementierung ist nicht optimal performant.
//// Die Funktion kann deutlich verbessert werden, indem die von F# zur Verfügung stehenden List-Operatoren verwendet werden.
//// Die Implementierung von revMap könnte zum Beispiel so aussehen
let rec revMap2 f x =
    List.rev x |> List.map f



// B)

// Gegebene Funktion `zip`
let rec zip lst1 lst2 =
    match lst1,lst2 with
    | x::xs,y::ys -> (x,y)::zip xs ys
    | _ -> []

// Implementierte Funktion `ordered`
// val ordered : lst:'a list -> bool when 'a : comparison
let ordered lst = 
    List.sort lst |> zip lst |> List.fold (fun isOrdered x -> fst x <= snd x && isOrdered) true

// Test gemäss Aufgabenstellung
// val it : bool * bool * bool = (true, false, true)
ordered [1;10;13;21], ordered [2;10;1], ordered []



// C)

// Implementiertes Active Pattern für `First`
// val ( |First|_| ) : x:string -> char option
let (|First|_|) x =
    match x with
    | x when String.length x > 0 -> Some (Seq.head x)
    | _ -> None

// Implementierte Funktion `doubleFirst`
// val doubleFirst : w:string -> string
let doubleFirst w = 
    match w with
    | First x -> string x + w
    | _ -> ""


// Test gemäss Aufgabenstellung
// val it : string * string = ("ssda", "")
doubleFirst "sda", doubleFirst ""