//// -------------- ////
//     Exercise 1     //
//// -------------- ////

// A)
// Currying steht für den Vorgang, eine mehrstellige Funktion, die n-Tupel als Eingabewerte akzeptiert in einer Funktion umzuwandeln, die eine (n-1)-Stellige Funktion zurückgibt.
// Uncurrying steht für den umgekehrten Vorgang. Hierbei wird eine Funktion, die eine (n-1)-Stellige Funktion zurückgibt in einer Funktion umgewandelt, die n-Tupel als Eingabewerte akzeptiert.
//
// Dieses Prinzip ist in der funktionalen Programmierung von grosser Wichtigkeit, weil mathematische Funktionen immer nur einen Parameter haben. In F# Sharp kümmert sich der Compiler darum,
// Funktionen welche mit n-Tupel als Eingabewerte deklariert wurden in einzelne Funktionen umzuwandeln, die jeweils nur einen einzigen Parameter entgegennehmen.



// B)

// Implementierte Funktion `curry`
// val curry : f:('a * 'b -> 'c) -> x:'a -> y:'b -> 'c
let curry f x y = f (x, y)

// Implementierte Funktion `uncurry`
// val uncurry : f:('a -> 'b -> 'c) -> x:'a * y:'b -> 'c
let uncurry f (x, y) = f x y

// Test gemäss Aufgabenstellung
// val it : int * int = (10, 15)
uncurry (+) (3, 7), curry (fun (x, y) -> x * y) 3 5



// C)

// Implementierte des Datentyps `'a deepList`
type 'a deepList = 
    | E of 'a
    | L of 'a deepList list

// Deklaration einer Liste von der Form [2;[[3;4];5];[[[]]];[[[6]]];7;8;[]]
// val ls : int deepList = L [E 2; L [L [E 3; E 4]; E 5]; L [L [L []]]; L [L [L [E 6]]]; E 7; E 8; L []]
let ls = L [E 2;L [L [E 3;E 4];E 5];L [ L [ L []]];L [L [L [E 6]]];E 7;E 8;L []]



// D)

// Implementierte Funktion `depth`
// val depth : _arg1:'a deepList -> int
let rec depth = function
    | E x -> 0
    | L [] -> 1
    | L x -> 1 + (List.map(fun x -> depth x) x |> List.max)

// Test gemäss Aufgabenstellung
// val it : int = 4
depth ls