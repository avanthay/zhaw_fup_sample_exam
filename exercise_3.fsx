//// -------------- ////
//     Exercise 3     //
//// -------------- ////


// A)
// Ein Funktion ist dann endrekursiv, wenn der rekursive Aufruf die letzte Operation ist. Das Resultat wird direkt vom rekursiven Aufruf zurückgegeben.
// Wird eine Addition auf das Ergebnis des rekursiven Aufrufs ausgeführt, so ist die Funktion nicht endrekursiv.

// In der Funktionale Programmierung ist der Einsatz von endrekursiven Funktionen wichtig. Da endrekursive Funktionen immer in einer iterativen Variante umgeschrieben werden können, nimmt
// der Compiler der meisten funktionalen Programmiersprachen diese Umwandlung automatisch vor, um den Bedarf an Speicherplatz im Stack auf eine Konstante zu reduzieren.



// B)

// Die Funktion `zip` aus der Aufgabe 1
let rec zip lst1 lst2 =
    match lst1, lst2 with
    | x::xs, y::ys -> (x, y)::(zip xs ys)
    | _ -> []

// Implementieren der Funktion `zip` als endrekursive Variante mit dem Akkumulator-Pattern
let zipTrAP lst1 lst2 =
    let rec zipTrA' acc lst1 lst2 =
        match lst1, lst2 with
        | x::xs, y::ys -> zipTrA' (acc @ [(x, y)]) xs ys
        | _ -> acc
    zipTrA' [] lst1 lst2

// Test der endrekursiven Funktion `zipTrAP`
// val zipAndZipTrAPAreEquals : bool = true
let zipAndZipTrAPAreEquals =
    let lst1, lst2 = [1..10], [11..20]
    zip lst1 lst2 = zipTrAP lst1 lst2

// Implementieren der Funktion `zip` als endrekursive Variante mit dem Continuation-Pattern
let zipTrCP lst1 lst2 =
    let rec zipTrCP' f lst1 lst2 =
        match lst1, lst2 with
        | x::xs, y::ys -> zipTrCP' (fun () -> f() @ [(x, y)]) xs ys
        | _ -> f()
    zipTrCP' (fun () -> []) lst1 lst2

// Test der endrekursiven Funktion `zipTrCP`
// val zipAndZipTrCPAreEquals : bool = true
let zipAndZipTrCPAreEquals =
    let lst1, lst2 = [1..10], [11..20]
    zip lst1 lst2 = zipTrCP lst1 lst2



// C)

// Gegebener Fixpunktiterator
let rec fix f g = f (fix f) g

// Implementieren einer "normalen", rekursiven Funktion für die Überprüfung auf palindromität (nicht Teil der Übung)
let rec testPalindrom = function
    | "" -> true
    | x when Seq.head x  = Seq.last x -> testPalindrom x.[1..Seq.length x - 2]
    | _ -> false

// Test der Funktion `testPalindrom` (immer noch nicht Teil der Übung)
// val it : bool * bool * bool * bool * bool = (false, true, true, true, true)
testPalindrom "yxaxay", testPalindrom "a", testPalindrom "aba", testPalindrom "abba", testPalindrom ""

// Implementieren der Funktion `f` (Teil der Übung)
let f h = function
    | "" -> true
    | x when Seq.head x  = Seq.last x -> h x.[1..Seq.length x - 2]
    | _ -> false

// Test gemäss Aufgabenstellung (Auch das ist Teil der Übung)
// val it : bool * bool * bool * bool * bool = (false, true, true, true, true)
fix f "yxaxay", fix f "a", fix f "aba", fix f "abba",fix f ""