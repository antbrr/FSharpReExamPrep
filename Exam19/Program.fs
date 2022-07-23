//1: Recursion on natural numbers

open Microsoft.FSharp.Core

type Peano =
| O
| S of Peano

let test1 = S (S (S O))

let test2 = 3u

//1.1

let rec toInt (a: Peano) =
    match a with
    | O -> 0u
    | S(x) -> toInt x + 1u

let rec fromInt (x: uint32) =
    match x with
    | 0u -> O
    | n -> S(fromInt(n-1u))
//1.2

let rec add (a: Peano) (b: Peano) =
    match b with
    | O -> a
    | S(b) -> add (S(a)) b

let rec mult (a: Peano) (b: Peano) =
    match b with
    | O -> O
    | S(x) -> add a (mult a x)

let rec pow (a: Peano) (b: Peano) =
    match b with
    | O -> S(O)
    | S(x) -> mult a (pow a x)


//1.3

let tailAdd (a: Peano) (b: Peano) =
    let rec aux acc p =
        match p with
        | O -> acc
        | S(x) -> aux (S(acc)) x
    aux a b

let tailMult (a: Peano) (b: Peano) =
    let rec aux acc p =
        match p with
        |O -> acc
        |S(x) -> aux (tailAdd a acc) x 
    aux O b
    
let tailPow (a: Peano) (b: Peano) =
    let rec aux acc p =
        match p with
        |O -> acc
        |S(x) -> aux (tailMult a acc) x
    aux (S O) b
    
//1.4
let rec loop f acc (p: Peano) =
    match p with
    | O -> acc
    | S(x) -> loop f (f acc) x
    
//1.5
let rec loopAdd (p1: Peano) (p2: Peano) =
    loop (fun acc -> S(acc)) p1 p2
    
let rec loopMult (p1: Peano) (p2: Peano) =
    loop (fun acc -> loopAdd p1 acc) O p2

let rec loopPow (p1: Peano) (p2: Peano) =
    loop (fun acc -> loopMult p1 acc) (S O) p2
    
    
//2: Code Comprehension

let rec f x =
    function
    | []                -> None
    | y::ys when x = y  -> Some ys
    | y::ys when x <> y -> 
        match f x ys with
        | Some ys' -> Some (y::ys')
        | None     -> None

let rec g xs =
    function
    | []    -> xs = []
    | y::ys -> 
        match f y xs with
        | Some xs' -> g xs' ys
        | None     -> false

//2.1

(*
 Q: What are the types of functions f and g ?
 
 A: f has type 'a -> 'a list -> 'a list option
    g has type 'a list -> 'a list -> bool
    
 Q: What do functions f and g do? Focus on what they do rather than how they do it.
 
 A: f takes an element and a list, and if that element exists in the list it returns the list without the first occurence of the element.
    if the element does not exist it returns None
    
    g takes two list and return true if they are the same list, false if they are not
    
 Q: What would be appropriate names for functions f and g ?
 
 A: f can be named removeSingle
 
    g can be named listEquality
    
*)


//2.2

// The function f generates a warning during compilation: warning FS0025: Incomplete pattern matches on this expression.

(*
    Q: Why does this happen, and where?
    
    A: It happens at the y::ys when x <> y . We need to remove the when condition to fix it.
    
    Q: Write a function f2 that does the same thing as f and that does not have this problem.
    
    A:
*)

let rec f2 x =
    function
    | []                -> None
    | y::ys when x = y  -> Some ys
    | y::ys -> 
        match f2 x ys with
        | Some ys' -> Some (y::ys')
        | None     -> None

//2.3
(*
    Both functions f and g contain matches on option types in their last clause. These can be removed
    and the program streamlined by using Option.map and/or Option.defaultValue.
    
    Create two functions fOpt and gOpt that behave the same as f and g respectively but with their
    match-statements replaced by shorter code that uses the library functions mentioned above. If you are
    unclear on what the library functions do, then look them up
    
    A:
*)

let rec fOpt x =
    function
    | []                -> None
    | y::ys when x = y  -> Some ys
    | y::ys when x <> y -> 
        f x ys
        |> Option.map(fun ys' -> y :: ys')
        
let rec gOpt xs =
    function
    | []    -> xs = []
    | y::ys -> 
         f y xs
         |> Option.map(fun xs' -> gOpt xs' ys)
         |> Option.defaultValue false
        
//2.4

(*
Q: Only one of the functions f and g is tail-recursive. Which one and why? To make a compelling
argument you should evaluate a function call of the tail-recursive function, similarly to what is done in
Chapter 1.4 of HR, and reason about that evaluation. You need to make clear what aspects of the
evaluation tell you that the funciton is tail recursive

A: It is f which is tail recursive. Lets consider the call

f 1 [2;3;1]

let rec f x =
    function
    | []                -> None
    | y::ys when x = y  -> Some ys
    | y::ys when x <> y -> 
        match f x ys with
        | Some ys' -> Some (y::ys')
        | None     -> None
        
f 1 [3;1]

    None(None(Some([2;3))
    

Q: Create a tail-recursive version of f or g , whichever was not already tail recursive, using continuations.
The function should be called fTail or gTail depending on which one you implement



*)

let fTail x xs =
    let rec aux acc xs' =
        match xs' with
        | []                -> Some acc
        | y::ys when x = y  -> Some ys
        | y::ys when x <> y -> 
        match f x ys with
        | Some ys' -> aux (Some (y::acc)) ys' 
        | None     -> None
    aux [] xs
    
        
//3 Sequences of PI


//4 Programmable Typewriters



         


