//1: Recursion on natural numbers

open System
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
    | S(x) -> (toInt x) + 1u

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
    let rec aux acc b' =
        match b' with
        | O -> acc
        | S(x) -> aux (S(acc)) x
    aux a b

let tailMult (a: Peano) (b: Peano) =
    let rec aux acc b' =
        match b' with
        | O -> acc
        | S(x) -> aux (tailAdd acc a) x
    aux O b
    
let tailPow (a: Peano) (b: Peano) =
    let rec aux acc b' =
        match b' with
        | O -> acc
        | S(x) -> aux (tailMult acc a) x
    aux (S(O)) b
    
//1.4
let rec loop f acc (p: Peano) =
    match p with
    | O -> acc
    | S(x) -> loop f (f acc) x
    
//1.5
let loopAdd (a: Peano) (b: Peano) =
    loop(fun p -> S(p)) a b
    
let loopMult (a: Peano) (b: Peano) =
    loop(tailAdd a) O b
    
let loopPow (a: Peano) (b: Peano) =
    loop(tailMult a) (S(O)) b
    
    
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

What are the types of functions f and g? 
    f: 'a -> 'a list -> 'a list option 
    g: 'a list -> 'a list -> bool
What do functions f and g do? Focus on what they do rather than how they do it.
    f x xs returns Some xs' where xs' is xs with the first occurrence of x removed.
          If x is not in xs, it returns None
    g xs ys returns true if ys is a permutation of xs, else false.
    
What would be appropriate names for functions f and g ?

    g: containsSameElements or isPermutation
    
//2.2

The function f generates a warning during compilation: warning FS0025: Incomplete pattern matches on this expression..

Why does this happen, and where?

This happens at the match statement. It happens because the compiler is not smart enough to know that either y::ys when x = y and y::ys when x <> y must be true.
So it tells us that we might not match all cases

Write a function f2 that does the same thing as f and that does not have this problem.

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

Both functions f and g contain matches on option types in their last clause. These can be removed and the program streamlined by using Option.map and/or Option.defaultValue.

Create two functions fOpt and gOpt that behave the same as f and g respectively but with their match-statements replaced by shorter code that uses the library functions mentioned above. If you are unclear on what the library functions do, then look them up.
*)

let rec fOpt x =
    function
    | []                -> None
    | y::ys when x = y  -> Some ys
    | y::ys -> fOpt x ys
               |> Option.map (fun ys' -> y :: ys')
let rec gOpt xs =
    function
    | []    -> xs = []
    | y::ys -> fOpt y xs
               |> Option.map (fun xs' -> gOpt xs' ys)
               |> Option.defaultValue false

//2.4

(*
 It is g which is tail recursive. 
 This is because g calls itself as the last thing 
 on every Some outcome of the recursive call.
 
 I will now try to further convince you by evaluating a call of the function g
 
 g [1;2;3] [3;2;1] ->
 
 g [1;2] [2;1] ->
 
 g [1] [1] -> 
 
 g [] [] -> 
 
 true
 
*)

let fTail x xs =
    let rec aux cont xs' =
        match xs' with
        | [] -> cont None
        | y :: ys when x = y -> cont (Some ys)
        | y :: ys -> aux(fun result ->
            match result with
            | None -> None
            | Some xs -> cont(Some(y :: xs))) ys
    aux id xs
    
    
        
//3 Sequences of PI

//3.1
let rec calculatePi (x: uint64) =
    let element (x: uint64) = decimal 4/decimal(2UL * x * (2UL * x + 1UL) * (2UL * x + 2UL))
    match x with
    | 0UL -> 3.0M
    | n when n % 2UL = 0UL -> calculatePi (n-1UL) - (element n)
    | n -> calculatePi (n-1UL) + (element n)

//3.2
let piSeq = Seq.unfold(fun state -> Some(calculatePi state, state+1UL) ) 0UL

//3.3

let circleArea (r: float) = Seq.map(fun elem -> elem * (decimal r * decimal r) ) piSeq

let sphereVolume (r: float) = Seq.map(fun elem -> (4M/3M) * elem * decimal(r**3.0)) piSeq

//3.4

let circleSphere1 (r:float) = seq {
    for pi in piSeq do yield (pi * (decimal r * decimal r) ,(4M/3M) * pi * decimal(r**3.0))
}

let circleSphere2 (r: float) = Seq.zip (circleArea r) (sphereVolume r)
  

//4 Programmable Typewriters



         


