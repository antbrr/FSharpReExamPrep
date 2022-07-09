type Peano =
| O
| S of Peano

let test1 = S (S (S O))

let three = S(S(S(O)))

let two = S(S(O))

let test2 = S (S (S (S (S (S O)))))

//1.1

let rec toInt (a: Peano) =
    match a with
    | O -> 0u
    | S(x) -> 1u + toInt x

let rec fromInt (x: uint32) =
    match x with
    | 0u -> O
    | x -> S(fromInt(x-1u))

//1.2

let rec add (a: Peano) (b: Peano) =
    match b with
    | O -> a
    | S(x) -> add (S(a)) x

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
        | O -> acc
        | S(x) -> aux (tailAdd a acc) x
    aux O b

let tailPow (a: Peano) (b: Peano) =
    let rec aux acc p =
        match p with
        | O -> acc
        | S(x) -> aux (tailMult a acc) x
    aux (S(O)) b

//1.4
 

let rec loop f acc p =
    match p with
    | O -> acc
    | S(x) -> loop f (f acc) x

//1.5
let loopAdd (a: Peano) (b: Peano) =
    loop (fun acc -> S(acc)) a b

let rec loopMult (a: Peano) (b: Peano) =
    loop (fun acc -> loopAdd a acc) O b

let rec loopPow (a: Peano) (b: Peano) =
    loop (fun acc -> loopMult a acc) (S O) b    


