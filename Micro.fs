module Micro

type Quote =
    {
        bid: float
        ask: float
    }
    member this.mid() = ( this.bid + this.ask ) / 2.0

let isprime n =
    let rec check i =
        i > n/2 || (n % i <> 0 && check (i + 1))
    check 2

/// Left fold - recursive call before the operation call i.e. on the left
let rec sum_left acc x =
    match x with
        | [] -> acc
        | h :: t -> sum_left (h + acc) t

/// Right fold - operation call first before recursive call i.e. on the right
let rec sum_right acc x =
    match x with
        | [] -> acc
        | h :: t -> h + sum_right acc t

let rec fact x = ( if x = 0 then 1 else x * fact ( x - 1 ) )

let rec fact_tail acc x =
    match x with
        | 0 -> acc;
        | x -> fact_tail ( acc * x ) ( x - 1 )

let ( |Negative|Positive| ) number =
    if number >= 0.0 then Positive else Negative

let testSign n =
    match n with
        | Negative -> -1
        | Positive -> 1

type MyException( message, category ) =
    inherit exn( message )
    member x.Category = category
    override x.ToString() = sprintf "[%s] %s" category message

let testException() =
    try
        raise ( MyException( "what a jerk", "omg" ) )
    with
    | :? MyException as ex -> printfn "My Exception: %s" ( ex.ToString() )
    | _ as ex -> printfn "%s" ( ex.ToString() )

exception MyGreatException of string * string

let testGreatException() =
    try
        raise ( MyGreatException( "great", "exception" ) )
    with
    | MyGreatException(msg, category) -> printfn "My Great Exception %s" msg
    | _ as ex -> printfn "%s" <| ex.ToString()

let slope p1 p2 =
    let ( x1, y1 ) = p1
    let ( x2, y2 ) = p2
    ( y1 - y2 )/( x1 - x2 )
