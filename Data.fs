namespace QuantFin

module Data =

  type Bar = {
    d: System.DateTime
    h: float
    l: float
    o: float
    c: float
    v: int64
    adj: float option
  }

  let last lst =
    let rec last' lst acc =
      match lst with
      | [] -> acc
      | h::t -> last' t (Some h)
    last' lst None

  type Queue<'t> =
    | FiniteQueue of int * int * ('t list) * ('t list)

  let push q item =
    match q with
    | FiniteQueue (0, _, _, _ ) -> failwith "Queue size is zero"
    | FiniteQueue (n, 0, [], []) -> FiniteQueue (n, 1, [item], [])
    | FiniteQueue (n, k, f, [] ) ->
      if ( k < n ) then
        FiniteQueue (n, k+1, f, [item])
      else
        FiniteQueue (n, k, [item], [])
    | FiniteQueue (n, k, fh::ft, r) ->
      if ( k < n ) then
        FiniteQueue (n, k+1, fh::ft, item::r)
      else
        if ft = [] then
          FiniteQueue (n, k, List.rev r, [item])
        else
          FiniteQueue (n, k, ft, item::r)

  let pop q =
    match q with
    | FiniteQueue (0, _, _, _ ) -> failwith "Queue size is zero"
    | FiniteQueue (_, _, [], _) -> failwith "Invariant violated: empty front"
    | FiniteQueue (n, k, [item], r) ->
        (item, FiniteQueue (n, k-1, List.rev r, []))
    | FiniteQueue (n, k, fh::ft, r) -> (fh, FiniteQueue (n, k-1, ft, r))

  let pokeHead q =
    match q with
    | FiniteQueue (0, _, _, _) -> failwith "Queue size is zero"
    | FiniteQueue (_, 0, [], _) -> None
    | FiniteQueue (_, _, [], _) -> failwith "Invariant violated: empty front"
    | FiniteQueue (_, _, fh::_, _) -> Some fh

  let pokeBack q =
    match q with
    | FiniteQueue (0, _, _, _) -> failwith "Queue size is zero"
    | FiniteQueue (_, 0, [], []) -> None
    | FiniteQueue (_, _, [], []) -> failwith "Invariant violated: empty front"
    | FiniteQueue (_, _, f, []) -> last f
    | FiniteQueue (_, _, _, rh::_) -> Some rh

  let makeQ n item = FiniteQueue (n, 1, [item], [])
