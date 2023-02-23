module MultiSet

type MultiSet<'a> when 'a : comparison =
    | M of Map<'a, uint32>

let empty = M Map.empty
let isEmpty (M(m)) = Map.isEmpty m
let size (M(m)) = (0u, m) ||> Map.fold (fun acc _ n -> acc + n)
let contains a (M(m)) = Map.containsKey a m
let numItems a (M(m)) = Option.defaultValue 0u (Map.tryFind a m)
let add a n (M(m)) = M (Map.add a (numItems a (M m) + n) m)
let addSingle a (M(m)) = M (Map.add a (numItems a (M m) + 1u) m)
let remove a n (M(m)) = if numItems a (M m) <= n then M (Map.remove a m)
                        else M (Map.add a (numItems a (M m) - n) m)
let removeSingle a (M(m)) = if numItems a (M m) <= 1u then M (Map.remove a m)
                            else M (Map.add a (numItems a (M m) - 1u) m)
let fold f acc (M(m)) = Map.fold f acc m
let foldBack f (M(m)) acc = Map.foldBack f m acc
