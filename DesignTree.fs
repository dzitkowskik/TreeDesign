module DesignTree

type 'a Tree = Node of 'a * ('a Tree list)

type Extent = (float * float) list

let movetree tree y =
    match tree with
    | Node((label, x:float), subtrees) -> Node((label, x+y), subtrees)
 
let moveextent e x = List.map (fun (p,q) -> (p+x,q+x)) e

let rec merge = function
    | ([], qs) -> qs
    | (ps, []) -> ps
    | ((p,_)::ps, (_,q)::qs) -> (p,q)::merge(ps,qs)

let mergelist es = List.foldBack (fun e acc -> merge(e, acc)) es []

let rmax (p:float, q:float) = if p>q then p else q

let rec fit = function
    | ((_,p)::ps, (q,_)::qs) -> 
        rmax(fit(ps, qs), (p-q)+1.0)
    | _ -> 0.0

let fitlistl es =
    let rec loop acc = function
        | [] -> []
        | (e::es) ->
            let x = fit(acc, e)
            x::(loop (merge(acc, moveextent e x)) es)
    loop [] es

let fitlistr es =
    let rec loop acc = function
        | [] -> []
        | (e::es) ->
            let x = -fit(e, acc)
            x::(loop (merge(moveextent e x, acc)) es)
    List.rev(loop [] (List.rev(es)))

let mean (x,y) = (x+y)/2.0

let fitlist es = 
    let left = (fitlistl es)
    let right = (fitlistr es)
    (List.zip left right) |> List.map mean

let rec designr tree = 
    match tree with
    | Node(label, subtrees) ->
        let (trees, extents) = (List.map designr subtrees) |> List.unzip
        let positions = fitlist extents
        let ptrees = (List.zip trees positions) |> List.map (fun (x,y) -> movetree x y)
        let pextents = (List.zip extents positions) |> List.map (fun (x,y) -> moveextent x y)
        let resultextent = (0.0, 0.0)::(mergelist pextents)
        let resulttree = Node((label, 0.0), ptrees)
        (resulttree, resultextent)

let design tree = fst (designr tree)