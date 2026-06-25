module CyclicQueue

type Value = int

let mutable first: int = 0
let mutable last: int = 0
let mutable queue: Value option array = Array.empty

let create (n: int) : unit =
    queue <- Array.empty
    for i in 0.. (n - 1) do
        queue <- (Array.ofList ((Array.toList queue) @ [None]))
    first <- 0
    last <- 0

let isEmpty () : bool =
    queue[first].IsNone && queue[last].IsNone

let enqueue (e: Value) : bool =
    if isEmpty () then
        if queue = Array.empty then
            false
        elif last = queue.Length - 1 then
            queue[last] <- Some e
            true
        else
            queue[last] <- Some e
            true
    elif last = first - 1 then
        false
    elif last = queue.Length - 1 then
        if first = 0 then
            false
        else
            last <- 0
            queue[last] <- Some e
            true
    else
        last <- last + 1
        queue[last] <- Some e
        true

let dequeue () : Value option =
    if isEmpty () then
        None
    else 
        if first = last then
            let a = queue[first]
            queue[first] <- None
            a
        elif first = queue.Length - 1 then            
            let a = queue[first]
            queue[first] <- None
            first <- 0 
            a 
        else
            let a = queue[first]
            queue[first] <- None
            first <- first + 1 
            a

let length () : int =
    let mutable l = 0
    if (Array.isEmpty queue) then
        0
    else
        for i in queue do
            if i = None then
                ()
            else 
                l <- l + 1
        l

let toString () : string =
    let string = [for j in queue do yield string(j)]
    let mutable bing = ""
    for i in 0.. (string.Length - 1) do
        if string[i] = "" then
            bing <- bing + "None" + " "
        else  
            bing <- bing + string[i] + " "
    bing

let size () : int =
    Array.length queue
