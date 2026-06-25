#load "CyclicQueue.fs"
open CyclicQueue

printf"
        let's begin the tests of the module CyclicQueue
    \n"

let testCreate () =
    create 5
    printfn"Testing creating queue:"
    printfn"Expected queue capacity: 5. Actual output: %d. Queue: %s\n" (size ()) (toString ())

let testEnqueue () =
    printfn"Testing enqueue by adding elements to queue:"
    let enqueueResults = [(7, true); (3, true); (-1, true); (5, true); (8, true); (9, false)]
    for elements, expectedOutput in enqueueResults do
        let actualOutput = enqueue elements

        printfn "Enqueuing %d (expected output: %A). Actual output: %A. Queue: %s" elements expectedOutput actualOutput (toString ())
    printfn""

let testDequeue () =
    printfn"Testing dequeue by removing elements from queue:"
    let dequeueResults = [7;3;-1;5;8;]
    for elements in dequeueResults do
        let actualOutput = dequeue ()
        printfn"Dequeuing %d. Actual dequeued element: %A. Queue: %s" elements actualOutput (toString ())
    printfn""

let testIsEmptyLength () =
    printfn"Testing queue length:"
    printfn"Checking isEmpty, expected output: true. Actual output: %b. Queue: %s" (isEmpty ()) (toString ())
    printfn"Checking length, expected queue length: 0. Actual output: %d. Queue: %s" (length ()) (toString ())
    printfn"Expected total queue capacity: 5. Actual output: %d. Queue: %s\n" (size ()) (toString ())

let testReset () =
    printfn"Testing queue reset:"
    create 5

    let enqueueElements = [7;3;-1;5;8;9]
    for element in enqueueElements.[0..4] do
        let result = enqueue element
        if not result then printfn "Failed to enqueue %d" element
   
    printfn"Queue after enqueuing first five elements: %s" (toString ())
    printfn"Resetting queue..." 
    
    create 5    //resetting
    
    if isEmpty () then
        printfn "Queue is empty as expected. Actual queue: %s\n" (toString ())
    else
        printfn "Queue is not empty. Actual queue: %s\n" (toString ())

let testDequeueEmptyQueue () =
    printfn "Testing dequeue from an empty queue:"
    create 5
    printfn "Expected output: None. Actual output: %A. Queue: %s\n" (dequeue ()) (toString ())

let testSize () =
    printfn "Testing creating queue with different sizes:"
    for queueSize in [-1..2] do
        create queueSize
        if queueSize <= 0 then
            printfn "Result (size %d): Queue not created due to invalid size value." queueSize
        else
            printfn "Result (size %d): Queue successfully created with size: %d." queueSize (size ())
    printfn ""
    
let testCyclicBehavior () =
    printfn "Testing cyclic behavior of the queue:"

    create 5    // resetting/creating queue
    let enqueueResults = [7;3;-1;5;8]
    for elements in enqueueResults do
        let result = enqueue elements
        printfn "Enqueuing %d. Result: %A. Queue: %s" elements result (toString ())

    let dequeueResults = [7;3]
    for elements in dequeueResults do
        let result = dequeue ()
        printfn "Dequeuing %A. Queue: %s" result (toString ())

    let wrapAround = [(6, true); (11, true)]
    for (elements, expectedOutput) in wrapAround do
        let actual = enqueue elements
        printfn "Enqueuing %d (expected: %A). Actual: %A. Queue: %s" elements expectedOutput actual (toString ())
    printfn ""

testCreate ()
testEnqueue ()
testDequeue ()
testIsEmptyLength ()
testReset ()
testDequeueEmptyQueue ()
testSize ()
// testCyclicBehavior ()

printfn "all done!"
