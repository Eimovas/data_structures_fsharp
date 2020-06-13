#load "Queue.fs"

open Queue

let stack = SafeQueue<int>()
stack.Enqueue 10
stack.Enqueue 15
stack.Enqueue 20

stack.Peek() |> printfn "%A"
stack.Size() |> printfn "%i"

stack.Dequeue() |> printfn "%A"
stack.Dequeue() |> printfn "%A"
stack.Dequeue() |> printfn "%A"
stack.Dequeue() |> printfn "%A"

stack.Size() |> printfn "%i"