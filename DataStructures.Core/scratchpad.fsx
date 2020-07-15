#load "LinkedList.fs"

open LinkedList

let list = LinkedList.DoublyLinkedList<int>()

list.Add 1
list.Add 5
list.Add 3
list.Add 3
list.Add 3
list.Add 3
list.Add 3

list.List() |> Seq.iter (fun i -> printfn "%A" i)

list.Tail
