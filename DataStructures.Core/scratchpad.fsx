#load "ArrayList.fs"

open ArrayList

let list = ArrayList<int>()

list.Add 1
list.Add 5
list.Add 3
list.Add 3
list.Add 3
list.Add 3
list.Add 3
list.RemoveAt 1

list |> Seq.iter (fun i -> printfn "%i" i)
list.Size
list.Capacity

