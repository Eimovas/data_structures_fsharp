module ArrayList

open System.Collections.Generic

type ArrayList<'a>() =
    let mutable array = Array.empty
    let mutable latestIndex = -1
    
    let isFull latestIndex = array.Length = (latestIndex + 1)
    let doubleSize() : 'a array =
        let currentLength = array.Length
        Array.init
            (if currentLength = 0 then 1 else array.Length * 2)
            (fun i ->
                if i > (currentLength - 1) then Unchecked.defaultof<'a>   
                else array.[i])  
    
    let add item =
        if isFull latestIndex then
            array <- doubleSize()
        
        latestIndex <- latestIndex + 1
        array.[latestIndex] <- item
    
    let addRange (items : 'a seq) = items |> Seq.iter add
    let removeAt index =
        let (first,second) = array |> Array.splitAt index

        array <- Array.concat [|first; second.[1..]|]
        latestIndex <- latestIndex - 1
    
    member __.Add item = add item
    member __.AddRange items = addRange items
    member __.RemoveAt index = removeAt index
    member __.Capacity = array.Length
    member __.Size = latestIndex + 1
    
    interface IEnumerable<'a> with
        member __.GetEnumerator() = (array.[..latestIndex] ).GetEnumerator()
        member __.GetEnumerator() = (array.[..latestIndex] :> IEnumerable<'a>).GetEnumerator() 
