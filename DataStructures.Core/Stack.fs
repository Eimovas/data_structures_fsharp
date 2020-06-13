module Stack

type Stack<'a>() =
    let mutable items = List.empty
    
    member __.Add item = items <- item::items
    member __.Remove() =
        match items with
        | [] -> None
        | head::tail ->
            items <- tail
            Some head
    member __.Peek() =
        match items with
        | [] -> None
        | head::_ -> Some head 
    member __.Size() = items.Length

type private StackMsg<'a> = 
    | Add of 'a
    | Remove of AsyncReplyChannel<'a option> 
    | Peek of AsyncReplyChannel<'a option> 
    | Size of AsyncReplyChannel<int>

type SafeStack<'a>() =        
    let agent = MailboxProcessor<StackMsg<'a>>.Start(fun inbox ->
        let rec loop items = async {
            let! msg = inbox.Receive()
            match msg with
            | Add item -> return! loop (item::items)
            | Remove reply ->
                match items with
                | [] ->
                    reply.Reply None
                    return! loop items
                | head::tail -> 
                    reply.Reply(Some head)
                    return! loop tail                
            | Peek reply ->
                if items.IsEmpty then reply.Reply(None) else reply.Reply(Some items.Head)
                return! loop items
            | Size reply -> reply.Reply(items.Length); return! loop items
        }
        loop List.empty
    )
    
    member __.Add item = agent.Post (Add item)
    member __.Remove() = agent.PostAndReply (fun reply -> Remove reply)
    member __.Peek() = agent.PostAndReply (fun reply -> Peek reply)
    member __.Size() = agent.PostAndReply (fun reply -> Size reply)