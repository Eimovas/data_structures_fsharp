module Queue

type Queue<'a>() =    
    let items = ResizeArray<'a>()
    let dequeue() =
        if items.Count = 0 then None
        else
            let item = items.[items.Count - 1]
            items.RemoveAt(items.Count - 1)
            Some(item)
    
    member __.Enqueue item = items.Insert(0, item)
    member __.Dequeue() = dequeue()
    member __.Size() = items.Count
    member __.Peek() = if items.Count = 0 then None else Some(items.[items.Count - 1])
    
type private QueueMsg<'a> = 
    | Enqueue of 'a
    | Dequeue of AsyncReplyChannel<'a option> 
    | Peek of AsyncReplyChannel<'a option> 
    | Size of AsyncReplyChannel<int>    
    
type SafeQueue<'a>() =
    let agent = MailboxProcessor.Start(fun inbox ->
        let rec loop items = async {
            let! msg = inbox.Receive()
            match msg with
            | Enqueue item -> return! loop (item::items)
            | Dequeue reply ->
                match items |> List.rev with
                | [] -> reply.Reply None; return! loop items
                | head::tail ->
                    reply.Reply(Some head)
                    return! loop (tail |> List.rev)
            | Size reply -> reply.Reply items.Length; return! loop items
            | Peek reply ->
                reply.Reply(items |> List.tryLast)
                return! loop items
        }
        loop List.empty)
    
    member __.Enqueue item = agent.Post (Enqueue item)
    member __.Dequeue() = agent.PostAndReply Dequeue
    member __.Size() = agent.PostAndReply Size
    member __.Peek() = agent.PostAndReply Peek