open System
open System.Collections.Generic

module DNA =

    type Nucleobase = A | C | G | T  
    type ParsedChar = ValidBase of Nucleobase | InvalidBase of char
    type Genome = { 
        speciesId: int; 
        geneId: int; 
        dna: List<Nucleobase>; 
        }
        
    let DNAList = new List<Genome>()

    let nucleobaseToChar n =
        match n with
        | A -> 'A'
        | C -> 'C'
        | G -> 'G'
        | T -> 'T'
        
    let toString l =
        let convertedList = List.ofSeq(l)
        convertedList
        |> List.map nucleobaseToChar
        |> Array.ofList
        |> String

    let charToNucleobase ch =
        match ch with
        | 'A' -> ValidBase A
        | 'C' -> ValidBase C
        | 'G' -> ValidBase G
        | 'T' -> ValidBase T
        | ib -> InvalidBase ib
    
    let toNucleobaseList (str:string) =
        str.ToCharArray()
        |> List.ofArray
        |> List.map charToNucleobase
    
    let toDNA str =
        let nucleobaseList = new List<Nucleobase>()
        toNucleobaseList str
        |> List.choose(
            function
            | ValidBase b -> 
                Some b
            | InvalidBase ch -> 
                eprintfn "'%c' is invalid" ch
                None
            )
        |> List.iter (fun x -> nucleobaseList.Add(x))
        nucleobaseList
            
    let Int str =
        Int32.Parse str
        
    let split i t =
        let rec splitList i t acc =
            match t with
            | [] -> List.rev acc, []
            | _ when i = 0 -> List.rev acc, t
            | head::tail -> splitList (i-1) tail (head::acc)
        splitList i t []
            
    let create s g d =
        DNAList.Add({ speciesId = s; geneId = g; dna = (toDNA d) })
        
    let snip s g i n =
        if DNAList.Exists(fun x -> x.speciesId = s && x.geneId = g) then
            let index = DNAList.FindIndex(fun x -> x.speciesId = s && x.geneId = g)
            DNAList.[index].dna.[i] <- (n |> toDNA |> (fun x -> x.[0]))          
            
    let events (e: string list) =
        match e.[0] with
        | "create" -> create (Int e.[1]) (Int e.[2]) e.[3]
        | "snip" -> snip (Int e.[1]) (Int e.[2]) (Int e.[3]) e.[5]
        | _ -> ()
            
module IO =
    open DNA
    
    let readLines path = 
        IO.File.ReadLines path
    
    let split (str:string) =
        str.Split [|','|] |> Array.toList
        
    let writeLines filename l =
        use file = IO.File.CreateText filename
        l |> Seq.iter (fun elem -> 
                        fprintfn file ">SE%d_G%d\n%s" 
                            elem.speciesId elem.geneId (toString elem.dna))

[<EntryPoint>]
let main argv = 
    IO.readLines "Tests/test1" 
    |> Seq.toList
    |> List.map IO.split
    |> List.iter (DNA.events)

    IO.writeLines "Results/test_v2.fa" DNA.DNAList
    0 // return an integer exit code
