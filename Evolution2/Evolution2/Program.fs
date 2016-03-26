open System
open System.Collections.Generic

module DNA =

    type Nucleobase = A | C | G | T  
    type ParsedChar = ValidBase of Nucleobase | InvalidBase of char
        
    let nucleobaseToChar nucleobase =
        match nucleobase with
        | A -> 'A'
        | C -> 'C'
        | G -> 'G'
        | T -> 'T'
        
    let toString dnaList =
        let convertedList = List.ofSeq(dnaList)
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

module Utilities =
            
    let Int str =
        Int32.Parse str

module Events =
    
    open DNA
    open Utilities

    let genomes = new SortedDictionary<int * int, List<Nucleobase>>()
    
    let create species gene dna =
        genomes.Add((species, gene), toDNA dna)
    
    let snip species gene index dna =
        match genomes.ContainsKey((species, gene)) with
        | false -> ()
        | true -> genomes.[(species, gene)].[index] <- (dna |> toDNA |> (fun x -> x.[0]))
        
    let insert species gene index dna =
        match genomes.ContainsKey((species, gene)) with
        | false -> ()
        | true -> genomes.[(species, gene)].InsertRange(index, (toDNA dna))  
        
    let delete species gene index length =
        match genomes.ContainsKey((species, gene)) with
        | false -> ()
        | true -> genomes.[(species, gene)].RemoveRange(index, length)
        
    let duplicate species gene1 gene2 =
        match genomes.ContainsKey((species, gene2)) with
        | false -> ()
        | true -> genomes.Add((species, gene1), new List<Nucleobase>(genomes.[(species, gene2)]))
     
    let loss species gene =
        match genomes.ContainsKey((species, gene)) with
        | false -> ()
        | true -> genomes.Remove((species, gene)) |> ignore
        
    let fission species gene1 gene2 index =
        match genomes.ContainsKey((species, gene2)) with
        | false -> ()
        | true -> 
            genomes.Add((species, gene1), new List<Nucleobase>(genomes.[species, gene2].
                                GetRange(index, genomes.[species, gene2].Count - index)))
            genomes.[(species, gene2)].RemoveRange(index, genomes.[species, gene2].Count - index)                
            
    let fusion species gene1 gene2 =
        match genomes.ContainsKey((species, gene1)) && 
            genomes.ContainsKey((species, gene2)) with
        | false -> ()
        | true -> 
            genomes.[(species, gene1)].AddRange(new List<Nucleobase>(genomes.[species, gene2]))
            loss species gene2
    
    let speciation species1 species2 =
        let species = genomes |> Seq.filter (fun x -> (fst x.Key) = species2)
        match species |> Seq.isEmpty with
        | true -> ()
        | false ->
            species 
            |> Seq.toArray
            |> Array.iter (fun x -> genomes.Add((species1, (snd x.Key)), new List<Nucleobase>(x.Value)))
                
    let events (event: string list) =
        match (event |> List.head) with
        | "create" -> create (Int event.[1]) (Int event.[2]) event.[3]
        | "snip" -> snip (Int event.[1]) (Int event.[2]) (Int event.[3]) event.[5]
        | "insert" -> insert (Int event.[1]) (Int event.[2]) (Int event.[3]) event.[4]
        | "delete" -> delete (Int event.[1]) (Int event.[2]) (Int event.[3]) (Int event.[4])
        | "duplicate" -> duplicate (Int event.[1]) (Int event.[2]) (Int event.[3])
        | "loss" -> loss (Int event.[1]) (Int event.[2])
        | "fission" -> fission (Int event.[1]) (Int event.[2]) (Int event.[3]) (Int event.[4])
        | "fusion" -> fusion (Int event.[1]) (Int event.[2]) (Int event.[3])
        | "speciation" -> speciation (Int event.[1]) (Int event.[2])
        | _ -> ()

module IO =

    open DNA
    open Events
    
    let readLines path = 
        IO.File.ReadLines path
    
    let split (str:string) =
        str.Split [|','|] |> Array.toList
        
    let writeLines filename =
        use file = IO.File.CreateText filename
        genomes |> Seq.iter (fun x -> 
                        fprintfn file ">SE%d_G%d\n%s" 
                            (fst x.Key) (snd x.Key) (toString x.Value))

[<EntryPoint>]
let main argv = 
    IO.readLines "Tests/test10" 
    |> Seq.toList
    |> List.map IO.split
    |> List.iter (Events.events)

    IO.writeLines "Results/test10.fa"
    0 // return an integer exit code
