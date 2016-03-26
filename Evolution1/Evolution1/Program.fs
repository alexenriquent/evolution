open System

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
        dnaList
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
        toNucleobaseList str
        |> List.choose (
            function
            | ValidBase b -> 
                Some b
            | InvalidBase ch -> 
                None
            )

module Utilities =
    
    let Int str =
        Int32.Parse str
        
    let replace index value =
        List.mapi (fun i x -> 
                    match i = index with
                    | true -> value
                    | false -> x)
    
    let split index dna =
        let fstDNA = dna |> Seq.take index |> Seq.toList
        let sndDNA = dna |> Seq.skip index |> Seq.toList
        (fstDNA, sndDNA)

module Events =

    open DNA
    open Utilities
        
    let create genes species gene dna =
        Map.add (species, gene) (toDNA dna) genes
        
    let snip genes species gene index dna =
        match genes |> Map.containsKey (species, gene) with
        | false -> genes
        | true ->
            let newDNA = replace index (dna |> toDNA |> List.head) 
                            (genes |> Map.find (species, gene))
            genes 
            |> Map.remove (species, gene) 
            |> Map.add (species, gene) newDNA
    
    let insert genes species gene index dna =
        match genes |> Map.containsKey (species, gene) with
        | false -> genes
        | true ->
            let newDNA = split index (genes |> Map.find (species, gene))
            genes   
            |> Map.remove (species, gene) 
            |> Map.add (species, gene) 
                ((fst newDNA) @ (toDNA dna) @ (snd newDNA))
    
    let delete genes species gene index length =
        match genes |> Map.containsKey (species, gene) with
        | false -> genes
        | true ->
            let newDNA = split index (genes |> Map.find (species, gene))
            genes 
            |> Map.remove (species, gene) 
            |> Map.add (species, gene) 
                ((fst newDNA) @ (snd (split length (snd newDNA))))           
    
    let duplicate genes species gene1 gene2 =
        match genes |> Map.containsKey (species, gene2) with
        | false -> genes
        | true ->
            genes |> Map.add (species, gene1) (genes |> Map.find (species, gene2))
    
    let loss genes species gene =
        genes |> Map.remove (species, gene)
        
    let fission genes species gene1 gene2 index =
        match genes |> Map.containsKey (species, gene2) with
        | false -> genes
        | true ->
            let newDNA = split index (genes |> Map.find (species, gene2))
            genes 
            |> Map.remove (species, gene2) 
            |> Map.add (species, gene2) (fst newDNA)
            |> Map.add (species, gene1) (snd newDNA)
                
    let fusion genes species gene1 gene2 =
        match genes |> Map.containsKey (species, gene1) &&
            genes |> Map.containsKey (species, gene2) with
        | false -> genes
        | true ->
            let newDNA = (genes |> Map.find (species, gene1)) @
                            (genes |> Map.find (species, gene2))
            genes 
            |> Map.remove (species, gene1) 
            |> Map.remove (species, gene2)
            |> Map.add (species, gene1) newDNA   
                    
    let speciation genes species1 species2 =
        let species = genes |> Map.filter (fun key value -> (fst key) = species2)
        match species |> Map.isEmpty with
        | true -> genes
        | false ->
            Map.fold (fun state key value -> 
                        state |> Map.add (species1, snd key) value) genes species
                        
    let events genes event =
        match event |> List.head with
        | "create" -> create genes (Int event.[1]) (Int event.[2]) event.[3]
        | "snip" -> snip genes (Int event.[1]) (Int event.[2]) (Int event.[3]) event.[5]
        | "insert" -> insert genes (Int event.[1]) (Int event.[2]) (Int event.[3]) event.[4]
        | "delete" -> delete genes (Int event.[1]) (Int event.[2]) (Int event.[3]) (Int event.[4])
        | "duplicate" -> duplicate genes (Int event.[1]) (Int event.[2]) (Int event.[3])
        | "loss" -> loss genes (Int event.[1]) (Int event.[2])
        | "fission" -> fission genes (Int event.[1]) (Int event.[2]) (Int event.[3]) (Int event.[4])
        | "fusion" -> fusion genes (Int event.[1]) (Int event.[2]) (Int event.[3])
        | "speciation" -> speciation genes (Int event.[1]) (Int event.[2])
        | _ -> genes
    
module IO =

    open DNA
    
    let readLines path = 
        IO.File.ReadLines path
    
    let split (str:string) =
        str.Split [|','|] |> Array.toList
    
    let writeLines filename map =
        use file = IO.File.CreateText filename
        map |> Map.iter (fun key value -> 
                        fprintfn file ">SE%d_G%d\n%s" 
                            (fst key) (snd key) (toString value))

[<EntryPoint>]
let main argv = 
    IO.readLines "Tests/test10" 
    |> Seq.toList
    |> List.map IO.split
    |> List.mapi (fun key value -> key, value)
    |> Map.ofList
    |> Map.fold (fun state key value -> Events.events state value) Map.empty
    |> IO.writeLines "Results/test10.fa" 
    0 // return an integer exit code
