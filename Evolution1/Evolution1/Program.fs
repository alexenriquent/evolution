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
        
    let create genomes species gene dna =
        Map.add (species, gene) (toDNA dna) genomes
        
    let snip genomes species gene index dna =
        match genomes |> Map.containsKey (species, gene) with
        | false -> genomes
        | true ->
            let newDNA = replace index (dna |> toDNA |> List.head) 
                            (genomes |> Map.find (species, gene))
            genomes |> Map.remove (species, gene) 
                    |> Map.add (species, gene) newDNA
    
    let insert genomes species gene index dna =
        match genomes |> Map.containsKey (species, gene) with
        | false -> genomes
        | true ->
            let newDNA = split index (genomes |> Map.find (species, gene))
            genomes |> Map.remove (species, gene) 
                |> Map.add (species, gene) 
                    ((fst newDNA) @ (toDNA dna) @ (snd newDNA))
    
    let delete genomes species gene index length =
        match genomes |> Map.containsKey (species, gene) with
        | false -> genomes
        | true ->
            let newDNA = split index (genomes |> Map.find (species, gene))
            genomes |> Map.remove (species, gene) 
                    |> Map.add (species, gene) 
                    ((fst newDNA) @ (snd (split length (snd newDNA))))           
    
    let duplicate genomes species gene1 gene2 =
        match genomes |> Map.containsKey (species, gene2) with
        | false -> genomes
        | true ->
            genomes |> Map.add (species, gene1) (genomes |> Map.find (species, gene2))
    
    let loss genomes species gene =
        genomes |> Map.remove (species, gene)
        
    let fission genomes species gene1 gene2 index =
        match genomes |> Map.containsKey (species, gene2) with
        | false -> genomes
        | true ->
            let newDNA = split index (genomes |> Map.find (species, gene2))
            genomes |> Map.remove (species, gene2) 
                    |> Map.add (species, gene2) (fst newDNA)
                    |> Map.add (species, gene1) (snd newDNA)
                
    let fusion genomes species gene1 gene2 =
        match genomes |> Map.containsKey (species, gene1) &&
            genomes |> Map.containsKey (species, gene2) with
        | false -> genomes
        | true ->
            let newDNA = (genomes |> Map.find (species, gene1)) @
                            (genomes |> Map.find (species, gene2))
            genomes |> Map.remove (species, gene1) 
                    |> Map.remove (species, gene2)
                    |> Map.add (species, gene1) newDNA   
                    
    let speciation genomes species1 species2 =
        let species = genomes |> Map.filter (fun key value -> (fst key) = species2)
        match species |> Map.isEmpty with
        | true -> genomes
        | false ->
            Map.fold (fun state key value -> 
                        state |> Map.add (species1, snd key) value) genomes species
                        
    let events genomes event =
        match event |> List.head with
        | "create" -> create genomes (Int event.[1]) (Int event.[2]) event.[3]
        | "snip" -> snip genomes (Int event.[1]) (Int event.[2]) (Int event.[3]) event.[5]
        | "insert" -> insert genomes (Int event.[1]) (Int event.[2]) (Int event.[3]) event.[4]
        | "delete" -> delete genomes (Int event.[1]) (Int event.[2]) (Int event.[3]) (Int event.[4])
        | "duplicate" -> duplicate genomes (Int event.[1]) (Int event.[2]) (Int event.[3])
        | "loss" -> loss genomes (Int event.[1]) (Int event.[2])
        | "fission" -> fission genomes (Int event.[1]) (Int event.[2]) (Int event.[3]) (Int event.[4])
        | "fusion" -> fusion genomes (Int event.[1]) (Int event.[2]) (Int event.[3])
        | "speciation" -> speciation genomes (Int event.[1]) (Int event.[2])
        | _ -> genomes
    
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
