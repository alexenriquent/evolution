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
        
    let create map species gene dna =
        Map.add (species, gene) (toDNA dna) map
        
    let snip map species gene index dna =
        match map |> Map.containsKey (species, gene) with
        | false -> map
        | true ->
            let newDNA = replace index (dna |> toDNA |> List.head) 
                            (map |> Map.find (species, gene))
            map |> Map.remove (species, gene) 
                |> Map.add (species, gene) newDNA
    
    let insert map species gene index dna =
        match map |> Map.containsKey (species, gene) with
        | false -> map
        | true ->
            let newDNA = split index (map |> Map.find (species, gene))
            map |> Map.remove (species, gene) 
                |> Map.add (species, gene) 
                    ((fst newDNA) @ (toDNA dna) @ (snd newDNA))
    
    let delete map species gene index length =
        match map |> Map.containsKey (species, gene) with
        | false -> map
        | true ->
            let newDNA = split index (map |> Map.find (species, gene))
            map |> Map.remove (species, gene) 
                |> Map.add (species, gene) 
                    ((fst newDNA) @ (snd (split length (snd newDNA))))           
    
    let duplicate map species gene1 gene2 =
        match map |> Map.containsKey (species, gene2) with
        | false -> map
        | true ->
            map |> Map.add (species, gene1) (map |> Map.find (species, gene2))
    
    let loss map species gene =
        map |> Map.remove (species, gene)
        
    let fission map species gene1 gene2 index =
        match map |> Map.containsKey (species, gene2) with
        | false -> map
        | true ->
            let newDNA = split index (map |> Map.find (species, gene2))
            map |> Map.remove (species, gene2) 
                |> Map.add (species, gene2) (fst newDNA)
                |> Map.add (species, gene1) (snd newDNA)
                
    let fusion map species gene1 gene2 =
        match map |> Map.containsKey (species, gene1) with
        | false -> map
        | true ->
            match map |> Map.containsKey (species, gene2) with
            | false -> map
            | true ->
                let newDNA = (map |> Map.find (species, gene1)) @
                                (map |> Map.find (species, gene2))
                map |> Map.remove (species, gene1) 
                    |> Map.remove (species, gene2)
                    |> Map.add (species, gene1) newDNA   
                    
    let speciation map species1 species2 =
        let species = map |> Map.filter (fun key value -> (fst key) = species2)
        match species |> Map.isEmpty with
        | true -> map
        | false ->
            Map.fold (fun state key value -> 
                        state |> Map.add (species1, snd key) value) map species
                        
    let events map event =
        match event |> List.head with
        | "create" -> create map (Int event.[1]) (Int event.[2]) event.[3]
        | "snip" -> snip map (Int event.[1]) (Int event.[2]) (Int event.[3]) event.[5]
        | "insert" -> insert map (Int event.[1]) (Int event.[2]) (Int event.[3]) event.[4]
        | "delete" -> delete map (Int event.[1]) (Int event.[2]) (Int event.[3]) (Int event.[4])
        | "duplicate" -> duplicate map (Int event.[1]) (Int event.[2]) (Int event.[3])
        | "loss" -> loss map (Int event.[1]) (Int event.[2])
        | "fission" -> fission map (Int event.[1]) (Int event.[2]) (Int event.[3]) (Int event.[4])
        | "fusion" -> fusion map (Int event.[1]) (Int event.[2]) (Int event.[3])
        | "speciation" -> speciation map (Int event.[1]) (Int event.[2])
        | _ -> map
    
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
    |> Map.fold (fun state key value -> DNA.events state value) Map.empty
    |> IO.writeLines "Results/test10.fa" 
    0 // return an integer exit code
