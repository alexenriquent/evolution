open System

module DNA =

    type Nucleobase = A | C | G | T  
    type ParsedChar = ValidBase of Nucleobase | InvalidBase of char
    type Genome = { 
        speciesId: int; 
        geneId: int; 
        dna: Nucleobase list; 
        }
        
    let nucleobaseToChar n =
        match n with
        | A -> 'A'
        | C -> 'C'
        | G -> 'G'
        | T -> 'T'
        
    let toString l =
        l
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
        |> List.choose(
            function
            | ValidBase b -> 
                Some b
            | InvalidBase ch -> 
                eprintfn "'%c' is invalid" ch
                None
            )
            
    let Int str =
        Int32.Parse str

    let target l s g =
        l |> List.filter (fun x -> x.speciesId = s && x.geneId = g) 

    let replace i s =
        List.mapi (fun index x -> if index = i then s else x )
            
    let split i t =
        let rec splitList i t acc =
            match t with
            | [] -> List.rev acc, []
            | _ when i = 0 -> List.rev acc, t
            | head::tail -> splitList (i-1) tail (head::acc)
        splitList i t []
        
    let filter l f =
        l |> List.filter f
    
    let create l s g d =
        { speciesId = s; geneId = g; dna = (toDNA d) } :: l
        
    let snip l s g i n =
        match (target l s g) with
        | [] -> l
        | _ ->
            let target = target l s g |> List.head
            let result = 
                { speciesId = s; 
                    geneId = g; 
                    dna = replace i (n |> toDNA |> List.head) target.dna } 
            result :: (filter l (fun x -> x <> target))

    let insert l s g i str =
        match (target l s g) with
        | [] -> l
        | _ ->
            let target = target l s g |> List.head
            let newDNA = split i target.dna
            let result = 
                { speciesId = s; 
                    geneId = g; 
                    dna = (fst newDNA) @ (toDNA str) @ (snd newDNA) }
            result :: (filter l (fun x -> x <> target))                               
    
    let delete l s g i len =
        match (target l s g) with
        | [] -> l
        | _ ->
            let target = target l s g |> List.head
            let newDNA = split i target.dna
            let result = 
                { speciesId = s; 
                    geneId = g; 
                    dna = (fst newDNA) @ (snd (split len (snd newDNA))) }
            result :: (filter l (fun x -> x <> target))       
    
    let duplicate l s g1 g2 =
        match (target l s g2) with
        | [] -> l
        | _ ->
            let target = target l s g2 |> List.head
            let result = 
                { speciesId = s; 
                    geneId = g1; 
                    dna = target.dna }
            result :: l
        
    let loss l s g =
        match (target l s g) with
        | [] -> l
        | _ ->
            let target = target l s g |> List.head
            filter l (fun x -> x <> target)
        
    let fission l s g1 g2 i =        
        match (target l s g2) with
        | [] -> l
        | _ ->
            let target = target l s g2 |> List.head
            let newDNA = split i target.dna
            let result1 = 
                { speciesId = s; 
                    geneId = g2; 
                    dna = (fst newDNA) }
            let result2 = 
                { speciesId = s; 
                    geneId = g1; 
                    dna = (snd newDNA) }
            [result1; result2] @ (filter l (fun x -> x <> target))
        
    let fusion l s g1 g2 =
        match (target l s g1) with
        | [] -> l
        | _ ->
            match (target l s g2) with
            | [] -> l
            | _ ->
                let target1 = target l s g1 |> List.head
                let target2 = target l s g2 |> List.head
                let result = 
                    { speciesId = s; 
                        geneId = g1; 
                        dna = target1.dna @ target2.dna }
                result :: (filter l (fun x -> x <> target1 && x <> target2))
    
    let speciation l s1 s2 =
        let target = l |> List.filter (fun x -> x.speciesId = s2)
        match target with
        | [] -> l
        | _ ->
            let rec newSpecies l s =
                match l with
                | [] -> []
                | [head] -> 
                    { speciesId = s; 
                    geneId = head.geneId; 
                    dna = head.dna } :: []
                | head::tail ->
                    { speciesId = s; 
                    geneId = head.geneId; 
                    dna = head.dna } :: newSpecies tail s
            (newSpecies target s1) @ l
                         
    let events l (e:string list) =
        match e.[0] with
        | "create" -> create l (Int e.[1]) (Int e.[2]) e.[3]
        | "snip" -> snip l (Int e.[1]) (Int e.[2]) (Int e.[3]) e.[5]
        | "insert" -> insert l (Int e.[1]) (Int e.[2]) (Int e.[3]) e.[4]
        | "delete" -> delete l (Int e.[1]) (Int e.[2]) (Int e.[3]) (Int e.[4])
        | "duplicate" -> duplicate l (Int e.[1]) (Int e.[2]) (Int e.[3])
        | "loss" -> loss l (Int e.[1]) (Int e.[2])
        | "fission" -> fission l (Int e.[1]) (Int e.[2]) (Int e.[3]) (Int e.[4])
        | "fusion" -> fusion l (Int e.[1]) (Int e.[2]) (Int e.[3])
        | "speciation" -> speciation l (Int e.[1]) (Int e.[2])
        | _ -> l

module IO =
    open DNA
    
    let readLines path = 
        IO.File.ReadLines path
    
    let split (str:string) =
        str.Split [|','|] |> Array.toList
        
    let writeLines filename l =
        use file = IO.File.CreateText filename
        l |> List.iter (fun elem -> 
                        fprintfn file ">SE%d_G%d\n%s" 
                            elem.speciesId elem.geneId (toString elem.dna))

[<EntryPoint>]
let main argv = 
    let input = IO.readLines "Tests/test10" 
                |> Seq.toList
                |> List.map IO.split
    let output = List.fold (fun acc evt -> DNA.events acc evt) ([]: DNA.Genome list) input
                |> List.sort
                |> IO.writeLines "Results/test_v1.fa"
    0 // return an integer exit code
