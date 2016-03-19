open System

module DNA =

    type Nucleobase = A | C | G | T  
    type ParsedChar = ValidBase of Nucleobase | InvalidBase of char

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
                    
type Species = { 
    speciesId: int; 
    geneId: int; 
    dna: DNA.Nucleobase list; 
    }
            
module Events =

    let toInt s =
        Int32.Parse s

    let target l s g =
        l 
        |> List.filter (fun x -> x.speciesId = s && x.geneId = g) 
        |> List.head

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
        { speciesId = s; geneId = g; dna = (DNA.toDNA d) } :: l
        
    let snip l s g i n =
        let target = target l s g
        let result = 
            { speciesId = s; 
                geneId = g; 
                dna = replace i (n |> DNA.toDNA |> List.head) target.dna } 
        result :: (filter l (fun x -> x <> target))

    let insert l s g i str =
        let target = target l s g
        let newDNA = split i target.dna
        let result = 
            { speciesId = s; 
                geneId = g; 
                dna = (fst newDNA) @ (DNA.toDNA str) @ (snd newDNA) }
        result :: (filter l (fun x -> x <> target))                               
    
    let delete l s g i len =
        let target = target l s g
        let newDNA = split i target.dna
        let result = 
            { speciesId = s; 
                geneId = g; 
                dna = (fst newDNA) @ (snd (split len (snd newDNA))) }
        result :: (filter l (fun x -> x <> target))       
    
    let duplicate l s g1 g2 =
        let target = target l s g2
        let result = 
            { speciesId = s; 
                geneId = g1; 
                dna = target.dna }
        result :: l
        
    let loss l s g =
        let target = target l s g
        filter l (fun x -> x <> target)
        
    let fission l s g1 g2 i =
        let target = target l s g2
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
        let target1 = target l s g1
        let target2 = target l s g2
        let result = 
            { speciesId = s; 
                geneId = g1; 
                dna = target1.dna @ target2.dna }
        result :: (filter l (fun x -> x <> target1 && x <> target2))
    
    let speciation l s1 s2 =
        let target = l |> List.filter (fun x -> x.speciesId = s2)
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
        | "create" -> create l (toInt e.[1]) (toInt e.[2]) e.[3]
        | "snip" -> snip l (toInt e.[1]) (toInt e.[2]) (toInt e.[3]) e.[5]
        | "insert" -> insert l (toInt e.[1]) (toInt e.[2]) (toInt e.[3]) e.[4]
        | "delete" -> delete l (toInt e.[1]) (toInt e.[2]) (toInt e.[3]) (toInt e.[4])
        | "duplicate" -> duplicate l (toInt e.[1]) (toInt e.[2]) (toInt e.[3])
        | "loss" -> loss l (toInt e.[1]) (toInt e.[2])
        | "fission" -> fission l (toInt e.[1]) (toInt e.[2]) (toInt e.[3]) (toInt e.[4])
        | "fusion" -> fusion l (toInt e.[1]) (toInt e.[2]) (toInt e.[3])
        | "speciation" -> speciation l (toInt e.[1]) (toInt e.[2])
        | _ -> l
        
module IO =

    let readLines path = 
        IO.File.ReadLines path
    
    let split (str:string) =
        str.Split [|','|] |> Array.toList
        
    let writeLines filename l =
        use file = IO.File.CreateText filename
        l |> List.iter (fun elem -> 
                        fprintfn file ">SE%d_G%d\n%s" 
                            elem.speciesId elem.geneId (DNA.toString elem.dna))
        
    let rec printLines l =
        match l with
        | [] -> printfn ""
        | [head] -> 
            printfn ">SE%d_G%d\n%s" head.speciesId head.geneId (DNA.toString head.dna)
        | head::tail -> 
            printfn ">SE%d_G%d\n%s" head.speciesId head.geneId (DNA.toString head.dna)
            printLines tail

let input = IO.readLines @"/Users/alex/Repository/Evolution/test10" 
            |> Seq.toList
            |> List.map IO.split

let output = List.fold (fun acc evt -> Events.events acc evt) ([]: Species list) input
            |> List.sort
            |> IO.writeLines @"/Users/alex/Repository/Evolution/test.fa"
