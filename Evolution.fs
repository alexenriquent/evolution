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
    speciesId: string; 
    geneId: string; 
    dna: DNA.Nucleobase list; 
    }
            
module Events =

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
        let dna = DNA.toDNA d
        let newSE = { speciesId = s; geneId = g; dna = dna }
        newSE :: l
        
    let snip l s g i n =
        let target = target l s g
        let result = 
            { speciesId = target.speciesId; 
                geneId = target.geneId; 
                dna = replace (Int32.Parse i) (n |> DNA.toDNA |> List.head) target.dna } 
        result :: (filter l (fun x -> x <> target))
        
    let insert l s g i str =
        let target = target l s g
        let newDNA = split (Int32.Parse i) target.dna
        let result = 
            { speciesId = target.speciesId; 
                geneId = target.geneId; 
                dna = (fst newDNA) @ (DNA.toDNA str) @ (snd newDNA) }
        result :: (filter l (fun x -> x <> target))                               
    
    let delete l s g i len =
        let target = target l s g
        let newDNA = split (Int32.Parse i) target.dna
        let result = 
            { speciesId = target.speciesId; 
                geneId = target.geneId; 
                dna = (fst newDNA) @ (snd (split (Int32.Parse len) (snd newDNA))) }
        result :: (filter l (fun x -> x <> target))       
    
    let events l (e:string list) =
        match e.[0] with
        | "create" -> create l e.[1] e.[2] e.[3]
        | "snip" -> snip l e.[1] e.[2] e.[3] e.[5]
        | "insert" -> insert l e.[1] e.[2] e.[3] e.[4]
        | "delete" -> delete l e.[1] e.[2] e.[3] e.[4]
        | _ -> l
        
module IO =

    let readLines path = 
        IO.File.ReadLines path
    
    let split (str:string) =
        str.Split [|','|] |> Array.toList
        
    let writeLines filename l =
        use file = IO.File.CreateText filename
        l |> List.iter (fun elem -> 
                        fprintfn file ">SE%s_G%s\n%s" 
                            elem.speciesId elem.geneId (DNA.toString elem.dna))
        
    let rec printLines l =
        match l with
        | [] -> printfn ""
        | [head] -> 
            printfn ">SE%s_G%s\n%s" head.speciesId head.geneId (DNA.toString head.dna)
        | head::tail -> 
            printfn ">SE%s_G%s\n%s" head.speciesId head.geneId (DNA.toString head.dna)
            printLines tail

let input = IO.readLines @"/Users/alex/Desktop/Evolution/test10" 
            |> Seq.toList
            |> List.map IO.split

let output = List.fold (fun acc evt -> Events.events acc evt) ([]: Species list) input
            |> List.sort
            |> IO.writeLines @"/Users/alex/Desktop/Evolution/test.fa"
