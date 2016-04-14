open System
open System.Linq

// This module contains the DNA-related functions.
module DNA =
    
    // A single case union type of nucleobases, including 
    // A (Adenine), C (Cytosine), G (Guanine) and T (Thymine).
    // A nucleobase is not just any character, therefore it has
    // to be from a limited set.
    type Nucleobase = A | C | G | T  

    // A single case union type of nucleobases after being parsed.
    type ParsedChar = ValidBase of Nucleobase | InvalidBase of char
    
    // A single nucleotide record
    type Nucleotide = { event: string; origin: int * int; position: int; nucleobase: Nucleobase }
      
    // Parses a nucleobase to a character.
    let nucleobaseToChar nucleobase =
        match nucleobase with
        | A -> 'A'
        | C -> 'C'
        | G -> 'G'
        | T -> 'T'
    
    // Converts a list of nucleobases to a string.      
    let toString dnaList =
        dnaList
        |> List.map nucleobaseToChar
        |> Array.ofList
        |> String

    // Parses a character to a nucleobase.
    // This function parses a valid character into the ValidBase type
    // and an invalid character to the Invalidbase type. 
    let charToNucleobase ch =
        match ch with
        | 'A' -> ValidBase A
        | 'C' -> ValidBase C
        | 'G' -> ValidBase G
        | 'T' -> ValidBase T
        | ib -> InvalidBase ib

    // Converts a string of nucleobases to a list.
    let toNucleobaseList (str:string) =
        str.ToCharArray()
        |> List.ofArray
        |> List.map charToNucleobase
    
    // A top-level function to converts a string representing a
    // sequence of DNA to a list of valid bases by filtering out 
    // all the invalid bases.
    let toDNA str =
        toNucleobaseList str
        |> List.choose (
            function
            | ValidBase b -> 
                Some b
            | InvalidBase ch -> 
                None
            )

    // Updates each nucleotide's position starting from
    // the given index.
    let rec updateIndex index nucleotides =
        match nucleotides with 
        | [] -> []
        | head::tail ->
            { event = head.event; 
            origin = head.origin;
            position = index;
            nucleobase = head.nucleobase; } :: (updateIndex (index + 1) tail)  

    // Construct a nucleotide record.
    let newNucleotide evt org pos dna =
        {event = evt; origin = org; position = pos; nucleobase = dna;}
                  
    // Construct a list of nucleotides.
    let toNucleotides evt org pos dna =
        dna
        |> List.fold (fun acc x -> newNucleotide evt org pos x :: acc) []
        |> List.rev
        |> updateIndex pos
    
    // Converts a list of nucleotides to a string.
    let toDNAString nucleotides =
        nucleotides 
        |> List.fold (fun acc x -> x.nucleobase :: acc) []
        |> List.rev
        |> toString

    // Determines if two sequences of nucleotides contain common elements
    let findCommon (nucleotides1: List<Nucleotide>) (nucleotides2: List<Nucleotide>) =
        let dna1 = Seq.fold (fun acc x -> (x.event, x.origin, x.position) :: acc) [] nucleotides1
        let dna2 = Seq.fold (fun acc x -> (x.event, x.origin, x.position) :: acc) [] nucleotides2
        match dna1.Intersect(dna2).Any() with
        | true -> true
        | false -> false

// This module contains the helper functions used in
// the evolution events.
module Utilities =
    
    // Parses a string to an integer.
    let Int str =
        Int32.Parse str    
    
    // Returns a new list which one of the elements
    // has been replaced with another element.    
    let replace index value =
        List.mapi (fun i x -> 
                    match i = index with
                    | true -> value
                    | false -> x)
    
    // Splits a list and returns a tuple containing the first
    // and second parts of that list.
    let split index list =
        let fstPart = list |> Seq.take index |> Seq.toList
        let sndPart = list |> Seq.skip index |> Seq.toList
        (fstPart, sndPart)
    
    // Finds an element in the map.    
    let find key1 key2 =
        Map.find (key1, key2) 
    
    // Checks if an element exists in the map
    let exists map key1 key2 =
        map |> Map.containsKey (key1, key2)
    
    // Remove the first element of the list.  
    let removeFirst list =
        match list with 
        | [] -> []
        | [head] -> []
        | head::tail -> tail

// This module contains the evolution events.
module Events =

    open DNA
    open Utilities
    
    // Creates a new gene and returns a new map containing that gene.   
    let create genes speciesId geneId dna =
        Map.add (speciesId, geneId) (toNucleotides "create" (speciesId, geneId) 0 (toDNA dna)) genes
     
    // A single nucleotide polymorphism (SNP).
    // Replaces a single nucleobase (or nucleotide) within
    // an existing gene with a different nucleobase.
    let snip genes speciesId geneId index dna =
        let target = genes |> find speciesId geneId |> List.item index
        let nucleobase = [dna |> toDNA |> List.head]
        let nucleotide = toNucleotides target.event target.origin target.position nucleobase
        let nucleotides = split index (genes |> find speciesId geneId) 
        genes 
        |> Map.remove (speciesId, geneId) 
        |> Map.add (speciesId, geneId) ((fst nucleotides) @ nucleotide @ (removeFirst (snd nucleotides)))
    
    // Inserts a new sequence of DNA to an existing gene.
    let insert genes speciesId geneId index dna =
        let nucleotides = split index (genes |> find speciesId geneId)
        let newSection = toNucleotides "insert" (speciesId, geneId) index (toDNA dna)
        genes   
        |> Map.remove (speciesId, geneId) 
        |> Map.add (speciesId, geneId) ((fst nucleotides) @ newSection @ (snd nucleotides))
    
    // Deletes a section of DNA from an existing gene.
    let delete genes speciesId geneId index length =
        let nucleotides = split index (genes |> find speciesId geneId)
        genes 
        |> Map.remove (speciesId, geneId) 
        |> Map.add (speciesId, geneId) ((fst nucleotides) @ (snd (split length (snd nucleotides))))           
    
    // Adds a new gene to an existing species that is an exact
    // copy of another gene within the same species.
    let duplicate genes speciesId geneId1 geneId2 =
        genes |> Map.add (speciesId, geneId1) (genes |> find speciesId geneId2)
    
    // Removes an existing gene from an existing species.
    let loss genes speciesId geneId =
        genes |> Map.remove (speciesId, geneId)
     
    // Splits an existing gene into 2 genes.
    let fission genes speciesId geneId1 geneId2 index =
        let nucleotides = split index (genes |> find speciesId geneId2)
        genes 
        |> Map.remove (speciesId, geneId2) 
        |> Map.add (speciesId, geneId2) (fst nucleotides)
        |> Map.add (speciesId, geneId1) (snd nucleotides)
    
    // Fuses two existing genes to create a new gene.         
    let fusion genes speciesId geneId1 geneId2 =
        let nucleotides = (genes |> find speciesId geneId1) @ (genes |> find speciesId geneId2)
        genes 
        |> Map.remove (speciesId, geneId1) 
        |> Map.remove (speciesId, geneId2)
        |> Map.add (speciesId, geneId1) nucleotides   
    
    // Creates a new species from an existing species              
    let speciation genes speciesId1 speciesId2 =
        let species = genes |> Map.filter (fun key value -> (fst key) = speciesId2)
        match species |> Map.isEmpty with
        | true -> genes
        | false -> Map.fold (fun state key value -> 
                            state |> Map.add (speciesId1, snd key) value) genes species    
    
    // Checks from the event if the specified gene exists 
    // in the gene list
    let valid genes event =
        match event |> List.head with
        | "create" | "speciation" -> true
        | "duplicate" | "fission" -> exists genes (Int event.[1]) (Int event.[3])
        | "fusion" -> exists genes (Int event.[1]) (Int event.[2]) && 
                      exists genes (Int event.[1]) (Int event.[3])
        | _ -> exists genes (Int event.[1]) (Int event.[2])
     
    // Different types of events which alter existing genes
    // in various ways.                 
    let events genes event =
        match valid genes event with
        | false -> genes
        | true ->
            match event |> List.head with
            | "create" -> create genes (Int event.[1]) (Int event.[2]) event.[3]
            | "snip" -> snip genes (Int event.[1]) (Int event.[2]) (Int event.[3]) event.[5]
            | "insert" -> insert genes (Int event.[1]) (Int event.[2]) (Int event.[3]) event.[4]
            | "delete" -> delete genes (Int event.[1]) (Int event.[2]) (Int event.[3]) (Int event.[4])
            | "duplicate" ->  duplicate genes (Int event.[1]) (Int event.[2]) (Int event.[3])
            | "loss" -> loss genes (Int event.[1]) (Int event.[2])
            | "fission" -> fission genes (Int event.[1]) (Int event.[2]) (Int event.[3]) (Int event.[4])
            | "fusion" -> fusion genes (Int event.[1]) (Int event.[2]) (Int event.[3])
            | "speciation" -> speciation genes (Int event.[1]) (Int event.[2])
            | _ -> genes 
    
    // Tracks homologous genes.
    let homologous genes gene =
        genes |> Map.fold (fun state key value -> match findCommon value gene with
                                                    | false -> state
                                                    | true -> key :: state) [] |> List.sort
   
    // Formats the homologous gene tracking results. 
    let toHomolog genes =
        genes
        |> List.fold (fun acc (key1, key2) -> "SE" + (string key1) + "_G" + (string key2) :: acc) []
        |> List.rev
        |> String.concat " "
 
// This module contains the I/O operations.
module IO =

    open DNA
    open Events
    
    // Reads the input file and adds each line to a list
    // of strings.
    let readLines path = 
        IO.File.ReadLines path
    
    // Writes the results to file.
    let writeResults filename map =
        let results = map |> Map.fold (fun state key value -> 
                            ">SE" + (string (fst key)) + 
                            "_G" + (string (snd key)) + 
                            "\n" + (toDNAString value) :: state) []
                          |> List.rev
                          |> String.concat "\n"
        IO.File.WriteAllText(filename, results + "\n")
    
    // Writes the homologous gene tracking results to file.
    let writeHomologousResults filename map =
        let results = map |> Map.fold (fun state key value -> 
                            "SE" + (string (fst key)) + 
                            "_G" + (string (snd key)) + 
                            ": " + (toHomolog value) :: state) []
                          |> List.rev
                          |> String.concat "\n"
        IO.File.WriteAllText(filename, results + "\n")

[<EntryPoint>]
let main argv = 
    let filename = argv.[0] 
    let events = IO.readLines filename 
                |> Seq.toList
                |> List.map (fun str -> str.Split [|','|] |> Array.toList)
                |> List.mapi (fun key value -> key, value)
                |> Map.ofList
    let output = events |> Map.fold (fun state key value -> Events.events state value) Map.empty
    let homologous = output |> Map.fold (fun state key value -> Map.add key (Events.homologous output value) state) Map.empty
    IO.writeResults (filename + ".fa") output
    IO.writeHomologousResults (filename + ".homologs") homologous
    0 // return an integer exit code
