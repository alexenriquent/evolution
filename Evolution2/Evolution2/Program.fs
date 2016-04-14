open System
open System.Linq
open System.Collections.Generic

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
    type Nucleotide = {
        event: string;
        origin: int * int;
        position: int;
        mutable nucleobase: Nucleobase;
        }
     
    // Parses a nucleobase to a character. 
    let nucleobaseToChar nucleobase =
        match nucleobase with
        | A -> 'A'
        | C -> 'C'
        | G -> 'G'
        | T -> 'T'
    
    // Converts a list of nucleobases to a string.   
    let toString dnaList =
        let convertedList = List.ofSeq(dnaList)
        convertedList
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
        |> List.choose(
            function
            | ValidBase b -> 
                Some b
            | InvalidBase ch -> 
                eprintfn "'%c' is invalid" ch
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

    // Transfer a sequence of nucleotides to a list of nucleotides.
    let toNucleotideList nucleotides = 
        let nucleotideList = new List<Nucleotide>()
        nucleotides
        |> Seq.iter (fun item -> nucleotideList.Add(item))
        nucleotideList

    // Construct a list of nucleotides.
    let toNucleotides evt org pos dna =
        dna
        |> List.fold (fun acc item -> (newNucleotide evt org pos item) :: acc) []
        |> List.rev
        |> updateIndex pos
        |> toNucleotideList
    
    // Converts a list of nucleotides to a string.
    let toDNAString nucleotides =
        nucleotides
        |> Seq.fold (fun acc nucleotide -> nucleotide.nucleobase :: acc) []
        |> List.rev
        |> toString   
     
    // Copies a sequence of nucleotides.
    let copy nucleotides =
        nucleotides
        |> Seq.fold (fun acc x -> (newNucleotide x.event x.origin x.position x.nucleobase) :: acc) []
        |> List.rev   
    
    // Determines if two sequences of nucleotides contain common elements. 
    let findCommon (nucleotides1: List<Nucleotide>) (nucleotides2: List<Nucleotide>) =
        let dna1 = Seq.fold (fun acc x -> (x.event, x.origin, x.position) :: acc) [] nucleotides1
        let dna2 = Seq.fold (fun acc x -> (x.event, x.origin, x.position) :: acc) [] nucleotides2
        match dna1.Intersect(dna2).Any() with
        | true -> true
        | false -> false

// This module contains a dictionary of all
module World =

    open DNA

    // A dictionary containing genes.
    let mutable genes = Unchecked.defaultof<SortedDictionary<int * int, List<Nucleotide>>>

    // A dictionary containing the homologous gene tracking results.
    let mutable homologousGenes = Unchecked.defaultof<SortedDictionary<int * int, (int * int) list>>
    
// This module contains the helper functions used in
// the evolution events.
module Utilities =

    // Parses a string to an integer.       
    let Int str =
        Int32.Parse str

// This module contains the evolution events.
module Events =
    
    open DNA
    open World
    open Utilities
    
    // Creates a new gene and adds it to the gene list.
    let create speciesId geneId dna =
        genes.Add((speciesId, geneId), toNucleotides "create" (speciesId, geneId) 0 (toDNA dna))
    
    // A single nucleotide polymorphism (SNP).
    // Replaces a single nucleobase (or nucleotide) within
    // an existing gene with a different nucleobase.
    let snip speciesId geneId index dna =
        genes.[(speciesId, geneId)].[index].nucleobase <- (dna |> toDNA |> (fun x -> x.[0]))
    
    // Inserts a new sequence of DNA to an existing gene.
    let insert speciesId geneId index dna =
        genes.[(speciesId, geneId)].InsertRange(index, (toNucleotides "insert" (speciesId, geneId) index (toDNA dna)))  
    
    // Deletes a section of DNA from an existing gene.
    let delete speciesId geneId index length =
        genes.[(speciesId, geneId)].RemoveRange(index, length)
    
    // Adds a new gene to an existing species that is an exact
    // copy of another gene within the same species.
    let duplicate speciesId geneId1 geneId2 =
        genes.Add((speciesId, geneId1), new List<Nucleotide>(copy genes.[(speciesId, geneId2)]))
    
    // Removes an existing gene from an existing species.
    let loss speciesId geneId =
        genes.Remove((speciesId, geneId)) |> ignore
    
    // Splits an existing gene into 2 genes
    let fission speciesId geneId1 geneId2 index =
        let length = genes.[speciesId, geneId2].Count - index
        let newDNA = new List<Nucleotide>(copy (genes.[speciesId, geneId2].GetRange(index, length)))
        genes.Add((speciesId, geneId1), newDNA)
        genes.[(speciesId, geneId2)].RemoveRange(index, length)                
    
    // Fuses two existing genes to create a new gene.
    let fusion speciesId geneId1 geneId2 =
        genes.[(speciesId, geneId1)].AddRange(new List<Nucleotide>(copy genes.[speciesId, geneId2]))
        loss speciesId geneId2
    
    // Creates a new species from an existing species.
    let speciation speciesId1 speciesId2 =
        let species = genes |> Seq.filter (fun x -> (fst x.Key) = speciesId2)
        match species |> Seq.isEmpty with
        | true -> ()
        | false ->
            species 
            |> Seq.toArray
            |> Array.iter (fun x -> genes.Add((speciesId1, (snd x.Key)), new List<Nucleotide>(copy x.Value)))
    
    // Checks from the event if the specified gene exists 
    // in the gene dictionary.
    let valid event =
        let exists species gene = genes.ContainsKey((species, gene))
        match event |> List.head with
        | "create" | "speciation" -> true
        | "duplicate" | "fission" -> exists (Int event.[1]) (Int event.[3])
        | "fusion" -> exists (Int event.[1]) (Int event.[2]) && 
                      exists (Int event.[1]) (Int event.[3])
        | _ -> exists (Int event.[1]) (Int event.[2])

    // Different types of events which alter existing genes
    // in various ways.
    let events (event: string list) =
        match valid event with
        | false -> ()
        | true ->
            match event |> List.head with
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
    
    // Tracks homologous genes.
    let homologous gene =
        genes |> Seq.fold (fun acc x -> match (findCommon x.Value gene) (*|> List.length = 0*) with
                                        | false -> acc
                                        | true -> x.Key :: acc) [] |> List.sort
    
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
    open World
    
    // Reads the input file and adds each line to a list
    // of strings.
    let readLines path = 
        IO.File.ReadLines path
    
    // Writes the evolution results to file.
    let writeResults filename =
        let results = genes |> Seq.fold (fun acc x -> 
                            ">SE" + (string (fst x.Key)) + 
                            "_G" + (string (snd x.Key)) + 
                            "\n" + (toDNAString x.Value) :: acc) []
                          |> List.rev
                          |> String.concat "\n"
        IO.File.WriteAllText(filename, results + "\n")

    // Writes the homologous gene tracking results to file.
    let writeHomologousResults filename =
        let results = homologousGenes |> Seq.fold (fun acc x -> 
                            "SE" + (string (fst x.Key)) + 
                            "_G" + (string (snd x.Key)) + 
                            ": " + (toHomolog x.Value) :: acc) []
                          |> List.rev
                          |> String.concat "\n"
        IO.File.WriteAllText(filename, results + "\n")

[<EntryPoint>]
let main argv = 
    World.genes <- new SortedDictionary<int * int, List<DNA.Nucleotide>>()
    World.homologousGenes <- new SortedDictionary<int * int, (int * int) list>()
    let filename = argv.[0]
    IO.readLines filename 
    |> Seq.toList
    |> List.map (fun str -> str.Split [|','|] |> Array.toList)
    |> List.iter (Events.events)
    World.genes |> Seq.iter (fun x -> World.homologousGenes.Add(x.Key, (Events.homologous x.Value)))
    IO.writeResults (filename + ".fa")
    IO.writeHomologousResults (filename + ".homologs")
    0 // return an integer exit code
