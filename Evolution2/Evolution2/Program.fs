open System
open System.Collections.Generic

/// <summary>
/// This module contains the DNA-related functions.
/// </summary>
module DNA =
    
    /// <summary>
    /// A single case union type of nucleobases, including 
    /// A (Adenine), C (Cytosine), G (Guanine) and T (Thymine).
    /// A nucleobase is not just any character, therefore it has
    /// to be from a limited set.
    /// </summary>
    type Nucleobase = A | C | G | T  

    /// <summary>
    /// A single case union type of nucleobases after being parsed.
    /// </summary>
    type ParsedChar = ValidBase of Nucleobase | InvalidBase of char

    type Nucleotide = {
        event: string;
        origin: int * int;
        position: int;
        mutable nucleobase: Nucleobase;
        }
     
    /// <summary>
    /// Parses a nucleobase to a character.
    /// </summary>
    /// <param name="nucleobase">A single nucleobase</param>
    /// <returns>A character representing the input nucleobase</returns>    
    let nucleobaseToChar nucleobase =
        match nucleobase with
        | A -> 'A'
        | C -> 'C'
        | G -> 'G'
        | T -> 'T'
    
    /// <summary>
    /// Converts a list of nucleobases to a string.
    /// </summary>
    /// <param name="dnaList">A list containing a sequence of DNA</param>
    /// <returns>A string representing a sequence of DNA</returns>    
    let toString dnaList =
        let convertedList = List.ofSeq(dnaList)
        convertedList
        |> List.map nucleobaseToChar
        |> Array.ofList
        |> String
    
    /// <summary>
    /// Parses a character to a nucleobase.
    /// This function parses a valid character into the ValidBase type
    /// and an invalid character to the Invalidbase type. 
    /// </summary>
    /// <param name="ch">A character representing a single nucleobase</param>
    /// <returns>A valid or invalid nucleobase</returns>  
    let charToNucleobase ch =
        match ch with
        | 'A' -> ValidBase A
        | 'C' -> ValidBase C
        | 'G' -> ValidBase G
        | 'T' -> ValidBase T
        | ib -> InvalidBase ib
    
    /// <summary>
    /// Converts a string of nucleobases to a list.
    /// </summary>
    /// <param name="str">A string representing a sequence of DNA</param>
    /// <returns>A list containing a sequence of DNA</returns> 
    let toNucleobaseList (str:string) =
        str.ToCharArray()
        |> List.ofArray
        |> List.map charToNucleobase
    
    /// <summary>
    /// A top-level function to converts a string representing a
    /// sequence of DNA to a list of valid bases by filtering out 
    /// all the invalid bases.
    /// </summary>
    /// <param name="str">A string representing a sequence of DNA</param>
    /// <returns>A list containing a sequence of DNA</returns> 
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
    
    /// <summary>
    /// Updates each nucleotide's position starting from
    /// the given index.
    /// </summary>
    /// <param name="index">The starting index</param>
    /// <returns>A list of nucleotides with updated positions</returns> 
    let rec updateIndex index nucleotides =
        match nucleotides with 
        | [] -> []
        | head::tail ->
            { event = head.event; 
            origin = head.origin;
            position = index;
            nucleobase = head.nucleobase; } :: (updateIndex (index + 1) tail)

    /// <summary>
    /// Construct a nucleotide record.
    /// </summary>
    /// <param name="evt">A string representing an event</param>
    /// <param name="org">A tuple representing an origin</param>
    /// <param name="pos">An integer representing a position</param>
    /// <param name="dna">A nucleobase</param>
    /// <returns>A new necleotide record</returns>
    let newNucleotide evt org pos dna =
        {event = evt; origin = org; position = pos; nucleobase = dna;}

    /// <summary>
    /// Transfer a sequence of nucleotides to a list of nucleotides.
    /// </summary>
    /// <param name="nucleotides">A sequence of nucleotides</param>
    /// <returns>A list of nucleotides</returns>
    let toNucleotideList nucleotides = 
        let nucleotideList = new List<Nucleotide>()
        nucleotides
        |> Seq.iter (fun item -> nucleotideList.Add(item))
        nucleotideList

    /// <summary>
    /// Construct a list of nucleotides.
    /// </summary>
    /// <param name="evt">A string representing an event</param>
    /// <param name="org">A tuple representing an origin</param>
    /// <param name="pos">An integer representing a position</param>
    /// <param name="dna">A nucleobase</param>
    /// <returns>A list of necleotides</returns>
    let toNucleotides evt org pos dna =
        dna
        |> List.fold (fun acc item -> (newNucleotide evt org pos item) :: acc) []
        |> List.rev
        |> updateIndex pos
        |> toNucleotideList
    
    /// <summary>
    /// Converts a list of nucleotides to a string.
    /// </summary>
    /// <param name="nucleotides">A sequence of nucleotides</param>
    /// <returns>A string representing a sequence of nucleotides</returns>
    let toDNAString nucleotides =
        nucleotides
        |> Seq.fold (fun acc nucleotide -> nucleotide.nucleobase :: acc) []
        |> List.rev
        |> toString   
     
    /// <summary>
    /// Copies a sequence of nucleotides.
    /// </summary>
    /// <param name="nucleotides">A sequence of nucleotides</param>
    /// <returns>A new sequence of nucleotides</returns>   
    let copy nucleotides =
        nucleotides
        |> Seq.fold (fun acc x -> (newNucleotide x.event x.origin x.position x.nucleobase) :: acc) []
        |> List.rev   
    
    /// <summary>
    /// Finds common elements between two sequences of nucleotides.
    /// </summary>
    /// <param name="nucleotide1">A sequence of nucleotides</param>
    /// <param name="nucleotide2">A sequence of nucleotides</param>
    /// <returns>A list of common nucleotides</returns>  
    let findCommon (nucleotides1: List<Nucleotide>) (nucleotides2: List<Nucleotide>) =
        nucleotides1
        |> Seq.map (fun x -> nucleotides2 |> Seq.tryFind (fun y ->
                    x.event = y.event && x.origin = y.origin && x.position = y.position))
        |> Seq.fold (fun acc x ->
                    match x <> None with
                    | true -> x.Value :: acc
                    | false -> acc) []
        |> List.rev 

/// <summary>
/// This module contains a dictionary of all
/// genes in the world
/// </summary>
module World =

    open DNA

    /// <summary>
    /// A dictionary containing genes.
    /// </summary>
    let genes = new SortedDictionary<int * int, List<Nucleotide>>()

    /// <summary>
    /// A dictionary containing the homologous gene tracking results.
    /// </summary>
    let homologousGenes = new SortedDictionary<int * int, (int * int) list>()
    
/// <summary>
/// This module contains the helper functions used in
/// the evolution events.
/// </summary>
module Utilities =

    /// <summary>
    /// Parses a string to an integer.
    /// </summary>
    /// <param name="str">A string</param>
    /// <returns>An integer</returns>         
    let Int str =
        Int32.Parse str

/// <summary>
/// This module contains the evolution events.
/// </summary>
module Events =
    
    open DNA
    open World
    open Utilities
    
    /// <summary>
    /// Creates a new gene and adds it to the gene list.
    /// </summary>
    /// <param name="species">An integer representing a species ID</param>
    /// <param name="gene">An integer representing a gene ID</param>
    /// <param name="dna">A string representing a sequence of DNA</param>
    let create species gene dna =
        genes.Add((species, gene), toNucleotides "create" (species, gene) 0 (toDNA dna))
    
    /// <summary>
    /// A single nucleotide polymorphism (SNP).
    /// Replaces a single nucleobase (or nucleotide) within
    /// an existing gene with a different nucleobase.
    /// </summary>
    /// <param name="species">An integer representing a species ID</param>
    /// <param name="gene">An integer representing a gene ID</param>
    /// <param name="index">An integer representing the index of 
    /// nucleobase to be replaced</param>
    /// <param name="dna">A string representing a single nucleobase</param>
    let snip species gene index dna =
        genes.[(species, gene)].[index].nucleobase <- (dna |> toDNA |> (fun x -> x.[0]))
    
    /// <summary>
    /// Inserts a new sequence of DNA to an existing gene.
    /// </summary>
    /// <param name="species">An integer representing a species ID</param>
    /// <param name="gene">An integer representing a gene ID</param>
    /// <param name="index">An integer representing the index to be inserted</param>
    /// <param name="dna">A string representing a sequence of DNA</param>
    let insert species gene index dna =
        genes.[(species, gene)].InsertRange(index, (toNucleotides "insert" (species, gene) index (toDNA dna)))  
    
    /// <summary>
    /// Deletes a section of DNA from an existing gene.
    /// </summary>
    /// <param name="species">An integer representing a species ID</param>
    /// <param name="gene">An integer representing a gene ID</param>
    /// <param name="index">An index to start the delete operation</param>
    /// <param name="length">Length of the section to be deleted</param>
    let delete species gene index length =
        genes.[(species, gene)].RemoveRange(index, length)
    
    /// <summary>
    /// Adds a new gene to an existing species that is an exact
    /// copy of another gene within the same species.
    /// </summary>
    /// <param name="species">An integer representing a species ID</param>
    /// <param name="gene1">An integer representing the gene to be added</param>
    /// <param name="gene2">An integer representing the gene to be copied</param>
    let duplicate species gene1 gene2 =
        genes.Add((species, gene1), new List<Nucleotide>(copy genes.[(species, gene2)]))
    
    /// <summary>
    /// Removes an existing gene from an existing species.
    /// </summary>
    /// <param name="species">An integer representing a species ID</param>
    /// <param name="gene">An integer representing a gene ID</param>
    let loss species gene =
        genes.Remove((species, gene)) |> ignore
    
    /// <summary>
    /// Splits an existing gene into 2 genes.
    /// </summary>
    /// <param name="species">An integer representing a species ID</param>
    /// <param name="gene1">An integer representing the gene to be created</param>
    /// <param name="gene2">An integer representing the gene to be split</param>
    /// <param name="index">An integer representing the spliting index</param>
    let fission species gene1 gene2 index =
        let length = genes.[species, gene2].Count - index
        let newDNA = new List<Nucleotide>(copy (genes.[species, gene2].GetRange(index, length)))
        genes.Add((species, gene1), newDNA)
        genes.[(species, gene2)].RemoveRange(index, length)                
    
    /// <summary>
    /// Fuses two existing genes to create a new gene.
    /// </summary>
    /// <param name="species">An integer representing a species ID</param>
    /// <param name="gene1">An integer representing the gene to be fused</param>
    /// <param name="gene2">An integer representing the gene to be removed</param>
    let fusion species gene1 gene2 =
        genes.[(species, gene1)].AddRange(new List<Nucleotide>(copy genes.[species, gene2]))
        loss species gene2
    
    /// <summary>
    /// Creates a new species from an existing species.
    /// </summary>
    /// <param name="species1">An integer representing the new species
    /// to be created</param>
    /// <param name="species2">An integer representing the existing species 
    /// to be copied</param> 
    let speciation species1 species2 =
        let species = genes |> Seq.filter (fun x -> (fst x.Key) = species2)
        match species |> Seq.isEmpty with
        | true -> ()
        | false ->
            species 
            |> Seq.toArray
            |> Array.iter (fun x -> genes.Add((species1, (snd x.Key)), new List<Nucleotide>(copy x.Value)))
    
    /// <summary>
    /// Checks from the event if the specified gene exists 
    /// in the gene dictionary.
    /// </summary>
    /// <param name="event">A list containing an event</param>
    /// <returns>True if the specified gene exists in the gene dictionary,
    /// false otherwise</returns>
    let valid event =
        let exists species gene = genes.ContainsKey((species, gene))
        match event |> List.head with
        | "create" | "speciation" -> true
        | "duplicate" | "fission" -> exists (Int event.[1]) (Int event.[3])
        | "fusion" -> exists (Int event.[1]) (Int event.[2]) && 
                      exists (Int event.[1]) (Int event.[3])
        | _ -> exists (Int event.[1]) (Int event.[2])

    /// <summary>
    /// Different types of events which alter existing genes
    /// in various ways.
    /// </summary>
    /// <param name="action">An array containing events</param> 
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
    
    /// <summary>
    /// Tracks homologous genes.
    /// </summary>
    /// <param name="gene">The sequence of nucleotides to be compared</param>
    /// <returns>A list of homologous genes</returns> 
    let homologous gene =
        genes |> Seq.fold (fun acc x -> match (findCommon x.Value gene) |> List.length = 0 with
                                        | true -> acc
                                        | false -> x.Key :: acc) [] |> List.sort
    
    /// <summary>
    /// Formats the homologous gene tracking results.
    /// </summary>
    /// <param name="genes">The result list</param>
    /// <returns>A list of homologous genes</returns> 
    let toHomolog genes =
        genes
        |> List.fold (fun acc (key1, key2) -> "SE" + (string key1) + "_G" + (string key2) :: acc) []
        |> List.rev
        |> String.concat " "

/// <summary>
/// This module contains the I/O operations.
/// </summary>   
module IO =

    open DNA
    open Events
    open World
    
    /// <summary>
    /// Reads the input file and adds each line to a list
    /// of strings.
    /// </summary>
    /// <param name="path">A file path/filename</param>
    /// <returns>A sequence containing all lines from file</returns>
    let readLines path = 
        IO.File.ReadLines path
    
    /// <summary>
    /// Writes the evolution results to file.
    /// </summary>
    /// <param name="filename">A file path/filename</param>
    let writeResults filename =
        use file = IO.File.CreateText filename
        genes |> Seq.iter (fun x -> 
                        fprintfn file ">SE%d_G%d\n%s" 
                            (fst x.Key) (snd x.Key) (toDNAString x.Value))

    /// <summary>
    /// Writes the homologous gene tracking results to file.
    /// </summary>
    /// <param name="filename">A file path/filename</param>
    let writeHomologousResults filename =
        use file = IO.File.CreateText filename
        homologousGenes |> Seq.iter (fun x -> 
                        fprintfn file "SE%d_G%d: %s" 
                            (fst x.Key) (snd x.Key) (toHomolog x.Value))

[<EntryPoint>]
/// <summary>
/// The Main method for the application.
/// </summary>
/// <param name="args">A list of command line arguments</param>
let main argv = 
    IO.readLines argv.[0] 
    |> Seq.toList
    |> List.map (fun str -> str.Split [|','|] |> Array.toList)
    |> List.iter (Events.events)
    World.genes |> Seq.iter (fun x -> World.homologousGenes.Add(x.Key, (Events.homologous x.Value)))
    IO.writeResults "Results/test10.fa"
    IO.writeHomologousResults "Results/test10.homolog"
    0 // return an integer exit code
