open System

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
        dnaList
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
        |> List.choose (
            function
            | ValidBase b -> 
                Some b
            | InvalidBase ch -> 
                None
            )

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
    /// Returns a new list which one of the elements
    /// has been replaced with another element. 
    /// </summary>
    /// <param name="index">An integer representing the index 
    /// of the element to be replaces</param>
    /// <param name="value">A generic type of the value to replace</param>
    /// <returns>A new list which one of the elements
    /// has been replaced</returns>     
    let replace index value =
        List.mapi (fun i x -> 
                    match i = index with
                    | true -> value
                    | false -> x)
    
    /// <summary>
    /// Splits a list and returns a tuple containing the first
    /// and second parts of that list.
    /// </summary>
    /// <param name="index">An integer representing the index 
    /// of the list to be split</param>
    /// <param name="list">The list to be split</param>
    /// <returns>A tuple containing the first
    /// and second parts of that list</returns>  
    let split index list =
        let fstPart = list |> Seq.take index |> Seq.toList
        let sndPart = list |> Seq.skip index |> Seq.toList
        (fstPart, sndPart)

    /// <summary>
    /// Checks if the gene map contains the specified key.
    /// </summary>
    /// <param name="species">An integer representing a species ID</param>
    /// <param name="gene">An integer representing a gene ID</param>
    /// <returns>True if the gene map contains the specified key,
    /// false otherwise</returns>
    let contains species gene =
         Map.containsKey (species, gene)   
    
    /// <summary>
    /// Finds an element in the gene map.
    /// </summary>
    /// <param name="species">An integer representing a species ID</param>
    /// <param name="gene">An integer representing a gene ID</param>
    /// <returns>A map containing the specified element</returns>     
    let find species gene =
        Map.find (species, gene) 

/// <summary>
/// This module contains the evolution events.
/// </summary>
module Events =

    open DNA
    open Utilities
    
    /// <summary>
    /// Creates a new gene and returns a new map containing that gene.
    /// </summary>
    /// <param name="genes">A map containing all genes</param>
    /// <param name="species">An integer representing a species ID</param>
    /// <param name="gene">An integer representing a gene ID</param>
    /// <param name="dna">A string representing a sequence of DNA</param>
    /// <returns>A new map containing all genes including the newly created gene</returns>    
    let create genes species gene dna =
        Map.add (species, gene) (toDNA dna) genes
     
    /// <summary>
    /// A single nucleotide polymorphism (SNP).
    /// Replaces a single nucleobase (or nucleotide) within
    /// an existing gene with a different nucleobase.
    /// </summary>
    /// <param name="genes">A map containing all genes</param>
    /// <param name="species">An integer representing a species ID</param>
    /// <param name="gene">An integer representing a gene ID</param>
    /// <param name="index">An integer representing the index of 
    /// nucleobase to be replaced</param>
    /// <param name="dna">A string representing a single nucleobase</param>
    /// <returns>A new map containing all genes including the modified gene</returns>
    let snip genes species gene index dna =
        match genes |> contains species gene with
        | false -> genes
        | true ->
            let newBase = dna |> toDNA |> List.head
            let newDNA = replace index newBase (genes |> find species gene)
            genes 
            |> Map.remove (species, gene) 
            |> Map.add (species, gene) newDNA
    
    /// <summary>
    /// Inserts a new sequence of DNA to an existing gene.
    /// </summary>
    /// <param name="genes">A map containing all genes</param>
    /// <param name="species">An integer representing a species ID</param>
    /// <param name="gene">An integer representing a gene ID</param>
    /// <param name="index">An integer representing the index to be inserted</param>
    /// <param name="dna">A string representing a sequence of DNA</param>
    /// <returns>A new map containing all genes including the modified gene</returns>
    let insert genes species gene index dna =
        match genes |> contains species gene with
        | false -> genes
        | true ->
            let newDNA = split index (genes |> find species gene)
            genes   
            |> Map.remove (species, gene) 
            |> Map.add (species, gene) ((fst newDNA) @ (toDNA dna) @ (snd newDNA))
    
    /// <summary>
    /// Deletes a section of DNA from an existing gene.
    /// </summary>
    /// <param name="genes">A map containing all genes</param>
    /// <param name="species">An integer representing a species ID</param>
    /// <param name="gene">An integer representing a gene ID</param>
    /// <param name="index">An index to start the delete operation</param>
    /// <param name="length">Length of the section to be deleted</param>
    /// <returns>A new map containing all genes including the modified gene</returns>
    let delete genes species gene index length =
        match genes |> contains species gene with
        | false -> genes
        | true ->
            let newDNA = split index (genes |> find species gene)
            genes 
            |> Map.remove (species, gene) 
            |> Map.add (species, gene) ((fst newDNA) @ (snd (split length (snd newDNA))))           
    
    /// <summary>
    /// Adds a new gene to an existing species that is an exact
    /// copy of another gene within the same species.
    /// </summary>
    /// <param name="genes">A map containing all genes</param>
    /// <param name="species">An integer representing a species ID</param>
    /// <param name="gene1">An integer representing the gene to be added</param>
    /// <param name="gene2">An integer representing the gene to be copied</param>
    /// <returns>A new map containing all genes including the duplicated gene</returns>
    let duplicate genes species gene1 gene2 =
        match genes |> contains species gene2 with
        | false -> genes
        | true -> genes |> Map.add (species, gene1) (genes |> find species gene2)
    
    /// <summary>
    /// Removes an existing gene from an existing species.
    /// </summary>
    /// <param name="genes">A map containing all genes</param>
    /// <param name="species">An integer representing a species ID</param>
    /// <param name="gene">An integer representing a gene ID</param>
    /// <returns>A new map containing all genes except for the removed gene</returns>
    let loss genes species gene =
        genes |> Map.remove (species, gene)
     
    /// <summary>
    /// Splits an existing gene into 2 genes.
    /// </summary>
    /// <param name="genes">A map containing all genes</param>
    /// <param name="species">An integer representing a species ID</param>
    /// <param name="gene1">An integer representing the gene to be created</param>
    /// <param name="gene2">An integer representing the gene to be split</param>
    /// <param name="index">An integer representing the spliting index</param> 
    /// <returns>A new map containing all genes including the split genes</returns>  
    let fission genes species gene1 gene2 index =
        match genes |> contains species gene2 with
        | false -> genes
        | true ->
            let newDNA = split index (genes |> find species gene2)
            genes 
            |> Map.remove (species, gene2) 
            |> Map.add (species, gene2) (fst newDNA)
            |> Map.add (species, gene1) (snd newDNA)
    
    /// <summary>
    /// Fuses two existing genes to create a new gene.
    /// </summary>
    /// <param name="genes">A map containing all genes</param>
    /// <param name="species">An integer representing a species ID</param>
    /// <param name="gene1">An integer representing the gene to be fused</param>
    /// <param name="gene2">An integer representing the gene to be removed</param>
    /// <returns>A new map containing all genes including the fused gene</returns>            
    let fusion genes species gene1 gene2 =
        match genes |> contains species gene1 && genes |> contains species gene2 with
        | false -> genes
        | true ->
            let newDNA = (genes |> find species gene1) @ (genes |> find species gene2)
            genes 
            |> Map.remove (species, gene1) 
            |> Map.remove (species, gene2)
            |> Map.add (species, gene1) newDNA   
    
    /// <summary>
    /// Creates a new species from an existing species.
    /// </summary>
    /// <param name="genes">A map containing all genes</param>
    /// <param name="species1">An integer representing the new species
    /// to be created</param>
    /// <param name="species2">An integer representing the existing species 
    /// to be copied</param> 
    /// <returns>A new map containing all genes including the new set of genes</returns>                
    let speciation genes species1 species2 =
        let species = genes |> Map.filter (fun key value -> (fst key) = species2)
        match species |> Map.isEmpty with
        | true -> genes
        | false -> Map.fold (fun state key value -> 
                            state |> Map.add (species1, snd key) value) genes species
     
    /// <summary>
    /// Different types of events which alter existing genes
    /// in various ways.
    /// </summary>
    /// <param name="genes">A map containing all genes</param>
    /// <param name="action">An array containing events</param> 
    /// <returns>A new map containing all genes</returns>                  
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

/// <summary>
/// This module contains the I/O operations.
/// </summary>    
module IO =

    open DNA
    
    /// <summary>
    /// Reads the input file and adds each line to a list
    /// of strings.
    /// </summary>
    /// <param name="path">A file path/filename</param>
    /// <returns>A sequence containing all lines from file</returns>
    let readLines path = 
        IO.File.ReadLines path
    
    /// <summary>
    /// Writes a list to file.
    /// </summary>
    /// <param name="filename">A file path/filename</param>
    /// <param name="map">A map containing all genes</param>
    let writeLines filename map =
        use file = IO.File.CreateText filename
        map |> Map.iter (fun key value -> 
                            fprintfn file ">SE%d_G%d\n%s" 
                                (fst key) (snd key) (toString value))

[<EntryPoint>]
/// <summary>
/// The Main method for the application.
/// </summary>
/// <param name="args">A list of command line arguments</param>
let main argv = 
    IO.readLines argv.[0] 
    |> Seq.toList
    |> List.map (fun str -> str.Split [|','|] |> Array.toList)
    |> List.mapi (fun key value -> key, value)
    |> Map.ofList
    |> Map.fold (fun state key value -> Events.events state value) Map.empty
    |> IO.writeLines "Results/test10.fa" 
    0 // return an integer exit code
