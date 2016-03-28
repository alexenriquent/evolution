using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    /// <summary>
    /// The World class contains a set of genes and
    /// several types of events that alter existing 
    /// genes in various ways.
    /// </summary>
    public static class World {
        
        /// <summary>
        /// A list containing genes.
        /// </summary>
        public static List<Gene> genes = new List<Gene>();

        /// <summary>
        /// Creates a new gene and adds it to the gene list.
        /// </summary>
        /// <param name="species">An integer representing a species ID</param>
        /// <param name="gene">An integer representing a gene ID</param>
        /// <param name="dna">A string representing a sequence of DNA</param>
        private static void Create(int species, int gene, string dna) {
            genes.Add(new Gene(species, gene, new DNA(dna)));
        }

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
        private static void Snip(int species, int gene, int index, string dna) {
            int i = GeneIndex(species, gene);
            genes[i].Dna.Dna[index] = Nucleobase.ToDNA(dna)[0];
        }

        /// <summary>
        /// Inserts a new sequence of DNA to an existing gene.
        /// </summary>
        /// <param name="species">An integer representing a species ID</param>
        /// <param name="gene">An integer representing a gene ID</param>
        /// <param name="index">An integer representing the index to be inserted</param>
        /// <param name="dna">A string representing a sequence of DNA</param>
        private static void Insert(int species, int gene, int index, string dna) {
            int i = GeneIndex(species, gene);
            genes[i].Dna.Dna.InsertRange(index, Nucleobase.ToDNA(dna));
        }

        /// <summary>
        /// Deletes a section of DNA from an existing gene.
        /// </summary>
        /// <param name="species">An integer representing a species ID</param>
        /// <param name="gene">An integer representing a gene ID</param>
        /// <param name="index">An index to start the delete operation</param>
        /// <param name="length">Length of the section to be deleted</param>
        private static void Delete(int species, int gene, int index, int length) {
            int i = GeneIndex(species, gene);
            genes[i].Dna.Dna.RemoveRange(index, length);
        }

        /// <summary>
        /// Adds a new gene to an existing species that is an exact
        /// copy of another gene within the same species.
        /// </summary>
        /// <param name="species">An integer representing a species ID</param>
        /// <param name="gene1">An integer representing the gene to be added</param>
        /// <param name="gene2">An integer representing the gene to be copied</param>
        private static void Duplicate(int species, int gene1, int gene2) {
            int i = GeneIndex(species, gene2);
            Gene gene = new Gene(species, gene1, new DNA(genes[i].Dna.Dna));
            genes.Add(gene);
        }

        /// <summary>
        /// Removes an existing gene from an existing species.
        /// </summary>
        /// <param name="species">An integer representing a species ID</param>
        /// <param name="gene">An integer representing a gene ID</param>
        private static void Loss(int species, int gene) {
            int i = GeneIndex(species, gene);
            genes.RemoveAt(i);
        }

        /// <summary>
        /// Splits an existing gene into 2 genes.
        /// </summary>
        /// <param name="species">An integer representing a species ID</param>
        /// <param name="gene1">An integer representing the gene to be created</param>
        /// <param name="gene2">An integer representing the gene to be split</param>
        /// <param name="index">An integer representing the spliting index</param>
        private static void Fission(int species, int gene1, int gene2, int index) {
            int i = GeneIndex(species, gene2);
            Gene gene = new Gene(species, gene1, 
                        new DNA(genes[i].Dna.Dna.GetRange
                        (index, genes[i].Dna.Dna.Count - index)));
            genes.Add(gene);
            genes[i].Dna.Dna.RemoveRange(index, genes[i].Dna.Dna.Count - index);
        }

        /// <summary>
        /// Fuses two existing genes to create a new gene.
        /// </summary>
        /// <param name="species">An integer representing a species ID</param>
        /// <param name="gene1">An integer representing the gene to be fused</param>
        /// <param name="gene2">An integer representing the gene to be removed</param>
        private static void Fusion(int species, int gene1, int gene2) {
            int i1 = GeneIndex(species, gene1);
            int i2 = GeneIndex(species, gene2);
            genes[i1].Dna.Dna.AddRange(new List<Nucleobases>(genes[i2].Dna.Dna));
            Loss(species, gene2);
        }

        /// <summary>
        /// Creates a new species from an existing species.
        /// </summary>
        /// <param name="species1">An integer representing the new species
        /// to be created</param>
        /// <param name="species2">An integer representing the existing species 
        /// to be copied</param>
        private static void Speciation(int species1, int species2) {
            List<Gene> species = genes.FindAll(x => x.SpeciesId == species2);
            if (species.Count > 0) {
                foreach (Gene gene in species) {
                    genes.Add(new Gene(species1, gene.GeneId, new DNA(gene.Dna.Dna)));
                }
            }
        }

        /// <summary>
        /// Different types of events which alter existing genes
        /// in various ways.
        /// </summary>
        /// <param name="action">An array containing events</param>
        public static void Events(string[] action) {
            if (Valid(action)) {
                switch (action[0]) {
                    case "create": Create(Int(action[1]), Int(action[2]), action[3]); break;
                    case "snip": Snip(Int(action[1]), Int(action[2]), Int(action[3]), action[5]); break;
                    case "insert": Insert(Int(action[1]), Int(action[2]), Int(action[3]), action[4]); break;
                    case "delete": Delete(Int(action[1]), Int(action[2]), Int(action[3]), Int(action[4])); break;
                    case "duplicate": Duplicate(Int(action[1]), Int(action[2]), Int(action[3])); break;
                    case "loss": Loss(Int(action[1]), Int(action[2])); break;
                    case "fission": Fission(Int(action[1]), Int(action[2]), Int(action[3]), Int(action[4])); break;
                    case "fusion":  Fusion(Int(action[1]), Int(action[2]), Int(action[3])); break;
                    case "speciation": Speciation(Int(action[1]), Int(action[2])); break;
                }
            }
        }

        /// <summary>
        /// Checks from the event if the specified gene exists 
        /// in the gene list.
        /// </summary>
        /// <param name="action">A string array containing an event</param>
        /// <returns>True if the specified gene exists in the gene list,
        /// false otherwise</returns>
        private static bool Valid(string[] action) {
            if (action[0] == "create" || action[0] == "speciation") {
                return true;
            } else if (action[0] == "duplicate" || action[0] == "fission") {
                return GeneExists(Int(action[1]), Int(action[3]));
            } else if (action[0] == "fusion") {
                return GeneExists(Int(action[1]), Int(action[2])) &&
                        GeneExists(Int(action[1]), Int(action[3]));
            } else {
                return GeneExists(Int(action[1]), Int(action[2]));
            }
        }

        /// <summary>
        /// Parses a string to an integer.
        /// </summary>
        /// <param name="str">A string input</param>
        /// <returns>An integer</returns>
        private static int Int(string str) {
            return Int32.Parse(str);
        }

        /// <summary>
        /// Gets a gene index using a specified predicate 
        /// from the gene list.
        /// </summary>
        /// <param name="species">An integer representing a species ID</param>
        /// <param name="gene">An integer representing agene ID</param>
        /// <returns>The index of the specified gene</returns>
        private static int GeneIndex(int species, int gene) {
            return genes.FindIndex(x => x.SpeciesId == species && x.GeneId == gene);
        }

        /// <summary>
        /// Checks if a gene with a specified predicate
        /// exists in the gene list.
        /// </summary>
        /// <param name="species">An integer representing a species ID</param>
        /// <param name="gene">An integer representing a gene ID</param>
        /// <returns>True if the specified gene exists in the gene list,
        /// false otherwise</returns>
        private static bool GeneExists(int species, int gene) {
            return genes.Exists(x => x.SpeciesId == species && x.GeneId == gene);
        }

        /// <summary>
        /// Sorts and formats the gene list.
        /// </summary>
        /// <returns>The formated gene list</returns>
        public static List<string> Genes() {
            List<Gene> sortedGenes = genes.OrderBy(x => x.SpeciesId).
                                        ThenBy(x => x.GeneId).ToList();
            List<string> geneList = new List<string>();
            foreach (Gene gene in sortedGenes) {
                string geneStr = ">SE" + gene.SpeciesId + 
                                    "_G" + gene.GeneId + 
                                    "\n" + gene.Dna;
                geneList.Add(geneStr); 
            }
            return geneList;
        }
    }
}
