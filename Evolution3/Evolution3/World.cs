using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    // The World class contains a set of genes and
    // several types of events that alter existing 
    // genes in various ways.
    public static class World {
        
        // A list containing genes.
        public static List<Gene> genes;

        // A sorted dictionary containing the homologous gene tracking results.
        public static SortedDictionary<Tuple<int, int>, List<Tuple<int, int>>> homologousGenes;

        // Creates a new gene and adds it to the gene list.
        private static void Create(int speciesId, int geneId, string dna) {
            genes.Add(new Gene(speciesId, geneId, 
                      new DNA("create", new Tuple<int, int>(speciesId, geneId), dna)));
        }

        // A single nucleotide polymorphism (SNP).
        // Replaces a single nucleobase (or nucleotide) within
        // an existing gene with a different nucleobase.
        private static void Snip(int speciesId, int geneId, int index, string dna) {
            int i = GeneIndex(speciesId, geneId);
            genes[i].Dna.Dna[index].Nucleobase = Nucleobase.ToDNA(dna)[0];
        }

        // Inserts a new sequence of DNA to an existing gene.
        private static void Insert(int speciesId, int geneId, int index, string dna) {
            int i = GeneIndex(speciesId, geneId);
            genes[i].Dna.Dna.InsertRange(index, 
            DNA.ToNucleotides("insert", new Tuple<int, int>(speciesId, geneId), index, dna));
        }

        // Deletes a section of DNA from an existing gene.
        private static void Delete(int speciesId, int geneId, int index, int length) {
            int i = GeneIndex(speciesId, geneId);
            genes[i].Dna.Dna.RemoveRange(index, length);
        }

        // Adds a new gene to an existing species that is an exact
        // copy of another gene within the same species.
        private static void Duplicate(int speciesId, int geneId1, int geneId2) {
            int i = GeneIndex(speciesId, geneId2);
            Gene gene = new Gene(speciesId, geneId1, new DNA(genes[i].Dna.Dna));
            genes.Add(gene);
        }

        // Removes an existing gene from an existing species.
        private static void Loss(int speciesId, int geneId) {
            int i = GeneIndex(speciesId, geneId);
            genes.RemoveAt(i);
        }

        // Splits an existing gene into 2 genes
        private static void Fission(int speciesId, int geneId1, int geneId2, int index) {
            int i = GeneIndex(speciesId, geneId2);
            Gene gene = new Gene(speciesId, geneId1, 
                        new DNA(genes[i].Dna.Dna.GetRange
                        (index, genes[i].Dna.Dna.Count - index)));
            genes.Add(gene);
            genes[i].Dna.Dna.RemoveRange(index, genes[i].Dna.Dna.Count - index);
        }

        // Fuses two existing genes to create a new gene
        private static void Fusion(int speciesId, int geneId1, int geneId2) {
            int i1 = GeneIndex(speciesId, geneId1);
            int i2 = GeneIndex(speciesId, geneId2);
            genes[i1].Dna.Dna.AddRange(new List<Nucleotide>(genes[i2].Dna.Dna));
            Loss(speciesId, geneId2);
        }

        // Creates a new species from an existing species.
        private static void Speciation(int speciesId1, int speciesId2) {
            List<Gene> species = genes.FindAll(x => x.SpeciesId == speciesId2);
            if (species.Count > 0) {
                foreach (Gene gene in species) {
                    genes.Add(new Gene(speciesId1, gene.GeneId, new DNA(gene.Dna.Dna)));
                }
            }
        }

        // Different types of events which alter existing genes
        // in various ways.
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

        // Checks from the event if the specified gene exists 
        // in the gene list.
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

        // Tracks homologous genes of each gene in the gene list.
        public static void Homologous() {
            foreach (Gene gene1 in genes) {
                Tuple<int, int> key = new Tuple<int, int>(gene1.SpeciesId, gene1.GeneId);
                homologousGenes.Add(key, new List<Tuple<int, int>>());
                foreach (Gene gene2 in genes) {
                    if (GeneCommon(gene1, gene2)) {
                        Tuple<int, int> value = new Tuple<int, int>(gene2.SpeciesId, gene2.GeneId);
                        homologousGenes[key].Add(value);
                    }
                }
            }
        }

        // Determines whether two genes are homologous.
        private static bool GeneCommon(Gene gene1, Gene gene2) {
            foreach (Nucleotide nucleotide1 in gene1.Dna.Dna) {
                foreach (Nucleotide nucleotide2 in gene2.Dna.Dna) {
                    if (nucleotide1.Action == nucleotide2.Action &&
                        nucleotide1.Origin == nucleotide2.Origin &&
                        nucleotide1.Position == nucleotide2.Position) {
                        return true;
                    }
                }
            }
            return false;
        }

        // Parses a string to an integer.
        private static int Int(string str) {
            return Int32.Parse(str);
        }

        // Gets a gene index using a specified predicate 
        // from the gene list.
        private static int GeneIndex(int speciesId, int geneId) {
            return genes.FindIndex(x => x.SpeciesId == speciesId && x.GeneId == geneId);
        }

        // Checks if a gene with a specified predicate
        // exists in the gene list.
        private static bool GeneExists(int speciesId, int geneId) {
            return genes.Exists(x => x.SpeciesId == speciesId && x.GeneId == geneId);
        }

        // Sorts and formats the gene list.
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

        // Sorts and formats the homologous gene dictionary.
        public static List<string> HomologousGenes() {
            List<string> geneList = new List<string>();
            foreach (var gene in homologousGenes) {
                string geneStr = "SE" + gene.Key.Item1 +
                                 "_G" + gene.Key.Item2 + ": ";
                List<Tuple<int, int>> sortedGenes = gene.Value.OrderBy(x => x.Item1).
                                                    ThenBy(x => x.Item2).ToList();
                foreach (var nucleotide in sortedGenes) {
                    geneStr += "SE" + nucleotide.Item1 +
                               "_G" + nucleotide.Item2 + " ";
                }
                geneStr = geneStr.Remove(geneStr.Length - 1);
                geneList.Add(geneStr);
            }
            return geneList;
        }
    }
}
