using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    public static class World {

        public static List<Gene> genes = new List<Gene>();
        
        private static void Create(int species, int gene, string dna) {
            genes.Add(new Gene(species, gene, new DNA(dna)));
        }

        private static void Snip(int species, int gene, int index, string dna) {
            if (GeneExists(species, gene)) {
                int i = GeneIndex(species, gene);
                genes[i].Dna.Dna[index] = Nucleobase.ToDNA(dna)[0];
            }
        }

        private static void Insert(int species, int gene, int index, string dna) {
            if (GeneExists(species, gene)) {
                int i = GeneIndex(species, gene);
                genes[i].Dna.Dna.InsertRange(index, Nucleobase.ToDNA(dna));
            }
        }

        private static void Delete(int species, int gene, int index, int length) {
            if (GeneExists(species, gene)) {
                int i = GeneIndex(species, gene);
                genes[i].Dna.Dna.RemoveRange(index, length);
            }
        }

        private static void Duplicate(int species, int gene1, int gene2) {
            if (GeneExists(species, gene2)) {
                int i = GeneIndex(species, gene2);
                Gene gene = new Gene(species, gene1, new DNA(genes[i].Dna.Dna));
                genes.Add(gene);
            }
        }

        private static void Loss(int species, int gene) {
            if (GeneExists(species, gene)) {
                int i = GeneIndex(species, gene);
                genes.RemoveAt(i);
            }
        }

        private static void Fission(int species, int gene1, int gene2, int index) {
            if (GeneExists(species, gene2)) {
                int i = GeneIndex(species, gene2);
                Gene gene = new Gene(species, gene1, 
                                new DNA(genes[i].Dna.Dna.GetRange
                                (index, genes[i].Dna.Dna.Count - index)));
                genes.Add(gene);
                genes[i].Dna.Dna.RemoveRange(index, genes[i].Dna.Dna.Count - index);
            }
        }

        private static void Fusion(int species, int gene1, int gene2) {
            if (GeneExists(species, gene1) && GeneExists(species, gene2)) {
                int i1 = GeneIndex(species, gene1);
                int i2 = GeneIndex(species, gene2);
                genes[i1].Dna.Dna.AddRange(new List<Nucleobases>(genes[i2].Dna.Dna));
                Loss(species, gene2);
            }
        }

        private static void Speciation(int species1, int species2) {
            List<Gene> species = genes.FindAll(x => x.SpeciesId == species2);
            if (species.Count > 0) {
                foreach (Gene gene in species) {
                    genes.Add(new Gene(species1, gene.GeneId, new DNA(gene.Dna.Dna)));
                }
            }
        }

        public static void Events(string[] action) {
            switch (action[0]) {
                case "create": Create(Int(action[1]), Int(action[2]), action[3]); break;
                case "snip": Snip(Int(action[1]), Int(action[2]), Int(action[3]), action[5]); break;
                case "insert": Insert(Int(action[1]), Int(action[2]), Int(action[3]), action[4]); break;
                case "delete": Delete(Int(action[1]), Int(action[2]), Int(action[3]), Int(action[4])); break;
                case "duplicate": Duplicate(Int(action[1]), Int(action[2]), Int(action[3])); break;
                case "loss": Loss(Int(action[1]), Int(action[2])); break;
                case "fission": Fission(Int(action[1]), Int(action[2]), Int(action[3]), Int(action[4])); break;
                case "fusion": Fusion(Int(action[1]), Int(action[2]), Int(action[3])); break;
                case "speciation": Speciation(Int(action[1]), Int(action[2])); break;
            }
        }

        private static int Int(string str) {
            return Int32.Parse(str);
        }

        private static int GeneIndex(int species, int gene) {
            return genes.FindIndex(x => x.SpeciesId == species && x.GeneId == gene);
        }

        private static bool GeneExists(int species, int gene) {
            return genes.Exists(x => x.SpeciesId == species && x.GeneId == gene);
        }

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
