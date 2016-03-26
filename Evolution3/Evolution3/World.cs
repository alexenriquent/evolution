using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    public static class World {

        public static List<Genome> genomes = new List<Genome>();
        
        private static void Create(int species, int gene, string dna) {
            genomes.Add(new Genome(species, gene, new DNA(dna)));
        }

        private static void Snip(int species, int gene, int index, string dna) {
            if (GenomeExists(species, gene)) {
                int i = GenomeIndex(species, gene);
                genomes[i].Dna.Dna[index] = Nucleobase.ToDNA(dna)[0];
            }
        }

        private static void Insert(int species, int gene, int index, string dna) {
            if (GenomeExists(species, gene)) {
                int i = GenomeIndex(species, gene);
                genomes[i].Dna.Dna.InsertRange(index, Nucleobase.ToDNA(dna));
            }
        }

        private static void Delete(int species, int gene, int index, int length) {
            if (GenomeExists(species, gene)) {
                int i = GenomeIndex(species, gene);
                genomes[i].Dna.Dna.RemoveRange(index, length);
            }
        }

        private static void Duplicate(int species, int gene1, int gene2) {
            if (GenomeExists(species, gene2)) {
                int i = GenomeIndex(species, gene2);
                Genome genome = new Genome(species, gene1, new DNA(genomes[i].Dna.Dna));
                genomes.Add(genome);
            }
        }

        private static void Loss(int species, int gene) {
            if (GenomeExists(species, gene)) {
                int i = GenomeIndex(species, gene);
                genomes.RemoveAt(i);
            }
        }

        private static void Fission(int species, int gene1, int gene2, int index) {
            if (GenomeExists(species, gene2)) {
                int i = GenomeIndex(species, gene2);
                Genome genome = new Genome(species, gene1, 
                                new DNA(genomes[i].Dna.Dna.GetRange
                                (index, genomes[i].Dna.Dna.Count - index)));
                genomes.Add(genome);
                genomes[i].Dna.Dna.RemoveRange(index, genomes[i].Dna.Dna.Count - index);
            }
        }

        private static void Fusion(int species, int gene1, int gene2) {
            if (GenomeExists(species, gene1) && GenomeExists(species, gene2)) {
                int i1 = GenomeIndex(species, gene1);
                int i2 = GenomeIndex(species, gene2);
                genomes[i1].Dna.Dna.AddRange(new List<Nucleobases>(genomes[i2].Dna.Dna));
                Loss(species, gene2);
            }
        }

        private static void Speciation(int species1, int species2) {
            List<Genome> species = genomes.FindAll(x => x.SpeciesId == species2);
            if (species.Count > 0) {
                foreach (Genome genome in species) {
                    genomes.Add(new Genome(species1, genome.GeneId, new DNA(genome.Dna.Dna)));
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

        private static int GenomeIndex(int species, int gene) {
            return genomes.FindIndex(x => x.SpeciesId == species && x.GeneId == gene);
        }

        private static bool GenomeExists(int species, int gene) {
            return genomes.Exists(x => x.SpeciesId == species && x.GeneId == gene);
        }

        public static List<string> Genomes() {
            List<Genome> sortedGenomes = genomes.OrderBy(x => x.SpeciesId).
                                        ThenBy(x => x.GeneId).ToList();
            List<string> genomeList = new List<string>();
            foreach (Genome genome in sortedGenomes) {
                string genomeStr = ">SE" + genome.SpeciesId + 
                                    "_G" + genome.GeneId + 
                                    "\n" + genome.Dna;
                genomeList.Add(genomeStr); 
            }
            return genomeList;
        }

    }
}
