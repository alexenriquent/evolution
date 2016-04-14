using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    // This class contains the DNA properties and operations.
    public class DNA {

        // A list of nucleobases.
        private List<Nucleotide> dna;

        // The DNA class default constructor.
        public DNA() {
            this.dna = new List<Nucleotide>();
        }

        // The class contructor given a DNA string.
        public DNA(string action, Tuple<int, int> origin, string dna) {
            this.dna = new List<Nucleotide>();
            List<Nucleobases> nucleobases = Nucleobase.ToDNA(dna);
            for (int i = 0; i < nucleobases.Count; i++) {
                this.dna.Add(new Nucleotide(action, origin, i, nucleobases[i]));
            }
        }

        // The class contructor given a nucletide list.
        public DNA(List<Nucleotide> dna) {
            this.dna = new List<Nucleotide>();
            for (int i = 0; i < dna.Count; i++) {
                this.dna.Add(new Nucleotide(dna[i].Action, dna[i].Origin, dna[i].Position, dna[i].Nucleobase));
            }
        }

        // Accessor and mutator for this DNA.
        public List<Nucleotide> Dna {
            get { return this.dna; }
            set { this.dna = value; }
        }

        // Accessor and mutator for this DNA
        // with an index to access or modify a single 
        // nucleobase.
        public Nucleotide this[int index] {
            get { return this.dna[index]; }
            set { this.dna[index] = value; }
        }

        // Constructs a section of DNA sequence
        public static List<Nucleotide> ToNucleotides(string action, Tuple<int, int> origin, int position, string dna) {
            List<Nucleotide> nucleotides = new List<Nucleotide>();
            List<Nucleobases> nucleobases = Nucleobase.ToDNA(dna);
            for (int i = 0; i < nucleobases.Count; i++) {
                nucleotides.Add(new Nucleotide(action, origin, position + i, nucleobases[i]));
            }
            return nucleotides;
        }

        // Converts a list of nucleobases to a string.
        public override string ToString() {
            string dnaStr = "";
            foreach (Nucleotide nucleotide in this.dna) {
                switch (nucleotide.Nucleobase) {
                    case Nucleobases.A: dnaStr += 'A'; break;
                    case Nucleobases.C: dnaStr += 'C'; break;
                    case Nucleobases.T: dnaStr += 'T'; break;
                    case Nucleobases.G: dnaStr += 'G'; break;
                }
            }
            return dnaStr;
        }
    }
}
