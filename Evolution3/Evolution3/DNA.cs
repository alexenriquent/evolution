using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    /// <summary>
    /// This class contains the DNA properties and operations.
    /// </summary>
    public class DNA {

        /// <summary>
        /// A list of nucleobases.
        /// </summary>
        private List<Nucleotide> dna;

        /// <summary>
        /// The DNA class default constructor.
        /// </summary>
        public DNA() {
            this.dna = new List<Nucleotide>();
        }

        /// <summary>
        /// The class contructor given a DNA string.
        /// </summary>
        /// <param name="action">A string representing an event</param>
        /// <param name="origin">A tuple representing a gene origin</param>
        /// <param name="dna">A string representing a sequence of DNA</param>
        public DNA(string action, Tuple<int, int> origin, string dna) {
            this.dna = new List<Nucleotide>();
            List<Nucleobases> nucleobases = Nucleobase.ToDNA(dna);
            for (int i = 0; i < nucleobases.Count; i++) {
                this.dna.Add(new Nucleotide(action, origin, i, nucleobases[i]));
            }
        }

        /// <summary>
        /// The class contructor given a nucletide list.
        /// </summary>
        /// <param name="dna">A nucleotide list</param>
        public DNA(List<Nucleotide> dna) {
            this.dna = new List<Nucleotide>();
            for (int i = 0; i < dna.Count; i++) {
                this.dna.Add(new Nucleotide(dna[i].Action, dna[i].Origin, dna[i].Position, dna[i].Nucleobase));
            }
        }

        /// <summary>
        /// Accessor and mutator for this DNA.
        /// </summary>
        public List<Nucleotide> Dna {
            get { return this.dna; }
            set { this.dna = value; }
        }

        /// <summary>
        /// Accessor and mutator for this DNA
        /// with an index to access or modify a single 
        /// nucleobase.
        /// </summary>
        public Nucleotide this[int index] {
            get { return this.dna[index]; }
            set { this.dna[index] = value; }
        }

        /// <summary>
        /// Constructs a section of DNA sequence
        /// </summary>
        /// <param name="action">A string representing an event</param>
        /// <param name="origin">A tuple representing a gene origin</param>
        /// <param name="position">An integer representing a gene position</param>
        /// <param name="dna">A string representing a sequence of DNA</param>
        /// <returns></returns>
        public static List<Nucleotide> ToNucleotides(string action, Tuple<int, int> origin, int position, string dna) {
            List<Nucleotide> nucleotides = new List<Nucleotide>();
            List<Nucleobases> nucleobases = Nucleobase.ToDNA(dna);
            for (int i = 0; i < nucleobases.Count; i++) {
                nucleotides.Add(new Nucleotide(action, origin, position + i, nucleobases[i]));
            }
            return nucleotides;
        }

        /// <summary>
        /// Converts a list of nucleobases to a string.
        /// </summary>
        /// <returns>A string containing nucleobases</returns>
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
