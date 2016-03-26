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
        private List<Nucleobases> dna;

        /// <summary>
        /// The DNA class default constructor.
        /// </summary>
        public DNA() {
            this.dna = new List<Nucleobases>();
        }

        /// <summary>
        /// The class contructor with a DNA string.
        /// </summary>
        /// <param name="dna">A DNA string</param>
        public DNA(string dna) {
            this.dna = new List<Nucleobases>(Nucleobase.ToDNA(dna));
        }

        /// <summary>
        /// The class constructor with a DNA list.
        /// </summary>
        /// <param name="dna">A list of DNA</param>
        public DNA(List<Nucleobases> dna) {
            this.dna = new List<Nucleobases>(dna);
        }

        /// <summary>
        /// Accessor and mutator for this DNA.
        /// </summary>
        public List<Nucleobases> Dna {
            get { return this.dna; }
            set { this.dna = value; }
        }

        /// <summary>
        /// Accessor and mutator for this DNA
        /// with an index to access or modify a single 
        /// nucleobase.
        /// </summary>
        public Nucleobases this[int index] {
            get { return this.dna[index]; }
            set { this.dna[index] = value; }
        }


        /// <summary>
        /// Converts a list of nucleobases to a string.
        /// </summary>
        /// <returns>A string containing nucleobases</returns>
        public override string ToString() {
            string dnaStr = "";
            foreach (Nucleobases nucleobase in this.dna) {
                switch (nucleobase) {
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
