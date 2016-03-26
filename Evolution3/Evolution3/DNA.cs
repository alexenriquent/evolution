using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    public class DNA {

        private List<Nucleobases> dna;

        public DNA() {
            this.dna = new List<Nucleobases>();
        }

        public DNA(string dna) {
            this.dna = new List<Nucleobases>(Nucleobase.ToDNA(dna));
        }

        public DNA(List<Nucleobases> dna) {
            this.dna = new List<Nucleobases>(dna);
        }

        public List<Nucleobases> Dna {
            get { return this.dna; }
            set { this.dna = value; }
        }

        public Nucleobases this[int index] {
            get { return this.dna[index]; }
            set { this.dna[index] = value; }
        }

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
