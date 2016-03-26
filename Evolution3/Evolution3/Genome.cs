using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    public class Genome {

        private int speciesId;
        private int geneId;
        private DNA dna;

        public Genome() {
            this.speciesId = 0;
            this.geneId = 0;
            this.dna = null;
        }

        public Genome(int speciesId, int geneId, DNA dna) {
            this.speciesId = speciesId;
            this.geneId = geneId;
            this.dna = dna;
        }

        public int SpeciesId {
            get { return this.speciesId; }
            set { this.speciesId = value; }
        }

        public int GeneId {
            get { return this.geneId; }
            set { this.geneId = value; }
        }

        public DNA Dna {
            get { return this.dna; }
            set { this.dna = value; }
        }
    }
}
