using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    // This class contains the gene properties and operations.
    public class Gene {

        // The species ID associated with this gene object.
        private int speciesId;
        // The gene ID associated with this gene object.
        private int geneId;
        // The DNA associated with this gene object.
        private DNA dna;

        // The class default constructor.
        public Gene() {
            this.speciesId = 0;
            this.geneId = 0;
            this.dna = null;
        }

        // The class constructor with properties.
        public Gene(int speciesId, int geneId, DNA dna) {
            this.speciesId = speciesId;
            this.geneId = geneId;
            this.dna = dna;
        }

        // Accessor and mutator for this gene's species ID.
        public int SpeciesId {
            get { return this.speciesId; }
            set { this.speciesId = value; }
        }

        // Accessor and mutator for this gene's gene ID.
        public int GeneId {
            get { return this.geneId; }
            set { this.geneId = value; }
        }

        // Aaccessor and mutator for this gene's sequence of DNA.
        public DNA Dna {
            get { return this.dna; }
            set { this.dna = value; }
        }
    }
}
