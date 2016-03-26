using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    /// <summary>
    /// This class contains the gene properties and operations.
    /// </summary>
    public class Gene {

        /// <summary>
        /// The species ID associated with this gene object.
        /// </summary>
        private int speciesId;
        /// <summary>
        /// The gene ID associated with this gene object.
        /// </summary>
        private int geneId;
        /// <summary>
        /// The DNA associated with this gene object.
        /// </summary>
        private DNA dna;

        /// <summary>
        /// The class default constructor.
        /// </summary>
        public Gene() {
            this.speciesId = 0;
            this.geneId = 0;
            this.dna = null;
        }

        /// <summary>
        /// The class constructor with properties.
        /// </summary>
        /// <param name="speciesId">An integer representing a species ID</param>
        /// <param name="geneId">An integer representing a gene ID</param>
        /// <param name="dna">A DNA object</param>
        public Gene(int speciesId, int geneId, DNA dna) {
            this.speciesId = speciesId;
            this.geneId = geneId;
            this.dna = dna;
        }

        /// <summary>
        /// Accessor and mutator for this gene's 
        /// species ID.
        /// </summary>
        public int SpeciesId {
            get { return this.speciesId; }
            set { this.speciesId = value; }
        }

        /// <summary>
        /// Accessor and mutator for this gene's
        /// gene ID.
        /// </summary>
        public int GeneId {
            get { return this.geneId; }
            set { this.geneId = value; }
        }

        /// <summary>
        /// Aaccessor and mutator for this gene's
        /// sequence of DNA.
        /// </summary>
        public DNA Dna {
            get { return this.dna; }
            set { this.dna = value; }
        }
    }
}
