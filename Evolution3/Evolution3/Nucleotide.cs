using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    // This class contains the Nucleotide properties and operations
    public class Nucleotide {

        // The event associated with this nucleotide object
        private string action;
        // The origin of this nucleotide object.
        private Tuple<int, int> origin;
        // The position of this nucleotide object.
        private int position;
        // The nucleobase associated with this nucleotide object.
        private Nucleobases nucleobase;

        // The Nucleotide class constructor
        public Nucleotide(string action, Tuple<int, int> origin, int position, Nucleobases nucleobase) {
            this.action = action;
            this.origin = origin;
            this.position = position;
            this.nucleobase = nucleobase;
        }

        // Accessor and mutator for this nucleotide's event
        public string Action {
            get { return this.action; }
            set { this.action = value; }
        }

        // Accessor and mutator for this nucleotide's origin
        public Tuple<int,int> Origin {
            get { return this.origin; }
            set { this.origin = value; }
        }

        // Accessor and mutator for this nucleotide's position
        public int Position {
            get { return this.position; }
            set { this.position = value; }
        }

        // Accessor and mutator for this nucleotide's nucleobase
        public Nucleobases Nucleobase {
            get { return this.nucleobase; }
            set { this.nucleobase = value; }
        }
    }
}
