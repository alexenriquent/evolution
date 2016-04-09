using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    /// <summary>
    /// This class contains the Nucleotide properties and operations.
    /// </summary>
    public class Nucleotide {

        /// <summary>
        /// The event associated with this nucleotide object.
        /// </summary>
        private string action;
        /// <summary>
        /// The origin of this nucleotide object.
        /// </summary>
        private Tuple<int, int> origin;
        /// <summary>
        /// The position of this nucleotide object.
        /// </summary>
        private int position;
        /// <summary>
        /// The nucleobase associated with this nucleotide object.
        /// </summary>
        private Nucleobases nucleobase;

        /// <summary>
        /// The Nucleotide class constructor
        /// </summary>
        /// <param name="action">A string representing an event</param>
        /// <param name="origin">A tuple representing a gene origin</param>
        /// <param name="position">An integer representing a gene position</param>
        /// <param name="nucleobase">A nucleobase</param>
        public Nucleotide(string action, Tuple<int, int> origin, int position, Nucleobases nucleobase) {
            this.action = action;
            this.origin = origin;
            this.position = position;
            this.nucleobase = nucleobase;
        }

        /// <summary>
        /// Accessor and mutator for this nucleotide's event
        /// </summary>
        public string Action {
            get { return this.action; }
            set { this.action = value; }
        }

        /// <summary>
        /// Accessor and mutator for this nucleotide's origin
        /// </summary>
        public Tuple<int,int> Origin {
            get { return this.origin; }
            set { this.origin = value; }
        }

        /// <summary>
        /// Accessor and mutator for this nucleotide's position
        /// </summary>
        public int Position {
            get { return this.position; }
            set { this.position = value; }
        }

        /// <summary>
        /// Accessor and mutator for this nucleotide's nucleobase
        /// </summary>
        public Nucleobases Nucleobase {
            get { return this.nucleobase; }
            set { this.nucleobase = value; }
        }
    }
}
