using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    public class Nucleotide {

        private string action;
        private Tuple<int, int> origin;
        private int position;
        private Nucleobases nucleobase;

        public Nucleotide(string evolutionEvent, Tuple<int, int> origin, int position, Nucleobases nucleobase) {
            this.action = evolutionEvent;
            this.origin = origin;
            this.position = position;
            this.nucleobase = nucleobase;
        }

        public string Action {
            get { return this.action; }
            set { this.action = value; }
        }

        public Tuple<int,int> Origin {
            get { return this.origin; }
            set { this.origin = value; }
        }

        public int Position {
            get { return this.position; }
            set { this.position = value; }
        }

        public Nucleobases Nucleobase {
            get { return this.nucleobase; }
            set { this.nucleobase = value; }
        }
    }
}
