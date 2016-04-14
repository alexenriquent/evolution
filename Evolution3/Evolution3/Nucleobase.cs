using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    // A set of nucleobases, including A (Adenine),
    // C (Cytosine), G (Guanine) and T (Thymine).
    // A nucleobase is not just any character, it has
    // to be from a limited set.
    public enum Nucleobases { A, C, G, T }

    // This class contains the nucleobase-related operations.
    public static class Nucleobase {

        // Converts the input string to a list of nucleobases
        public static List<Nucleobases> ToDNA(string dna) {
            List<Nucleobases> dnaList = new List<Nucleobases>();
            foreach (char nucleobase in dna) {
                switch (nucleobase) {
                    case 'A': dnaList.Add(Nucleobases.A); break;
                    case 'C': dnaList.Add(Nucleobases.C); break;
                    case 'T': dnaList.Add(Nucleobases.T); break;
                    case 'G': dnaList.Add(Nucleobases.G); break;
                }
            }
            return dnaList;
        }
    }
}
