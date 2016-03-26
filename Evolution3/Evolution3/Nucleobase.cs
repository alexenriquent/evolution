using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    /// <summary>
    /// A set of nucleobases, including A (Adenine),
    /// C (Cytosine), G (Guanine) and T (Thymine).
    /// A nucleobase is not just any character, it has
    /// to be from a limited set.
    /// </summary>
    public enum Nucleobases { A, C, G, T }

    /// <summary>
    /// This class contains the nucleobase-related operations.
    /// </summary>
    public static class Nucleobase {

        /// <summary>
        /// Converts the input string to a list of nucleobases.
        /// </summary>
        /// <param name="dna">A string containing nucleobases</param>
        /// <returns>A list of nucleobases</returns>
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
