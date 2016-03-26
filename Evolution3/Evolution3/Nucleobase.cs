using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    public enum Nucleobases { A, C, G, T }

    public static class Nucleobase {

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
