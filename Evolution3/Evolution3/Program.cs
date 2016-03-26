using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    class Program {

        static void Main(string[] args) {
            List<string[]> lines = IO.ReadLines("Tests/test10");
            foreach (string[] line in lines) {
                World.Events(line);
            }
            IO.WriteLines("Results/test10.fa", World.Genomes());
        }
    }
}
