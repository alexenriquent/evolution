using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    // The application entry point.
    class Program {

        // The Main method for the application.
        static void Main(string[] args) {
            World.genes = new List<Gene>();
            World.homologousGenes = new SortedDictionary<Tuple<int, int>, List<Tuple<int, int>>>();
            string filename = args[0];
            List<string[]> events = IO.ReadLines(filename);

            foreach (string[] evolutionaryEvent in events) {
                World.Events(evolutionaryEvent);
            }

            World.Homologous();
            IO.WriteLines(filename + ".fa", World.Genes());
            IO.WriteLines(filename + ".homologs", World.HomologousGenes());
        }
    }
}
