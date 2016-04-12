using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    /// <summary>
    /// The application entry point.
    /// </summary>
    class Program {

        /// <summary>
        /// The Main method for the application.
        /// </summary>
        /// <param name="args">A list of command line arguments</param>
        static void Main(string[] args) {
            World.genes = new List<Gene>();
            World.homologousGenes = new SortedDictionary<Tuple<int, int>, List<Tuple<int, int>>>();
            List<string[]> lines = IO.ReadLines(args[0]);
            foreach (string[] line in lines) {
                World.Events(line);
            }

            World.Homologous();
            IO.WriteLines(args[0] + ".fa", World.Genes());
            IO.WriteLines(args[0] + ".homologs", World.HomologousGenes());
        }
    }
}
