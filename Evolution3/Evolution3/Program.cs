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
        /// The Main method for the application
        /// </summary>
        /// <param name="args">A list of command line arguments</param>
        static void Main(string[] args) {
            List<string[]> lines = IO.ReadLines("Tests/test10");
            foreach (string[] line in lines) {
                World.Events(line);
            }
            IO.WriteLines("Results/test10.fa", World.Genes());
        }
    }
}
