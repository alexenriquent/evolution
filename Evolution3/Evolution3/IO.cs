using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    // This class contains the I/O operations.
    public static class IO {

        // Reads the input file and adds each line to a list
        // of strings.
        public static List<string[]> ReadLines(string filename) {
            List<string[]> splitedLines = new List<string[]>();
            string[] lines = System.IO.File.ReadAllLines(filename);
            foreach (string line in lines) {
                splitedLines.Add(line.Split(','));
            }
            return splitedLines;
        }
    
        // Writes a list to file.
        public static void WriteLines(string filename, List<string> strList) {
            string results = "";
            foreach (string line in strList) {
                results += line + "\n";
            }
            System.IO.File.WriteAllText(filename, results);
        }
    }
}
