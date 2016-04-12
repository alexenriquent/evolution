using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    /// <summary>
    /// This class contains the I/O operations.
    /// </summary>
    public static class IO {

        /// <summary>
        /// Reads the input file and adds each line to a list
        /// of strings.
        /// </summary>
        /// <param name="filename">A file path/filename</param>
        /// <returns>A list containing all lines from file</returns>
        public static List<string[]> ReadLines(string filename) {
            List<string[]> splitedLines = new List<string[]>();
            string[] lines = System.IO.File.ReadAllLines(filename);
            foreach (string line in lines) {
                splitedLines.Add(line.Split(','));
            }
            return splitedLines;
        }

        /// <summary>
        /// Writes a list to file.
        /// </summary>
        /// <param name="filename">A file path/filename</param>
        /// <param name="strList">A list of string</param>
        public static void WriteLines(string filename, List<string> strList) {
            string results = "";
            foreach (string line in strList) {
                results += line + "\n";
            }
            System.IO.File.WriteAllText(filename, results);
        }
    }
}
