using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    public static class IO {

        public static List<string[]> ReadLines(string filename) {
            List<string[]> splitedLines = new List<string[]>();
            string[] lines = System.IO.File.ReadAllLines(filename);
            foreach (string line in lines) {
                splitedLines.Add(line.Split(','));
            }
            return splitedLines;
        }

        public static void WriteLines(string filename, List<string> strList) {
            System.IO.File.WriteAllLines(filename, strList);
        }
    }
}
