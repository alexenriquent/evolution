using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Evolution3 {

    class IO {

        public static string[] ReadLines(string filename) {
            string[] lines = System.IO.File.ReadAllLines(filename);
            return lines; 
        }
    }
}
