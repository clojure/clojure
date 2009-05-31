using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace clojure.runtime
{
    /// <summary>
    /// Implements part of the functionaligy of java.util.Properties.
    /// </summary>
    public class Properties : Dictionary<string,string>
    {

        public string getProperty(string key)
        {
            string value = null;
            TryGetValue(key, out value);
            return value;
        }

        public void LoadFromString(string content)
        {
            using (TextReader rdr = new StringReader(content))
            {
                Load(rdr);
            }
        }

        public void Load(string fileName)
        {
            using ( TextReader rdr = File.OpenText(fileName) )
            {
                Load(rdr);
            }
        }

        public void Load(TextReader rdr)
        {
            Clear();

            string line;
            while ((line = rdr.ReadLine()) != null)
            {
                line = line.Trim();
                if (string.IsNullOrEmpty(line) ||
                    line.StartsWith(";") ||
                    line.StartsWith("#") ||
                    line.StartsWith("'") ||
                    !line.Contains("="))
                    continue;

                int index = line.IndexOf('=');
                string key = line.Substring(0, index).Trim();
                string value = line.Substring(index + 1).Trim();

                if ((value.StartsWith("\"") && value.EndsWith("\"")) ||
                    (value.StartsWith("'") && value.EndsWith("'")))
                {
                    value = value.Substring(1, value.Length - 2);
                }

                this[key] = value;
            }
        }
    }
}
