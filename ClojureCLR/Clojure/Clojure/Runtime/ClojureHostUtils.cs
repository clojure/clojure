using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Scripting.Hosting;

namespace clojure.runtime
{
    /// <summary>
    /// Provides helpers for interacting with ClojureCRL, especially DLR hosting.
    /// </summary>
    public class ClojureHostUtils
    {


        public static LanguageSetup/*!*/ CreateLanguageSetup(IDictionary<string, object> options)
        {
            var setup = new LanguageSetup(
                typeof(ClojureContext).AssemblyQualifiedName,
                ClojureContext.ClojureDisplayName,
                ClojureContext.ClojureNames.Split(';'),
                ClojureContext.ClojureFileExtensions.Split(';')
            );

            if (options != null)
            {
                foreach (var entry in options)
                {
                    setup.Options.Add(entry.Key, entry.Value);
                }
            }

            return setup;
        }

    }
}
