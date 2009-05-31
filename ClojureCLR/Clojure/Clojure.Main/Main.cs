using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using clojure.lang;

namespace Clojure
{
    public static class CljMain
    {
        private static readonly Symbol CLOJURE_MAIN = Symbol.intern("clojure.main");
        private static readonly Var REQUIRE = RT.var("clojure.core", "require");
        private static readonly Var LEGACY_REPL = RT.var("clojure.main", "legacy-repl");
        private static readonly Var LEGACY_SCRIPT = RT.var("clojure.main", "legacy-script");
        private static readonly Var MAIN = RT.var("clojure.main", "main");

        static void Main(string[] args)
        {
            REQUIRE.invoke(CLOJURE_MAIN);
            MAIN.applyTo(RT.seq(args));
        }

        static void legacy_repl(string[] args)
        {
            REQUIRE.invoke(CLOJURE_MAIN);
            LEGACY_REPL.invoke(RT.seq(args));

        }

        static void legacy_script(string[] args)
        {
            REQUIRE.invoke(CLOJURE_MAIN);
            LEGACY_SCRIPT.invoke(RT.seq(args));
        }


    }
}
