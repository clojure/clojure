/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package lava;

import lava.lang.Symbol;
import lava.lang.Var;
import lava.lang.RT;

public class main{

final static private Symbol LAVA_MAIN = Symbol.intern("lava.main");
final static private Var REQUIRE = RT.var("lava.core", "require");
final static private Var LEGACY_REPL = RT.var("lava.main", "legacy-repl");
final static private Var LEGACY_SCRIPT = RT.var("lava.main", "legacy-script");
final static private Var MAIN = RT.var("lava.main", "main");

public static void legacy_repl(String[] args) {
    REQUIRE.invoke(LAVA_MAIN);
    LEGACY_REPL.invoke(RT.seq(args));
}

public static void legacy_script(String[] args) {
    REQUIRE.invoke(LAVA_MAIN);
    LEGACY_SCRIPT.invoke(RT.seq(args));
}

public static void main(String[] args) {
    REQUIRE.invoke(LAVA_MAIN);
    MAIN.applyTo(RT.seq(args));
}
}
