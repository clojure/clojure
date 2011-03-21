/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/


package clojure.lang;

import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.IOException;

// Compiles libs and generates class files stored within the directory
// named by the Java System property "clojure.compile.path". Arguments are
// strings naming the libs to be compiled. The libs and compile-path must
// all be within CLASSPATH.

public class Compile{

private static final String PATH_PROP = "clojure.compile.path";
private static final String REFLECTION_WARNING_PROP = "clojure.compile.warn-on-reflection";
private static final String UNCHECKED_MATH_PROP = "clojure.compile.unchecked-math";
private static final Var compile_path = RT.var("clojure.core", "*compile-path*");
private static final Var compile = RT.var("clojure.core", "compile");
private static final Var warn_on_reflection = RT.var("clojure.core", "*warn-on-reflection*");
private static final Var unchecked_math = RT.var("clojure.core", "*unchecked-math*");

public static void main(String[] args) throws IOException{

	OutputStreamWriter out = (OutputStreamWriter) RT.OUT.deref();
	PrintWriter err = RT.errPrintWriter();
	String path = System.getProperty(PATH_PROP);
	int count = args.length;

	if(path == null)
		{
		err.println("ERROR: Must set system property " + PATH_PROP +
		            "\nto the location for compiled .class files." +
		            "\nThis directory must also be on your CLASSPATH.");
		System.exit(1);
		}

    boolean warnOnReflection = System.getProperty(REFLECTION_WARNING_PROP, "false").equals("true");
    boolean uncheckedMath = System.getProperty(UNCHECKED_MATH_PROP, "false").equals("true");

	try
		{
               Var.pushThreadBindings(RT.map(compile_path, path, warn_on_reflection, warnOnReflection, unchecked_math, uncheckedMath));

		for(String lib : args)
        {
            out.write("Compiling " + lib + " to " + path + "\n");
            out.flush();
            compile.invoke(Symbol.intern(lib));
        }
		}
	finally
		{
        Var.popThreadBindings();
		try
			{
			out.flush();
			out.close();
			}
		catch(IOException e)
			{
			e.printStackTrace(err);
			}
		}
}
}
