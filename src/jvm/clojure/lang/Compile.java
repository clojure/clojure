/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
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
private static final Var compile_path = RT.var("clojure.core", "*compile-path*");
private static final Var compile = RT.var("clojure.core", "compile");

public static void main(String[] args) throws Exception{

	OutputStreamWriter out = (OutputStreamWriter) RT.OUT.get();
	PrintWriter err = (PrintWriter) RT.ERR.get();
	String path = System.getProperty(PATH_PROP);
	int count = args.length;

	if(path == null)
		{
		err.println("ERROR: Must set system property " + PATH_PROP +
		            "\nto the location for compiled .class files." +
		            "\nThis directory must also be on your CLASSPATH.");
		System.exit(1);
		}

	try
		{
		Var.pushThreadBindings(RT.map(compile_path, path));

		out.write("Compiling " + count + " " +
		          ((count == 1) ? "lib" : "libs") +
		          " to " + path);
		out.flush();

		for(String lib : args)
			compile.invoke(Symbol.intern(lib));

		Var.popThreadBindings();
		}
	finally
		{
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
