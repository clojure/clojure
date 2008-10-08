/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Oct 18, 2007 */

package clojure.lang;

import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

public class Repl{
static final Symbol USER = Symbol.create("user");
static final Symbol CLOJURE = Symbol.create("clojure");

static final Var in_ns = RT.var("clojure", "in-ns");
static final Var refer = RT.var("clojure", "refer");
static final Var ns = RT.var("clojure", "*ns*");
static final Var warn_on_reflection = RT.var("clojure", "*warn-on-reflection*");
static final Var star1 = RT.var("clojure", "*1");
static final Var star2 = RT.var("clojure", "*2");
static final Var star3 = RT.var("clojure", "*3");
static final Var stare = RT.var("clojure", "*e");

public static void main(String[] args) throws Exception{

//	RT.init();

	try
		{
		//*ns* must be thread-bound for in-ns to work
		//thread-bind *warn-on-reflection* so it can be set!
		//thread-bind *1,*2,*3,*e so each repl has its own history
		//must have corresponding popThreadBindings in finally clause
		Var.pushThreadBindings(
				RT.map(ns, ns.get(),
					   warn_on_reflection, warn_on_reflection.get(),
					   star1, null,
					   star2, null,
					   star3, null,
					   stare, null));

		//create and move into the user namespace
		in_ns.invoke(USER);
		refer.invoke(CLOJURE);

		//load any supplied files
		for(String file : RT.processCommandLine(args))
			try
				{
				Compiler.loadFile(file);
				}
			catch(Exception e)
				{
				e.printStackTrace((PrintWriter)RT.ERR.get());
				}

		//repl IO support
		LineNumberingPushbackReader rdr = new LineNumberingPushbackReader(new InputStreamReader(System.in,RT.UTF8));
		OutputStreamWriter w = (OutputStreamWriter) RT.OUT.get();//new OutputStreamWriter(System.out);
		Object EOF = new Object();

		//start the loop
		w.write("Clojure\n");
		for(; ;)
			{
			try
				{
				w.write(Compiler.currentNS().name + "=> ");
				w.flush();
				Object r = LispReader.read(rdr, false, EOF, false);
				if(r == EOF)
					{
					w.write("\n");
					w.flush();
					break;
					}
				Object ret = Compiler.eval(r);
				RT.print(ret, w);
				w.write('\n');
				//w.flush();
				star3.set(star2.get());
				star2.set(star1.get());
				star1.set(ret);
				}
			catch(Throwable e)
				{
				Throwable c = e;
				while(c.getCause() != null)
					c = c.getCause();
				((PrintWriter)RT.ERR.get()).println(e instanceof Compiler.CompilerException ? e : c);
				stare.set(e);
				}
			}
		}
	catch(Exception e)
		{
		e.printStackTrace((PrintWriter)RT.ERR.get());
		}
	finally
		{
		Var.popThreadBindings();
		}
	System.exit(0);
}

}
