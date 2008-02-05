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

public class Repl{
static final Symbol REFER = Symbol.create("clojure", "refer");
static final Symbol QUOTE = Symbol.create("quote");
static final Symbol CLOJURE = Symbol.create("clojure");
public static void main(String[] args) throws Exception{
	RT.init();
	for(String file : args)
		try
			{
			Compiler.loadFile(file);
			}
		catch(Exception e)
			{
			e.printStackTrace();
			}

	//repl
	LineNumberingPushbackReader rdr = new LineNumberingPushbackReader(new InputStreamReader(System.in));
	OutputStreamWriter w = (OutputStreamWriter) RT.OUT.get();//new OutputStreamWriter(System.out);

	Object EOF = new Object();
	try
		{
		Var.pushThreadBindings(
				RT.map(
//						RT.NS_REFERS, RT.NS_REFERS.get(),
//				       RT.NS_IMPORTS, RT.NS_IMPORTS.get(),
				       RT.CURRENT_NS, RT.CURRENT_NS.get(),
				       RT.WARN_ON_REFLECTION, RT.WARN_ON_REFLECTION.get(),
				       Compiler.SOURCE, "REPL"
				));
		w.write("Clojure\n");
		RT.inNamespace.invoke(Symbol.create("user"));
		Compiler.eval(RT.list(REFER, RT.list(QUOTE, CLOJURE)));
		for(; ;)
			{
			try
				{
				Var.pushThreadBindings(
						RT.map(Compiler.LOADER, new DynamicClassLoader()));
				w.write(Compiler.currentNS().name + "=> ");
				w.flush();
				Object r = LispReader.read(rdr, false, EOF, false);
				if(r == EOF)
					break;
				Object ret = Compiler.eval(r);
				RT.print(ret, w);
				w.write('\n');
				//w.flush();
				}
			catch(Throwable e)
				{
				e.printStackTrace();
				}
			finally
				{
				Var.popThreadBindings();
				}
			}
		}
	catch(Exception e)
		{
		e.printStackTrace();
		}
	finally
		{
		Var.popThreadBindings();
		}
}

}
