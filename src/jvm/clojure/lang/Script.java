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

import java.io.OutputStreamWriter;
import java.io.IOException;
import java.util.List;
import java.util.Arrays;

public class Script{
public static void main(String[] args) throws Exception{

	try
		{
		for(String file : RT.processCommandLine(args))
			Compiler.loadFile(file);
		}
	finally
		{
		OutputStreamWriter w = (OutputStreamWriter) RT.OUT.get();
		try
			{
			w.flush();
			w.close();
			}
		catch(IOException e)
			{
			e.printStackTrace();
			}
		}
}
}

