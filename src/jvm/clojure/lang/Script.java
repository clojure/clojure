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
import java.io.PrintWriter;
import java.io.IOException;
import java.util.List;
import java.util.Arrays;

/**
 * <code>Script</code> provides a way to run one or more Clojure files
 * from a command line.  Example usage:
 * <p>
 * <pre>java -cp clojure.jar script1.clj @/dir/script2.clj -- [arguments]</pre>
 * </p>
 * <p>
 * The example above will:
 * <ol>
 * <li>bind *command-line-args* to a seq containing the (optional) arguments provided
 *     after the two dashes (--); this provides a way to provide command-line arguments
 *     to your scripts</li>
 * <li>load the Clojure file <i>at the filesystem path</i> <code>script1.clj</code></li>
 * <li>load the Clojure file with the name <code>dir/script2.clj</code> <i>from the
 *     current Java classpath</i>.  Files to be loaded from the classpath must be prefixed
 *     with a '@' character, and must be an "absolute path" to the classpath resource.
 *     Note that the "path" will be treated as absolute within the classpath, whether it is
 *     prefixed with a slash or not.</li>
 * </ol>
 * </p>
 * <p>
 * Any number of Clojure files can be provided as path arguments; these
 * files are loaded in order, as if via <code>load-file</code>.  Filesystem and classpath
 * paths may be provided in any order, and be intermixed as necessary.
 * </p>
 * <p>
 * Once the final script path has been loaded, the java process exits.
 * </p>
 */
public class Script{

public static void main(String[] args) throws Exception{
    try
		{
		for(String file : RT.processCommandLine(args))
            {
            if (file.startsWith("@"))
                {
                // trim leading slash if it's there -- loadResourceScript prepends its
                // own slash to every name it's given
                RT.loadResourceScript(file.substring(file.startsWith("@/") ? 2 : 1));
                }
            else
                {
                Compiler.loadFile(file);
                }
            }
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
			e.printStackTrace((PrintWriter)RT.ERR.get());
			}
		}
}
}

