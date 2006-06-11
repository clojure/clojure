/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 25, 2006 3:54:03 PM */

package clojure.runtime;

public interface IFn{

public Object invoke() throws Exception;

public Object invoke(Object arg1) throws Exception;

public Object invoke(Object arg1, Object arg2) throws Exception;

public Object invoke(Object arg1, Object arg2, Object arg3) throws Exception;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) throws Exception;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)
		throws Exception;

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5,
                     ISeq args) throws Exception;

public Object applyTo(ISeq arglist) throws Exception;
}
