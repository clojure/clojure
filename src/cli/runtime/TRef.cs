/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich May 30, 2006 */

using System;
using System.Threading;

namespace org.clojure.runtime
{

public class TRef : TVal, IComparable{
static int nextSeq = 0;

readonly int lockSeq;

public TRef() {
	this.lockSeq = Interlocked.Increment(ref nextSeq);
}

public int CompareTo(Object o){
	return lockSeq - ((TRef) o).lockSeq;
}
}
}
