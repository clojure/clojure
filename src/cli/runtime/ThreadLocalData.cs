/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 25, 2006 11:45:22 AM */

using System;
using System.Collections.Specialized;

namespace clojure.lang
{
public class ThreadLocalData{

[ThreadStatic]
private static Transaction transaction;
[ThreadStatic]
private static Object[] values;

static public Object[] getValues(){
     return values;
}

static public void setValues(Object[] vals) {
    values = vals;
}

static public Transaction getTransaction() {
    return transaction;
}

static public void setTransaction(Transaction t){
    transaction = t;
}


}
}
