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

public class InstanceMemberSymbol extends HostSymbol{
final public String className;
final public String memberName;

public InstanceMemberSymbol(String name) {
    super(name);
    int lastDot = name.lastIndexOf('.');
    if(lastDot == 0)
        this.className = null;
    else
        this.className = name.substring(1,lastDot);
    this.memberName = name.substring(lastDot + 1);
}
}
