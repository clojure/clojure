/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;

namespace clojure.lang
{

public class InstanceMemberSymbol :HostSymbol {
readonly public String className;
readonly public String memberName;

public InstanceMemberSymbol(String name) : base(name) {
int lastDot = name.LastIndexOf('.');
if (lastDot == 0)
	this.className = null;
else
	this.className = name.Substring(1, lastDot-1);
this.memberName = name.Substring(lastDot + 1);
}
}

}
