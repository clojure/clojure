/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

public class TaggedLiteral implements ILookup {

public static final Keyword TAG_KW = Keyword.intern("tag");
public static final Keyword FORM_KW = Keyword.intern("form");

public final Symbol tag;
public final Object form;

public static TaggedLiteral create(Symbol tag, Object form) {
	return new TaggedLiteral(tag, form);
}

private TaggedLiteral(Symbol tag, Object form){
	this.tag = tag;
	this.form = form;
}

public Object valAt(Object key) {
	return valAt(key, null);
}

public Object valAt(Object key, Object notFound) {
	if (FORM_KW.equals(key)) {
		return this.form;
	} else if (TAG_KW.equals(key)) {
		return this.tag;
	} else {
		return notFound;
	}
}

@Override
public boolean equals(Object o) {
	if (this == o) return true;
	if (o == null || getClass() != o.getClass()) return false;

	TaggedLiteral that = (TaggedLiteral) o;

	if (form != null ? !form.equals(that.form) : that.form != null) return false;
	if (tag != null ? !tag.equals(that.tag) : that.tag != null) return false;

	return true;
}

@Override
public int hashCode() {
	int result = Util.hash(tag);
	result = 31 * result + Util.hash(form);
	return result;
}

}
