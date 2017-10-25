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

public class ReaderConditional implements ILookup {

public static final Keyword FORM_KW = Keyword.intern("form");
public static final Keyword SPLICING_KW = Keyword.intern("splicing?");

public final Object form;
public final Boolean splicing;

public static ReaderConditional create(Object form, boolean splicing) {
	return new ReaderConditional(form, splicing);
}

private ReaderConditional(Object form, boolean splicing){
	this.form = form;
	this.splicing = splicing;
}


public Object valAt(Object key) {
	return valAt(key, null);
}

public Object valAt(Object key, Object notFound) {
	if (FORM_KW.equals(key)) {
		return this.form;
	} else if (SPLICING_KW.equals(key)) {
		return this.splicing;
	} else {
		return notFound;
	}
}


@Override
public boolean equals(Object o) {
	if (this == o) return true;
	if (o == null || getClass() != o.getClass()) return false;

	ReaderConditional that = (ReaderConditional) o;

	if (form != null ? !form.equals(that.form) : that.form != null) return false;
	if (splicing != null ? !splicing.equals(that.splicing) : that.splicing != null)
		return false;
	return true;
}

@Override
public int hashCode() {
	int result = Util.hash(form);
	result = 31 * result + Util.hash(splicing);
	return result;
}

}
