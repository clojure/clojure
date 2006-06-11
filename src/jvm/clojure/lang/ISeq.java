package clojure.lang;

/**
 * Copyright (c) Rich Hickey. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 * which can be found in the file CPL.TXT at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by
 * the terms of this license.
 * You must not remove this notice, or any other, from this software.
 */

/**
 * A persistent, functional, sequence interface
 * 
 * ISeqs are immutable values, i.e. neither first(), nor rest() changes
 * or invalidates the ISeq
 */
public interface ISeq {

Object first() throws Exception;

ISeq rest() throws Exception;
}
