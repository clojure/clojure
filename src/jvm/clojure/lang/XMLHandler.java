/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Dec 17, 2007 */

package clojure.lang;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

public class XMLHandler extends DefaultHandler{
ContentHandler h;


public XMLHandler(ContentHandler h){
	this.h = h;
}

public void setDocumentLocator(Locator locator){
	h.setDocumentLocator(locator);
}

public void startDocument() throws SAXException{
	h.startDocument();
}

public void endDocument() throws SAXException{
	h.endDocument();
}

public void startPrefixMapping(String prefix, String uri) throws SAXException{
	h.startPrefixMapping(prefix, uri);
}

public void endPrefixMapping(String prefix) throws SAXException{
	h.endPrefixMapping(prefix);
}

public void startElement(String uri, String localName, String qName, Attributes atts) throws SAXException{
	h.startElement(uri, localName, qName, atts);
}

public void endElement(String uri, String localName, String qName) throws SAXException{
	h.endElement(uri, localName, qName);
}

public void characters(char ch[], int start, int length) throws SAXException{
	h.characters(ch, start, length);
}

public void ignorableWhitespace(char ch[], int start, int length) throws SAXException{
	h.ignorableWhitespace(ch, start, length);
}

public void processingInstruction(String target, String data) throws SAXException{
	h.processingInstruction(target, data);
}

public void skippedEntity(String name) throws SAXException{
	h.skippedEntity(name);
}

/*
public static void main(String[] args){
	try
		{
		ContentHandler dummy = new DefaultHandler();
		SAXParserFactory f =  SAXParserFactory.newInstance();
		//f.setNamespaceAware(true);
		SAXParser p = f.newSAXParser();
		p.parse("http://arstechnica.com/journals.rssx",new XMLHandler(dummy));
		}
	catch(Exception e)
		{
		e.printStackTrace();
		}
}
//*/
}
