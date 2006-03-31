/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 31, 2006 11:03:27 AM */

package org.clojure.tools;

import org.objectweb.asm.*;

import java.io.PrintStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.Enumeration;

/**
 * Creates an sexpr dump of type info
 */
public class TypeDump implements ClassVisitor{

PrintStream p;

public TypeDump(PrintStream p)
	{
	this.p = p;
	}

static public void main(String jarName[])
	{
	try
		{
		TypeDump v = new TypeDump(System.out);
		ZipFile f = new ZipFile(jarName[0]);
		Enumeration en = f.entries();
		while(en.hasMoreElements())
			{
			ZipEntry e = (ZipEntry) en.nextElement();
			String name = e.getName();
			if(name.endsWith(".class"))
				{
				ClassReader cr = new ClassReader(f.getInputStream(e));
				cr.accept(v, false);
				}
			}
		}
	catch(IOException e)
		{
		e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
	}

public void visit(int version, int access, String name, String signature, String superName, String[] interfaces)
	{
	p.print('(');
	p.println("(:name " + name + ")");
	p.print(" (:super " + superName + ")");
	if(interfaces.length > 0)
		{
		p.println();
		p.print(" (:interfaces");
		for(int i = 0; i < interfaces.length; i++)
			{
			String anInterface = interfaces[i];
			p.print(" " + anInterface);
			}
		p.print(')');
		}
	}

public void visitSource(String source, String debug)
	{
	}

public void visitOuterClass(String owner, String name, String desc)
	{
	}

public AnnotationVisitor visitAnnotation(String desc, boolean visible)
	{
	return null;
	}

public void visitAttribute(Attribute attribute)
	{
	}

public void visitInnerClass(String name, String outerName, String innerName, int access)
	{
	}

public FieldVisitor visitField(int access, String name, String desc, String signature, Object value)
	{
	return null;
	}

String internalName(Type t)
	{
	if(t.getSort() == Type.OBJECT)
		return t.getInternalName();
	return t.toString();
	}

public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions)
	{
	Type args[] = Type.getArgumentTypes(desc);

	p.println();
	p.print(" (:method (:name " + name + ") (:arity " + args.length + ") (:ret " + internalName(Type.getReturnType(desc)) + ")");
	p.println();
	p.println("  (:desc \"" + desc + "\")");
	p.print("  (:args");
	for(int i = 0; i < args.length; i++)
		{
		Type arg = args[i];
		p.print(" " + internalName(arg));
		}
	p.print(')');
	p.print(')');
	return null;
	}

public void visitEnd()
	{
	p.println(')');
	}
}
