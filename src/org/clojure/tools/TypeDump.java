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

import java.io.IOException;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * Creates an sexpr dump of type info
 */
public class TypeDump implements ClassVisitor{

PrintStream p;
boolean ignore;
static List packages;

public TypeDump(PrintStream p)
	{
	this.p = p;
	}

static public void main(String args[])
	{
	if(args.length < 2)
		{
		System.err.println("Usage: java org.clojure.tools.TypeDump jarfile package [package ...]");
		return;
		}
	packages = Arrays.asList(args).subList(1, args.length);
	try
		{
		TypeDump v = new TypeDump(System.out);
		ZipFile f = new ZipFile(args[0]);
		Enumeration en = f.entries();
		System.out.println('(');
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
		System.out.println(')');
		}
	catch(IOException e)
		{
		e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
	}

public void visit(int version, int access, String name, String signature, String superName, String[] interfaces)
	{
	String pkg = name.substring(0,name.lastIndexOf('/')).replace('/','.');
	if((access & Opcodes.ACC_PUBLIC) == 0
			|| !packages.contains(pkg))
		{
		ignore = true;
		return;
		}
	else
		ignore = false;
	p.print('(');
	p.println("(:name \"" + name + "\")");
	p.print(" (:super \"" + superName + "\")");
	if(interfaces.length > 0)
		{
		p.println();
		p.print(" (:interfaces");
		for(int i = 0; i < interfaces.length; i++)
			{
			String anInterface = interfaces[i];
			p.print(" \"" + anInterface + "\"");
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
	if(!ignore && (access & Opcodes.ACC_PUBLIC) != 0)
		{
		p.println();
		p.print(" (:field (:name \"");
		p.print(name);
		p.print("\") (:type \"");
		p.print(internalName(Type.getType(desc)));
		p.print("\")");
		if((access & Opcodes.ACC_STATIC) != 0)
			{
			p.print(" (:static t)");
			if(value != null && (access & Opcodes.ACC_FINAL) != 0)
				{
				p.print(" (:const-value ");
				if(value instanceof String)
					p.print('"');
				p.print(value);
				if(value instanceof String)
					p.print('"');
				p.print(")");
				}
			}
		p.print(")");
		}
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
	if(!ignore && (access & Opcodes.ACC_PUBLIC) != 0)
		{
		Type args[] = Type.getArgumentTypes(desc);

		p.println();
		if(name.equals("<init>"))
			p.print(" (:ctor ");
		else
			p.print(" (:method (:name \"" + name + "\") (:ret \"" +
		        internalName(Type.getReturnType(desc)) + "\")");
		p.print("(:arity " + args.length + ")");
		if((access & Opcodes.ACC_STATIC) != 0)
			{
			p.print("  (:static t)");
			}
		if((access & Opcodes.ACC_VARARGS) != 0)
			{
			p.print("  (:varargs t)");
			}
		p.println();
		p.print("  (:desc \"" + desc + "\")");
		if(args.length > 0)
			{
			p.println();
			p.print("  (:args");
			for(int i = 0; i < args.length; i++)
				{
				Type arg = args[i];
				p.print(" \"" + internalName(arg) + "\"");
				}
			p.print(')');
			}
		p.print(')');
		}
	return null;
	}

public void visitEnd()
	{
	if(!ignore)
		p.println(')');
	}
}
