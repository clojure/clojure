/***
 * ASM: a very small and fast Java bytecode manipulation framework
 * Copyright (c) 2000-2005 INRIA, France Telecom
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */
package clojure.asm.commons;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import clojure.asm.Label;
import clojure.asm.MethodAdapter;
import clojure.asm.MethodVisitor;
import clojure.asm.Opcodes;
import clojure.asm.Type;

/**
 * A {@link MethodAdapter} that keeps track of stack map frame changes between
 * {@link #visitFrame(int,int,Object[],int,Object[]) visitFrame} calls. This
 * adapter must be used with the
 * {@link clojure.asm.ClassReader#EXPAND_FRAMES} option. Each visit<i>XXX</i>
 * instruction delegates to the next visitor in the chain, if any, and then
 * simulates the effect of this instruction on the stack map frame, represented
 * by {@link #locals} and {@link #stack}. The next visitor in the chain can get
 * the state of the stack map frame <i>before</i> each instruction by reading
 * the value of these fields in its visit<i>XXX</i> methods (this requires a
 * reference to the AnalyzerAdapter that is before it in the chain).
 *
 * @author Eric Bruneton
 */
public class AnalyzerAdapter extends MethodAdapter{

/**
 * <code>List</code> of the local variable slots for current execution
 * frame. Primitive types are represented by {@link Opcodes#TOP},
 * {@link Opcodes#INTEGER}, {@link Opcodes#FLOAT}, {@link Opcodes#LONG},
 * {@link Opcodes#DOUBLE},{@link Opcodes#NULL} or
 * {@link Opcodes#UNINITIALIZED_THIS} (long and double are represented by a
 * two elements, the second one being TOP). Reference types are represented
 * by String objects (representing internal names, or type descriptors for
 * array types), and uninitialized types by Label objects (this label
 * designates the NEW instruction that created this uninitialized value).
 * This field is <tt>null</tt> for unreacheable instructions.
 */
public List locals;

/**
 * <code>List</code> of the operand stack slots for current execution
 * frame. Primitive types are represented by {@link Opcodes#TOP},
 * {@link Opcodes#INTEGER}, {@link Opcodes#FLOAT}, {@link Opcodes#LONG},
 * {@link Opcodes#DOUBLE},{@link Opcodes#NULL} or
 * {@link Opcodes#UNINITIALIZED_THIS} (long and double are represented by a
 * two elements, the second one being TOP). Reference types are represented
 * by String objects (representing internal names, or type descriptors for
 * array types), and uninitialized types by Label objects (this label
 * designates the NEW instruction that created this uninitialized value).
 * This field is <tt>null</tt> for unreacheable instructions.
 */
public List stack;

/**
 * The labels that designate the next instruction to be visited. May be
 * <tt>null</tt>.
 */
private List labels;

/**
 * Information about uninitialized types in the current execution frame.
 * This map associates internal names to Label objects. Each label
 * designates a NEW instruction that created the currently uninitialized
 * types, and the associated internal name represents the NEW operand, i.e.
 * the final, initialized type value.
 */
private Map uninitializedTypes;

/**
 * The maximum stack size of this method.
 */
private int maxStack;

/**
 * The maximum number of local variables of this method.
 */
private int maxLocals;

/**
 * Creates a new {@link AnalyzerAdapter}.
 *
 * @param owner  the owner's class name.
 * @param access the method's access flags (see {@link Opcodes}).
 * @param name   the method's name.
 * @param desc   the method's descriptor (see {@link Type Type}).
 * @param mv     the method visitor to which this adapter delegates calls. May
 *               be <tt>null</tt>.
 */
public AnalyzerAdapter(
		final String owner,
		final int access,
		final String name,
		final String desc,
		final MethodVisitor mv){
	super(mv);
	locals = new ArrayList();
	stack = new ArrayList();
	uninitializedTypes = new HashMap();

	if((access & Opcodes.ACC_STATIC) == 0)
		{
		if(name.equals("<init>"))
			{
			locals.add(Opcodes.UNINITIALIZED_THIS);
			}
		else
			{
			locals.add(owner);
			}
		}
	Type[] types = Type.getArgumentTypes(desc);
	for(int i = 0; i < types.length; ++i)
		{
		Type type = types[i];
		switch(type.getSort())
			{
			case Type.BOOLEAN:
			case Type.CHAR:
			case Type.BYTE:
			case Type.SHORT:
			case Type.INT:
				locals.add(Opcodes.INTEGER);
				break;
			case Type.FLOAT:
				locals.add(Opcodes.FLOAT);
				break;
			case Type.LONG:
				locals.add(Opcodes.LONG);
				locals.add(Opcodes.TOP);
				break;
			case Type.DOUBLE:
				locals.add(Opcodes.DOUBLE);
				locals.add(Opcodes.TOP);
				break;
			case Type.ARRAY:
				locals.add(types[i].getDescriptor());
				break;
				// case Type.OBJECT:
			default:
				locals.add(types[i].getInternalName());
			}
		}
}

public void visitFrame(
		final int type,
		final int nLocal,
		final Object[] local,
		final int nStack,
		final Object[] stack){
	if(type != Opcodes.F_NEW)
		{ // uncompressed frame
		throw new IllegalStateException("ClassReader.accept() should be called with EXPAND_FRAMES flag");
		}

	if(mv != null)
		{
		mv.visitFrame(type, nLocal, local, nStack, stack);
		}

	if(this.locals != null)
		{
		this.locals.clear();
		this.stack.clear();
		}
	else
		{
		this.locals = new ArrayList();
		this.stack = new ArrayList();
		}
	visitFrameTypes(nLocal, local, this.locals);
	visitFrameTypes(nStack, stack, this.stack);
	maxStack = Math.max(maxStack, this.stack.size());
}

private void visitFrameTypes(
		final int n,
		final Object[] types,
		final List result){
	for(int i = 0; i < n; ++i)
		{
		Object type = types[i];
		result.add(type);
		if(type == Opcodes.LONG || type == Opcodes.DOUBLE)
			{
			result.add(Opcodes.TOP);
			}
		}
}

public void visitInsn(final int opcode){
	if(mv != null)
		{
		mv.visitInsn(opcode);
		}
	execute(opcode, 0, null);
	if((opcode >= Opcodes.IRETURN && opcode <= Opcodes.RETURN)
	   || opcode == Opcodes.ATHROW)
		{
		this.locals = null;
		this.stack = null;
		}
}

public void visitIntInsn(final int opcode, final int operand){
	if(mv != null)
		{
		mv.visitIntInsn(opcode, operand);
		}
	execute(opcode, operand, null);
}

public void visitVarInsn(final int opcode, final int var){
	if(mv != null)
		{
		mv.visitVarInsn(opcode, var);
		}
	execute(opcode, var, null);
}

public void visitTypeInsn(final int opcode, final String desc){
	if(opcode == Opcodes.NEW)
		{
		if(labels == null)
			{
			Label l = new Label();
			labels = new ArrayList(3);
			labels.add(l);
			if(mv != null)
				{
				mv.visitLabel(l);
				}
			}
		for(int i = 0; i < labels.size(); ++i)
			{
			uninitializedTypes.put(labels.get(i), desc);
			}
		}
	if(mv != null)
		{
		mv.visitTypeInsn(opcode, desc);
		}
	execute(opcode, 0, desc);
}

public void visitFieldInsn(
		final int opcode,
		final String owner,
		final String name,
		final String desc){
	if(mv != null)
		{
		mv.visitFieldInsn(opcode, owner, name, desc);
		}
	execute(opcode, 0, desc);
}

public void visitMethodInsn(
		final int opcode,
		final String owner,
		final String name,
		final String desc){
	if(mv != null)
		{
		mv.visitMethodInsn(opcode, owner, name, desc);
		}
	pop(desc);
	if(opcode != Opcodes.INVOKESTATIC)
		{
		Object t = pop();
		if(opcode == Opcodes.INVOKESPECIAL && name.charAt(0) == '<')
			{
			Object u;
			if(t == Opcodes.UNINITIALIZED_THIS)
				{
				u = owner;
				}
			else
				{
				u = uninitializedTypes.get(t);
				}
			for(int i = 0; i < locals.size(); ++i)
				{
				if(locals.get(i) == t)
					{
					locals.set(i, u);
					}
				}
			for(int i = 0; i < stack.size(); ++i)
				{
				if(stack.get(i) == t)
					{
					stack.set(i, u);
					}
				}
			}
		}
	pushDesc(desc);
	labels = null;
}

public void visitJumpInsn(final int opcode, final Label label){
	if(mv != null)
		{
		mv.visitJumpInsn(opcode, label);
		}
	execute(opcode, 0, null);
	if(opcode == Opcodes.GOTO)
		{
		this.locals = null;
		this.stack = null;
		}
}

public void visitLabel(final Label label){
	if(mv != null)
		{
		mv.visitLabel(label);
		}
	if(labels == null)
		{
		labels = new ArrayList(3);
		}
	labels.add(label);
}

public void visitLdcInsn(final Object cst){
	if(mv != null)
		{
		mv.visitLdcInsn(cst);
		}
	if(cst instanceof Integer)
		{
		push(Opcodes.INTEGER);
		}
	else if(cst instanceof Long)
		{
		push(Opcodes.LONG);
		push(Opcodes.TOP);
		}
	else if(cst instanceof Float)
		{
		push(Opcodes.FLOAT);
		}
	else if(cst instanceof Double)
		{
		push(Opcodes.DOUBLE);
		push(Opcodes.TOP);
		}
	else if(cst instanceof String)
		{
		push("java/lang/String");
		}
	else if(cst instanceof Type)
		{
		push("java/lang/Class");
		}
	else
		{
		throw new IllegalArgumentException();
		}
	labels = null;
}

public void visitIincInsn(final int var, final int increment){
	if(mv != null)
		{
		mv.visitIincInsn(var, increment);
		}
	execute(Opcodes.IINC, var, null);
}

public void visitTableSwitchInsn(
		final int min,
		final int max,
		final Label dflt,
		final Label labels[]){
	if(mv != null)
		{
		mv.visitTableSwitchInsn(min, max, dflt, labels);
		}
	execute(Opcodes.TABLESWITCH, 0, null);
	this.locals = null;
	this.stack = null;
}

public void visitLookupSwitchInsn(
		final Label dflt,
		final int keys[],
		final Label labels[]){
	if(mv != null)
		{
		mv.visitLookupSwitchInsn(dflt, keys, labels);
		}
	execute(Opcodes.LOOKUPSWITCH, 0, null);
	this.locals = null;
	this.stack = null;
}

public void visitMultiANewArrayInsn(final String desc, final int dims){
	if(mv != null)
		{
		mv.visitMultiANewArrayInsn(desc, dims);
		}
	execute(Opcodes.MULTIANEWARRAY, dims, desc);
}

public void visitMaxs(final int maxStack, final int maxLocals){
	if(mv != null)
		{
		this.maxStack = Math.max(this.maxStack, maxStack);
		this.maxLocals = Math.max(this.maxLocals, maxLocals);
		mv.visitMaxs(this.maxStack, this.maxLocals);
		}
}

// ------------------------------------------------------------------------

private Object get(final int local){
	maxLocals = Math.max(maxLocals, local);
	return local < locals.size() ? locals.get(local) : Opcodes.TOP;
}

private void set(final int local, final Object type){
	maxLocals = Math.max(maxLocals, local);
	while(local >= locals.size())
		{
		locals.add(Opcodes.TOP);
		}
	locals.set(local, type);
}

private void push(final Object type){
	stack.add(type);
	maxStack = Math.max(maxStack, stack.size());
}

private void pushDesc(final String desc){
	int index = desc.charAt(0) == '(' ? desc.indexOf(')') + 1 : 0;
	switch(desc.charAt(index))
		{
		case'V':
			return;
		case'Z':
		case'C':
		case'B':
		case'S':
		case'I':
			push(Opcodes.INTEGER);
			return;
		case'F':
			push(Opcodes.FLOAT);
			return;
		case'J':
			push(Opcodes.LONG);
			push(Opcodes.TOP);
			return;
		case'D':
			push(Opcodes.DOUBLE);
			push(Opcodes.TOP);
			return;
		case'[':
			if(index == 0)
				{
				push(desc);
				}
			else
				{
				push(desc.substring(index, desc.length()));
				}
			break;
			// case 'L':
		default:
			if(index == 0)
				{
				push(desc.substring(1, desc.length() - 1));
				}
			else
				{
				push(desc.substring(index + 1, desc.length() - 1));
				}
			return;
		}
}

private Object pop(){
	return stack.remove(stack.size() - 1);
}

private void pop(final int n){
	int size = stack.size();
	int end = size - n;
	for(int i = size - 1; i >= end; --i)
		{
		stack.remove(i);
		}
}

private void pop(final String desc){
	char c = desc.charAt(0);
	if(c == '(')
		{
		int n = 0;
		Type[] types = Type.getArgumentTypes(desc);
		for(int i = 0; i < types.length; ++i)
			{
			n += types[i].getSize();
			}
		pop(n);
		}
	else if(c == 'J' || c == 'D')
		{
		pop(2);
		}
	else
		{
		pop(1);
		}
}

private void execute(final int opcode, final int iarg, final String sarg){
	if(this.locals == null)
		{
		return;
		}
	Object t1, t2, t3, t4;
	switch(opcode)
		{
		case Opcodes.NOP:
		case Opcodes.INEG:
		case Opcodes.LNEG:
		case Opcodes.FNEG:
		case Opcodes.DNEG:
		case Opcodes.I2B:
		case Opcodes.I2C:
		case Opcodes.I2S:
		case Opcodes.GOTO:
		case Opcodes.RETURN:
			break;
		case Opcodes.ACONST_NULL:
			push(Opcodes.NULL);
			break;
		case Opcodes.ICONST_M1:
		case Opcodes.ICONST_0:
		case Opcodes.ICONST_1:
		case Opcodes.ICONST_2:
		case Opcodes.ICONST_3:
		case Opcodes.ICONST_4:
		case Opcodes.ICONST_5:
		case Opcodes.BIPUSH:
		case Opcodes.SIPUSH:
			push(Opcodes.INTEGER);
			break;
		case Opcodes.LCONST_0:
		case Opcodes.LCONST_1:
			push(Opcodes.LONG);
			push(Opcodes.TOP);
			break;
		case Opcodes.FCONST_0:
		case Opcodes.FCONST_1:
		case Opcodes.FCONST_2:
			push(Opcodes.FLOAT);
			break;
		case Opcodes.DCONST_0:
		case Opcodes.DCONST_1:
			push(Opcodes.DOUBLE);
			push(Opcodes.TOP);
			break;
		case Opcodes.ILOAD:
		case Opcodes.FLOAD:
		case Opcodes.ALOAD:
			push(get(iarg));
			break;
		case Opcodes.LLOAD:
		case Opcodes.DLOAD:
			push(get(iarg));
			push(Opcodes.TOP);
			break;
		case Opcodes.IALOAD:
		case Opcodes.BALOAD:
		case Opcodes.CALOAD:
		case Opcodes.SALOAD:
			pop(2);
			push(Opcodes.INTEGER);
			break;
		case Opcodes.LALOAD:
		case Opcodes.D2L:
			pop(2);
			push(Opcodes.LONG);
			push(Opcodes.TOP);
			break;
		case Opcodes.FALOAD:
			pop(2);
			push(Opcodes.FLOAT);
			break;
		case Opcodes.DALOAD:
		case Opcodes.L2D:
			pop(2);
			push(Opcodes.DOUBLE);
			push(Opcodes.TOP);
			break;
		case Opcodes.AALOAD:
			pop(1);
			t1 = pop();
			pushDesc(((String) t1).substring(1));
			break;
		case Opcodes.ISTORE:
		case Opcodes.FSTORE:
		case Opcodes.ASTORE:
			t1 = pop();
			set(iarg, t1);
			if(iarg > 0)
				{
				t2 = get(iarg - 1);
				if(t2 == Opcodes.LONG || t2 == Opcodes.DOUBLE)
					{
					set(iarg - 1, Opcodes.TOP);
					}
				}
			break;
		case Opcodes.LSTORE:
		case Opcodes.DSTORE:
			pop(1);
			t1 = pop();
			set(iarg, t1);
			set(iarg + 1, Opcodes.TOP);
			if(iarg > 0)
				{
				t2 = get(iarg - 1);
				if(t2 == Opcodes.LONG || t2 == Opcodes.DOUBLE)
					{
					set(iarg - 1, Opcodes.TOP);
					}
				}
			break;
		case Opcodes.IASTORE:
		case Opcodes.BASTORE:
		case Opcodes.CASTORE:
		case Opcodes.SASTORE:
		case Opcodes.FASTORE:
		case Opcodes.AASTORE:
			pop(3);
			break;
		case Opcodes.LASTORE:
		case Opcodes.DASTORE:
			pop(4);
			break;
		case Opcodes.POP:
		case Opcodes.IFEQ:
		case Opcodes.IFNE:
		case Opcodes.IFLT:
		case Opcodes.IFGE:
		case Opcodes.IFGT:
		case Opcodes.IFLE:
		case Opcodes.IRETURN:
		case Opcodes.FRETURN:
		case Opcodes.ARETURN:
		case Opcodes.TABLESWITCH:
		case Opcodes.LOOKUPSWITCH:
		case Opcodes.ATHROW:
		case Opcodes.MONITORENTER:
		case Opcodes.MONITOREXIT:
		case Opcodes.IFNULL:
		case Opcodes.IFNONNULL:
			pop(1);
			break;
		case Opcodes.POP2:
		case Opcodes.IF_ICMPEQ:
		case Opcodes.IF_ICMPNE:
		case Opcodes.IF_ICMPLT:
		case Opcodes.IF_ICMPGE:
		case Opcodes.IF_ICMPGT:
		case Opcodes.IF_ICMPLE:
		case Opcodes.IF_ACMPEQ:
		case Opcodes.IF_ACMPNE:
		case Opcodes.LRETURN:
		case Opcodes.DRETURN:
			pop(2);
			break;
		case Opcodes.DUP:
			t1 = pop();
			push(t1);
			push(t1);
			break;
		case Opcodes.DUP_X1:
			t1 = pop();
			t2 = pop();
			push(t1);
			push(t2);
			push(t1);
			break;
		case Opcodes.DUP_X2:
			t1 = pop();
			t2 = pop();
			t3 = pop();
			push(t1);
			push(t3);
			push(t2);
			push(t1);
			break;
		case Opcodes.DUP2:
			t1 = pop();
			t2 = pop();
			push(t2);
			push(t1);
			push(t2);
			push(t1);
			break;
		case Opcodes.DUP2_X1:
			t1 = pop();
			t2 = pop();
			t3 = pop();
			push(t2);
			push(t1);
			push(t3);
			push(t2);
			push(t1);
			break;
		case Opcodes.DUP2_X2:
			t1 = pop();
			t2 = pop();
			t3 = pop();
			t4 = pop();
			push(t2);
			push(t1);
			push(t4);
			push(t3);
			push(t2);
			push(t1);
			break;
		case Opcodes.SWAP:
			t1 = pop();
			t2 = pop();
			push(t1);
			push(t2);
			break;
		case Opcodes.IADD:
		case Opcodes.ISUB:
		case Opcodes.IMUL:
		case Opcodes.IDIV:
		case Opcodes.IREM:
		case Opcodes.IAND:
		case Opcodes.IOR:
		case Opcodes.IXOR:
		case Opcodes.ISHL:
		case Opcodes.ISHR:
		case Opcodes.IUSHR:
		case Opcodes.L2I:
		case Opcodes.D2I:
		case Opcodes.FCMPL:
		case Opcodes.FCMPG:
			pop(2);
			push(Opcodes.INTEGER);
			break;
		case Opcodes.LADD:
		case Opcodes.LSUB:
		case Opcodes.LMUL:
		case Opcodes.LDIV:
		case Opcodes.LREM:
		case Opcodes.LAND:
		case Opcodes.LOR:
		case Opcodes.LXOR:
			pop(4);
			push(Opcodes.LONG);
			push(Opcodes.TOP);
			break;
		case Opcodes.FADD:
		case Opcodes.FSUB:
		case Opcodes.FMUL:
		case Opcodes.FDIV:
		case Opcodes.FREM:
		case Opcodes.L2F:
		case Opcodes.D2F:
			pop(2);
			push(Opcodes.FLOAT);
			break;
		case Opcodes.DADD:
		case Opcodes.DSUB:
		case Opcodes.DMUL:
		case Opcodes.DDIV:
		case Opcodes.DREM:
			pop(4);
			push(Opcodes.DOUBLE);
			push(Opcodes.TOP);
			break;
		case Opcodes.LSHL:
		case Opcodes.LSHR:
		case Opcodes.LUSHR:
			pop(3);
			push(Opcodes.LONG);
			push(Opcodes.TOP);
			break;
		case Opcodes.IINC:
			set(iarg, Opcodes.INTEGER);
			break;
		case Opcodes.I2L:
		case Opcodes.F2L:
			pop(1);
			push(Opcodes.LONG);
			push(Opcodes.TOP);
			break;
		case Opcodes.I2F:
			pop(1);
			push(Opcodes.FLOAT);
			break;
		case Opcodes.I2D:
		case Opcodes.F2D:
			pop(1);
			push(Opcodes.DOUBLE);
			push(Opcodes.TOP);
			break;
		case Opcodes.F2I:
		case Opcodes.ARRAYLENGTH:
		case Opcodes.INSTANCEOF:
			pop(1);
			push(Opcodes.INTEGER);
			break;
		case Opcodes.LCMP:
		case Opcodes.DCMPL:
		case Opcodes.DCMPG:
			pop(4);
			push(Opcodes.INTEGER);
			break;
		case Opcodes.JSR:
		case Opcodes.RET:
			throw new RuntimeException("JSR/RET are not supported");
		case Opcodes.GETSTATIC:
			pushDesc(sarg);
			break;
		case Opcodes.PUTSTATIC:
			pop(sarg);
			break;
		case Opcodes.GETFIELD:
			pop(1);
			pushDesc(sarg);
			break;
		case Opcodes.PUTFIELD:
			pop(sarg);
			pop();
			break;
		case Opcodes.NEW:
			push(labels.get(0));
			break;
		case Opcodes.NEWARRAY:
			pop();
			switch(iarg)
				{
				case Opcodes.T_BOOLEAN:
					pushDesc("[Z");
					break;
				case Opcodes.T_CHAR:
					pushDesc("[C");
					break;
				case Opcodes.T_BYTE:
					pushDesc("[B");
					break;
				case Opcodes.T_SHORT:
					pushDesc("[S");
					break;
				case Opcodes.T_INT:
					pushDesc("[I");
					break;
				case Opcodes.T_FLOAT:
					pushDesc("[F");
					break;
				case Opcodes.T_DOUBLE:
					pushDesc("[D");
					break;
					// case Opcodes.T_LONG:
				default:
					pushDesc("[J");
					break;
				}
			break;
		case Opcodes.ANEWARRAY:
			pop();
			if(sarg.charAt(0) == '[')
				{
				pushDesc("[" + sarg);
				}
			else
				{
				pushDesc("[L" + sarg + ";");
				}
			break;
		case Opcodes.CHECKCAST:
			pop();
			if(sarg.charAt(0) == '[')
				{
				pushDesc(sarg);
				}
			else
				{
				push(sarg);
				}
			break;
			// case Opcodes.MULTIANEWARRAY:
		default:
			pop(iarg);
			pushDesc(sarg);
			break;
		}
	labels = null;
}
}
