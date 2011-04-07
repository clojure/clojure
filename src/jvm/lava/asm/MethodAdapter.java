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
package lava.asm;

/**
 * An empty {@link MethodVisitor} that delegates to another
 * {@link MethodVisitor}. This class can be used as a super class to quickly
 * implement usefull method adapter classes, just by overriding the necessary
 * methods.
 *
 * @author Eric Bruneton
 */
public class MethodAdapter implements MethodVisitor{

/**
 * The {@link MethodVisitor} to which this adapter delegates calls.
 */
protected MethodVisitor mv;

/**
 * Constructs a new {@link MethodAdapter} object.
 *
 * @param mv the code visitor to which this adapter must delegate calls.
 */
public MethodAdapter(final MethodVisitor mv){
	this.mv = mv;
}

public AnnotationVisitor visitAnnotationDefault(){
	return mv.visitAnnotationDefault();
}

public AnnotationVisitor visitAnnotation(
		final String desc,
		final boolean visible){
	return mv.visitAnnotation(desc, visible);
}

public AnnotationVisitor visitParameterAnnotation(
		final int parameter,
		final String desc,
		final boolean visible){
	return mv.visitParameterAnnotation(parameter, desc, visible);
}

public void visitAttribute(final Attribute attr){
	mv.visitAttribute(attr);
}

public void visitCode(){
	mv.visitCode();
}

public void visitFrame(
		final int type,
		final int nLocal,
		final Object[] local,
		final int nStack,
		final Object[] stack){
	mv.visitFrame(type, nLocal, local, nStack, stack);
}

public void visitInsn(final int opcode){
	mv.visitInsn(opcode);
}

public void visitIntInsn(final int opcode, final int operand){
	mv.visitIntInsn(opcode, operand);
}

public void visitVarInsn(final int opcode, final int var){
	mv.visitVarInsn(opcode, var);
}

public void visitTypeInsn(final int opcode, final String desc){
	mv.visitTypeInsn(opcode, desc);
}

public void visitFieldInsn(
		final int opcode,
		final String owner,
		final String name,
		final String desc){
	mv.visitFieldInsn(opcode, owner, name, desc);
}

public void visitMethodInsn(
		final int opcode,
		final String owner,
		final String name,
		final String desc){
	mv.visitMethodInsn(opcode, owner, name, desc);
}

public void visitJumpInsn(final int opcode, final Label label){
	mv.visitJumpInsn(opcode, label);
}

public void visitLabel(final Label label){
	mv.visitLabel(label);
}

public void visitLdcInsn(final Object cst){
	mv.visitLdcInsn(cst);
}

public void visitIincInsn(final int var, final int increment){
	mv.visitIincInsn(var, increment);
}

public void visitTableSwitchInsn(
		final int min,
		final int max,
		final Label dflt,
		final Label labels[]){
	mv.visitTableSwitchInsn(min, max, dflt, labels);
}

public void visitLookupSwitchInsn(
		final Label dflt,
		final int keys[],
		final Label labels[]){
	mv.visitLookupSwitchInsn(dflt, keys, labels);
}

public void visitMultiANewArrayInsn(final String desc, final int dims){
	mv.visitMultiANewArrayInsn(desc, dims);
}

public void visitTryCatchBlock(
		final Label start,
		final Label end,
		final Label handler,
		final String type){
	mv.visitTryCatchBlock(start, end, handler, type);
}

public void visitLocalVariable(
		final String name,
		final String desc,
		final String signature,
		final Label start,
		final Label end,
		final int index){
	mv.visitLocalVariable(name, desc, signature, start, end, index);
}

public void visitLineNumber(final int line, final Label start){
	mv.visitLineNumber(line, start);
}

public void visitMaxs(final int maxStack, final int maxLocals){
	mv.visitMaxs(maxStack, maxLocals);
}

public void visitEnd(){
	mv.visitEnd();
}
}
