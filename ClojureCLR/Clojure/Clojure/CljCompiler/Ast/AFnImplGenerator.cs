/**
 *   Copyright (c) David Miller. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection.Emit;
using System.Reflection;

namespace clojure.lang.CljCompiler.Ast
{
    class AFnImplGenerator
    {

        internal static Type Create(GenContext context, Type baseClass)
        {
            ModuleBuilder mb = context.ModuleBldr;
            string name = baseClass.Name + "_impl";
            TypeBuilder baseTB = context.ModuleBldr.DefineType(name, TypeAttributes.Class | TypeAttributes.Public, baseClass);

            baseTB.DefineDefaultConstructor(MethodAttributes.Public);

            for (int i = 0; i < 20; i++ )
                DefineDelegateFieldAndOverride(baseTB, i);

            return baseTB.CreateType();
        }

        static Type[] CreateObjectTypeArray(int size)
        {
            Type[] typeArray = new Type[size];
            for (int i = 0; i < size; i++)
                typeArray[i] = typeof(Object);
            return typeArray;
        }

        static MethodInfo Method_AFn_WrongArityException = typeof(AFn).GetMethod("WrongArityException2");
        static MethodInfo Method_Delegate_Invoke = typeof(Delegate).GetMethod("Invoke");

        static void DefineDelegateFieldAndOverride(TypeBuilder tb, int numArgs)
        {
            Type fieldType = FuncTypeHelpers.GetFFuncType(numArgs);
            string fieldName = "_fn" + numArgs;
            FieldBuilder fb = tb.DefineField(fieldName, fieldType, FieldAttributes.Public);

            MethodBuilder mb = tb.DefineMethod("invoke", MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.Virtual, typeof(object), CreateObjectTypeArray(numArgs));
            ILGenerator gen = mb.GetILGenerator();

            Label eqLabel = gen.DefineLabel();

            //  this._fni == null ?
            gen.Emit(OpCodes.Ldarg_0);
            gen.Emit(OpCodes.Ldfld, fb);
            gen.Emit(OpCodes.Ldnull);
            gen.Emit(OpCodes.Beq, eqLabel);
            //Not equal to Null, invoke it.
            gen.Emit(OpCodes.Ldarg_0);
            gen.Emit(OpCodes.Ldfld, fb);
            for (int i = 0; i < numArgs; i++)
                gen.Emit(OpCodes.Ldarg, i+1);
            gen.Emit(OpCodes.Call,fb.FieldType.GetMethod("Invoke"));            
            
            gen.Emit(OpCodes.Ret);

            gen.MarkLabel(eqLabel);
            // Equal to Null: throw WrongArityException
            gen.Emit(OpCodes.Ldarg_0);
            gen.Emit(OpCodes.Call, Method_AFn_WrongArityException);
            gen.Emit(OpCodes.Throw);          
          }

    }
}
