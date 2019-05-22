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

import static java.lang.invoke.MethodHandles.dropArguments;
import static java.lang.invoke.MethodHandles.exactInvoker;
import static java.lang.invoke.MethodHandles.filterArguments;
import static java.lang.invoke.MethodHandles.filterReturnValue;
import static java.lang.invoke.MethodHandles.foldArguments;
import static java.lang.invoke.MethodHandles.identity;
import static java.lang.invoke.MethodHandles.insertArguments;
import static java.lang.invoke.MethodType.methodType;

import java.lang.annotation.Annotation;
import java.lang.invoke.CallSite;
import java.lang.invoke.ConstantCallSite;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodHandles.Lookup;
import java.lang.invoke.MethodType;
import java.lang.invoke.MutableCallSite;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.List;

class RTMetaFactories{
	private static final Class<? extends Annotation> CALLER_SENSITIVE;
	private static final MethodHandle REFLECTOR_INVOKE_STATIC_METHOD;
	private static final MethodHandle REFLECTOR_INVOKE_CONSTRUCTOR;
	private static final MethodHandle REFLECTOR_INVOKE_INSTANCE_METHOD;
	private static final MethodHandle REFLECTOR_INVOKE_NO_ARG_INSTANCE_MEMBER;
	private static final MethodHandle REFLECTOR_SET_INSTANCE_FIELD;
	private static final MethodHandle BOX_ARG;
	private static final MethodHandle PREP_RET;
	static
		{
		CALLER_SENSITIVE = findCallerSensitiveClass();

		Lookup lookup = MethodHandles.lookup();
		try
			{
			REFLECTOR_INVOKE_STATIC_METHOD = lookup.findStatic(Reflector.class, "invokeStaticMethod",
					methodType(Object.class, Class.class, String.class, Object[].class));
			REFLECTOR_INVOKE_CONSTRUCTOR = lookup.findStatic(Reflector.class, "invokeConstructor",
					methodType(Object.class, Class.class, Object[].class));
			REFLECTOR_INVOKE_INSTANCE_METHOD = lookup.findStatic(Reflector.class, "invokeInstanceMethod",
					methodType(Object.class, Object.class, String.class, Object[].class));
			REFLECTOR_INVOKE_NO_ARG_INSTANCE_MEMBER = lookup.findStatic(Reflector.class, "invokeNoArgInstanceMember",
					methodType(Object.class, Object.class, String.class, boolean.class));
			REFLECTOR_SET_INSTANCE_FIELD = lookup.findStatic(Reflector.class, "setInstanceField",
					methodType(Object.class, Object.class, String.class, Object.class));

			BOX_ARG =  lookup.findStatic(Reflector.class, "boxArg",
					methodType(Object.class, Class.class, Object.class));
			PREP_RET = lookup.findStatic(Reflector.class, "prepRet",
					methodType(Object.class, Class.class, Object.class))
					.bindTo(Boolean.class).asType(methodType(Boolean.class, Object.class));
		  }
		catch (NoSuchMethodException | IllegalAccessException e)
	  	{
			throw new AssertionError(e);
	  	}
		}

	@SuppressWarnings("unchecked")
	private static Class<? extends Annotation> findCallerSensitiveClass() {
		Class<?> callerSensitive;
		try
			{  // CallerSensitive for JDK8
			callerSensitive = Class.forName("sun.reflect.CallerSensitive", false, null);
			}
		catch (ClassNotFoundException e)
			{
			try
				{  // CallerSensitive for JDK9+
				callerSensitive = Class.forName("jdk.internal.reflect.CallerSensitive", false, null);
				}
			catch (ClassNotFoundException e2)
				{
				throw new AssertionError(e2);
				}
		}
		return (Class<? extends Annotation>) callerSensitive;
	}

	private static final ClassValue<MethodHandle> BOX_ARG_CACHE = new ClassValue<MethodHandle>() {
		protected MethodHandle computeValue(Class<?> type) {
			if (!type.isPrimitive())
				throw new AssertionError("only primitive conversion are allowed");
			return BOX_ARG.bindTo(type).asType(methodType(type, Object.class));
		}
	};

	private static MethodHandle boxArg(Class<?> c) {
		if (!c.isPrimitive())
			return null;
		return BOX_ARG_CACHE.get(c);
	}

	private static MethodHandle prepRet(Class<?> c) {
		if (c != Boolean.class)
			return null;
		return PREP_RET;
	}

	private static MethodHandle convert(MethodHandle mh, int start) {
		// filter arguments
		MethodType type = mh.type();
		int parameterCount = type.parameterCount();
		MethodHandle[] filters = null; // lazily allocated
		for(int i = start; i < parameterCount; i++)
			{
			MethodHandle boxArg = boxArg(type.parameterType(i));
			if (boxArg != null)
				{
				if (filters == null)  
					filters = new MethodHandle[parameterCount];
				filters[i] = boxArg;
				}
			}
		if (filters != null)
			mh = filterArguments(mh, 0, filters);

		// filter return value
		MethodHandle prepRet = prepRet(type.returnType());
		if (prepRet != null)
			mh = filterReturnValue(mh, prepRet);
		return mh;
	}

	static MethodHandle adjustTo(MethodHandle mh, int start, MethodType type) {
		MethodHandle converted = convert(mh, start);
		if (mh.isVarargsCollector())   // re-enable varargs collecting
			converted = converted.asVarargsCollector(Object[].class);
		return converted.asType(type);
	}


	/* behaviors */  

	private static MethodHandle asStaticMethodMH(Lookup lookup, Class<?> staticClass, String methodName, int arity) {
		if (methodName.equals("new"))
			return asConstructorMH(lookup, staticClass, arity);

		List<Method> methods = Reflector.getMethods(staticClass, arity, methodName, true);
		if (methods.size() != 1)
			return null;

		Method method = methods.get(0);
		if (method.isAnnotationPresent(CALLER_SENSITIVE))
			return null;

		MethodHandle mh;
		try
			{
			mh = lookup.unreflect(method);
			}
		catch (IllegalAccessException e)
			{  // don't try to optimize, so the stack trace of the error is identical as the one without indy
			return null;
			}
		return mh;
	}

	private static MethodHandle asConstructorMH(Lookup lookup, Class<?> staticClass, int arity) { 
		List<Constructor<?>> constructors = Reflector.getConstructors(staticClass, arity);
		if (constructors.size() != 1)
			return null;

		Constructor<?> constructor = constructors.get(0);
		if (constructor.isAnnotationPresent(CALLER_SENSITIVE))
			return null;

		MethodHandle mh;
		try
			{
			mh = lookup.unreflectConstructor(constructor);
			}
		catch (IllegalAccessException e)
			{  // don't try to optimize, so the stack trace of the error is identical as the one without indy
			return null;
			}
		return mh;
	}

	private static MethodHandle asInstanceMethodMH(Lookup lookup, Class<?> dynamicClass, String methodName, int arity) {
		List<Method> methods = Reflector.getMethods(dynamicClass, arity, methodName, false);
		if (methods.size() != 1)
			return null;

		Method method = methods.get(0);
		if (method.isAnnotationPresent(CALLER_SENSITIVE))
			return null;

		try
			{
			return lookup.unreflect(method);
			}
		catch (IllegalAccessException e)
			{  // don't try to optimize, so the stack trace of the error is identical as the one without indy
			return null;
			}
	}

	private static MethodHandle asGetterFieldMH(Lookup lookup, Class<?> dynamicClass, String fieldName) {
		Field field = Reflector.getField(dynamicClass, fieldName, false);
		if (field == null)
			return null;

		try
			{
			return lookup.unreflectGetter(field);
			}
		catch (IllegalAccessException e)
			{  // don't try to optimize, so the stack trace of the error is identical as the one without indy
			return null;
			}
	}

	private static MethodHandle asSetterFieldMH(Lookup lookup, Class<?> dynamicClass, String fieldName) {
		Field field = Reflector.getField(dynamicClass, fieldName, false);
		if (field == null)
			return null;

		MethodHandle setter;
		try
			{
			setter = lookup.unreflectSetter(field);
			}
		catch (IllegalAccessException e)
			{  // don't try to optimize, so the stack trace of the error is identical as the one without indy
			return null;
			}

		return foldArguments(dropArguments(identity(Object.class), 0, Object.class), setter);
	}


	/* fallback method handles that calls the Reflector APIs */

	static MethodHandle dynamicInvokeStaticMethod(Class<?> staticClass, String methodName, int arity){
		return insertArguments(REFLECTOR_INVOKE_STATIC_METHOD, 0, staticClass, methodName)
				.asCollector(Object[].class, arity);
	}

	static MethodHandle dynamicInvokeConstructor(Class<?> staticClass, int arity){
		return insertArguments(REFLECTOR_INVOKE_CONSTRUCTOR, 0, staticClass)
				.asCollector(Object[].class, arity);
	}

	static MethodHandle dynamicInvokeInstanceMethod(String methodName, int arity){
		return insertArguments(REFLECTOR_INVOKE_INSTANCE_METHOD, 1, methodName)
				.asCollector(Object[].class, arity);
	}

	static MethodHandle dynamicInvokeNoArgInstanceMember(String memberName, boolean requireField) {
		return insertArguments(REFLECTOR_INVOKE_NO_ARG_INSTANCE_MEMBER, 1, memberName, requireField);
	}

	static MethodHandle dynamicSetInstanceField(String fieldName) {
		return insertArguments(REFLECTOR_SET_INSTANCE_FIELD, 1, fieldName);
	}


	/* a versatile inlining cache used for all receiver based dispatch */

	private static final class InlineCache extends MutableCallSite {
		private interface Resolver {
			MethodHandle resolve(Class<?> dynamicClass, MethodType type);
			MethodHandle fallback(MethodType type);
		}

		private static final int MAX_DEPTH = 5;
		private static final int MAX_BAILOUT = 1;

		private static final MethodHandle TYPE_CHECK;
		private static final MethodHandle FALLBACK;
		static
			{
			Lookup lookup = MethodHandles.lookup();
			try
				{
				TYPE_CHECK = lookup.findStatic(InlineCache.class, "typeCheck",
						methodType(boolean.class, Class.class, Object.class));
				FALLBACK = lookup.findVirtual(InlineCache.class, "fallback",
						methodType(MethodHandle.class, Object.class));
				}
			catch (NoSuchMethodException | IllegalAccessException e)
				{
				throw new AssertionError(e);
				}
		}

		private static boolean typeCheck(Class<?> c, Object target) {
			return c == target.getClass();   // implicit NPE
		}

		private final Resolver resolver;
		private final InlineCache root;
		private final int depth;
		private final int bailout;

		InlineCache(MethodType type, Resolver resolver){
			super(type);
			this.resolver = resolver;
			this.root = this;
			this.depth = 1;
			this.bailout = 0;
			setTarget(foldArguments(exactInvoker(type), FALLBACK.bindTo(this)));
		}

		private InlineCache(MethodType type, Resolver resolver, InlineCache root, int depth, int bailout){
			super(type);
			this.resolver = resolver;
			this.root = root;
			this.depth = depth;
			this.bailout = bailout;
			setTarget(foldArguments(exactInvoker(type), FALLBACK.bindTo(this)));
		}

		private MethodHandle fallback(Object target) {
			MethodType type = type();
			if (depth == MAX_DEPTH)  // the inlining cache starts to become too big,
				{                      // we still keep the previous stages to optimize the common cases
				MethodHandle mh = resolver.fallback(type).asType(type);
				setTarget(mh);
				return mh;
				}

			Class<?> dynamicClass = target.getClass();
			int bailout = this.bailout;
			MethodHandle mh = resolver.resolve(dynamicClass, type);
			if (mh == null)
				{  // bailout
				mh = resolver.fallback(type()).asType(type);
				if (bailout == MAX_BAILOUT)
					{  // too many bailouts, discard the inlining cache and revert to dynamically invoke for all instances
					root.setTarget(mh);
					return mh;
					}

				bailout++;
				}
			else
				{
				mh = adjustTo(mh, 1, type());
				}

			MethodHandle gwt = MethodHandles.guardWithTest(
					TYPE_CHECK.bindTo(dynamicClass),
					mh,
					new InlineCache(type, resolver, root, depth + 1, bailout).dynamicInvoker());
			setTarget(gwt);
			return mh; 
		}
	}


	/* boostrap entrypoints */

	static CallSite bsm_invoke_static(Lookup lookup, String methodName, MethodType type, String className){
		Class<?> staticClass = RT.classForName(className);
		int arity = type.parameterCount();
		MethodHandle mh = asStaticMethodMH(lookup, staticClass, methodName, arity);
		if (mh == null)
			mh = dynamicInvokeStaticMethod(staticClass, methodName, arity).asType(type);
		return new ConstantCallSite(adjustTo(mh, 0, type));
	}

	static CallSite bsm_invoke_constructor(Lookup lookup, String methodName, MethodType type, String className){
		Class<?> staticClass = RT.classForName(className);
		int arity = type.parameterCount();
		MethodHandle mh = asConstructorMH(lookup, staticClass, arity);
		if (mh == null)
			mh = dynamicInvokeConstructor(staticClass, arity).asType(type);
		return new ConstantCallSite(adjustTo(mh, 0, type));
	}

	static CallSite bsm_invoke_instance(Lookup lookup, String methodName, MethodType type){
		return new InlineCache(type, new InlineCache.Resolver() {
			public MethodHandle resolve(Class<?> dynamicClass, MethodType type) {
				return asInstanceMethodMH(lookup, dynamicClass, methodName, type.parameterCount() - 1);
			}
			public MethodHandle fallback(MethodType type) {
				return dynamicInvokeInstanceMethod(methodName, type.parameterCount() - 1);
			}
		});
	}

	static CallSite bsm_invoke_no_arg_member(Lookup lookup, String memberName, MethodType type, Integer boxedRequireField){
		boolean requireField = boxedRequireField != 0;
		return new InlineCache(type, new InlineCache.Resolver() {
			public MethodHandle resolve(Class<?> dynamicClass, MethodType type) {
				if (requireField)
					return asGetterFieldMH(lookup, dynamicClass, memberName);
				List meths = Reflector.getMethods(dynamicClass, 0, memberName, false);
				if(!meths.isEmpty())
					return asInstanceMethodMH(lookup, dynamicClass, memberName, 0);
				return asGetterFieldMH(lookup, dynamicClass, memberName);
			}
			public MethodHandle fallback(MethodType type) {
				return dynamicInvokeNoArgInstanceMember(memberName, requireField);
			}
		});
	}

	static CallSite bsm_set_instance_field(Lookup lookup, String fieldName, MethodType type){
		return new InlineCache(type, new InlineCache.Resolver() {
			public MethodHandle resolve(Class<?> dynamicClass, MethodType type) {
				return asSetterFieldMH(lookup, dynamicClass, fieldName);
			}
			public MethodHandle fallback(MethodType type) {
				return dynamicSetInstanceField(fieldName);
			}
		});
	}
}
