//
//  NSTypeImpl.m
//  sample
//
//  Created by Gal Dolber on 2/4/14.
//  Copyright (c) 2014 clojure-objc. All rights reserved.
//

#import "NSTypeImpl.h"
#import "objc/runtime.h"
#import "objc/message.h"
#import "clojure/lang/Atom.h"
#import "java/lang/RuntimeException.h"
#import "java/lang/Character.h"
#import "clojure/lang/RT.h"
#import "NSCommon.h"
#import <UIKit/UIKit.h>

static ClojureLangAtom *dynamicClasses;

#define va_arg_p(type)\
    {\
    type v = va_arg(ap, type); \
    [i setArgument:&v atIndex:n];\
    break;\
    }\

#define dispatch_args(self, sel) \
    va_list ap; \
    va_start(ap, sel); \
    id o = [ClojureLangRT getWithId:[dynamicClasses deref] withId:NSStringFromClass([self class])]; \
    id pair = [ClojureLangRT getWithId:o withId:NSStringFromSelector(sel)]; \
    id fn =  [ClojureLangRT secondWithId:pair];\
    id types = [ClojureLangRT firstWithId:pair];\
    id sig;\
    if (types == nil) { \
        sig = [self methodSignatureForSelector:sel]; \
        types = [NSCommon signaturesToTypes:sig skipSel:YES]; \
    } else {\
        sig = [NSMethodSignature signatureWithObjCTypes:[NSCommon makeSignature:types]];\
    }\
    id retType = [ClojureLangRT firstWithId:types]; \
    types = [ClojureLangRT nextWithId:types]; \
    id ttypes = types; \
    NSInvocation *i = [NSInvocation invocationWithMethodSignature:sig]; \
    [i setSelector:sel]; \
    int n = 2; \
    while (types != nil) { \
        switch (to_char([ClojureLangRT firstWithId:types])) { \
            case float_type: va_arg_p(double)\
            case longlong_type: va_arg_p(long long)\
            case long_type: va_arg_p(long)\
            case char_type: va_arg_p(int)\
            case short_type: va_arg_p(int)\
            case int_type: va_arg_p(int)\
            case double_type: va_arg_p(double)\
            case ulong_type: va_arg_p(unsigned long)\
            case ulonglong_type: va_arg_p(unsigned long long)\
            case uchar_type: va_arg_p(int)\
            case ushort_type: va_arg_p(int)\
            case uint_type: va_arg_p(unsigned int)\
            case bool_type: va_arg_p(int)\
            case id_type: va_arg_p(id)\
            case cgpoint_type: va_arg_p(CGPoint)\
            case nsrange_type: va_arg_p(NSRange)\
            case uiedge_type: va_arg_p(UIEdgeInsets)\
            case cgsize_type: va_arg_p(CGSize)\
            case cgafflinetransform_type: va_arg_p(CGAffineTransform)\
            case catransform3d_type: va_arg_p(CATransform3D)\
            case uioffset_type: va_arg_p(UIOffset)\
            case cgrect_type: va_arg_p(CGRect)\
            case pointer_type: va_arg_p(void*)\
        } \
        n++; \
        types = [ClojureLangRT nextWithId:types]; \
    } \
    va_end(ap); \
    [NSCommon callWithInvocation:i withSelf:self withTypes:[ClojureLangRT consWithId:retType withId:ttypes] withFn:fn]; \

#define dispatch_args_r(self, sel, type)\
    dispatch_args(self, sel);\
    type r;\
    [i getReturnValue:&r];\
    return r;\

void dispatch_void(id self, SEL sel, ...) {
    dispatch_args(self, sel);
}

float dispatch_float(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, float);
}

long long  dispatch_longlong(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, long long);
}

long dispatch_long(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, long);
}

char dispatch_char(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, char);
}

short dispatch_short(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, short);
}

int dispatch_int(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, int);
}

double dispatch_double(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, double);
}

unsigned long long dispatch_unsigned_longlong(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, unsigned long long);
}

unsigned long dispatch_unsigned_long(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, unsigned long);
}

unsigned char dispatch_unsigned_char(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, unsigned char);
}

unsigned short dispatch_unsigned_short(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, unsigned short);
}

unsigned int dispatch_unsigned_int(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, unsigned int);
}

bool dispatch_bool(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, BOOL);
}

CGPoint dispatch_CGPoint(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, CGPoint);
}

NSRange dispatch_NSRange(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, NSRange);
}

UIEdgeInsets dispatch_UIEdgeInsets(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, UIEdgeInsets);
}

CGSize dispatch_CGSize(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, CGSize);
}

CGAffineTransform dispatch_CGAffineTransform(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, CGAffineTransform);
}

CATransform3D dispatch_CATransform3D(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, CATransform3D);
}

UIOffset dispatch_UIOffset(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, UIOffset);
}

CGRect dispatch_CGRect(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, CGRect);
}

id dispatch_id(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, id);
}

void* dispatch_pointer(id self, SEL sel, ...) {
    dispatch_args_r(self, sel, void*);
}

IMP getDispatch(char c) {
    switch (c) {
        case void_type: return (IMP)dispatch_void;
        case float_type: return (IMP)dispatch_float;
        case long_type: return (IMP)dispatch_long;
        case longlong_type: return (IMP)dispatch_longlong;
        case char_type: return (IMP)dispatch_char;
        case short_type: return (IMP)dispatch_short;
        case int_type: return (IMP)dispatch_int;
        case double_type: return (IMP)dispatch_double;
        case ulong_type: return (IMP)dispatch_unsigned_long;
        case ulonglong_type: return (IMP)dispatch_unsigned_longlong;
        case uchar_type: return (IMP)dispatch_unsigned_char;
        case ushort_type: return (IMP)dispatch_unsigned_short;
        case uint_type: return (IMP)dispatch_unsigned_int;
        case bool_type: return (IMP)dispatch_bool;
        case cgpoint_type: return (IMP)dispatch_CGPoint;
        case nsrange_type: return (IMP)dispatch_NSRange;
        case uiedge_type: return (IMP)dispatch_UIEdgeInsets;
        case cgsize_type: return (IMP)dispatch_CGSize;
        case cgafflinetransform_type: return (IMP)dispatch_CGAffineTransform;
        case catransform3d_type: return (IMP)dispatch_CATransform3D;
        case uioffset_type: return (IMP)dispatch_UIOffset;
        case cgrect_type: return (IMP)dispatch_CGRect;
        case pointer_type: return (IMP)dispatch_pointer;
    }
    return (IMP)dispatch_id;
}

@implementation NSTypeImpl

+(void)initialize {
    dynamicClasses = [[ClojureLangAtom alloc] initWithId:[ClojureLangPersistentHashMap EMPTY]];
}

+(Class) makeClassWithName:(NSString*)name superclass:(NSString*)s map:(ClojureLangAPersistentMap*)m {
    [dynamicClasses swapWithClojureLangIFn:[[ClojureLangRT varWithNSString:@"clojure.core" withNSString:@"assoc"] deref]
                                    withId:name withId:m];
    Class superc = NSClassFromString(s);
    Class clazz = objc_allocateClassPair(superc, [name UTF8String], 0);
    if (clazz == nil) {
        @throw [[JavaLangRuntimeException alloc] initWithNSString:[@"Class already exists: " stringByAppendingString:name]];
    }
    id seq = [m seq];
    while (seq != nil) {
        id f = [ClojureLangRT firstWithId:seq];
        SEL sel = NSSelectorFromString([ClojureLangRT firstWithId:f]);
        id types = [ClojureLangRT firstWithId:[ClojureLangRT secondWithId:f]];
        void * d;
        const char * enc;
        if (types == nil) {
            Method method = class_getClassMethod(superc, sel);
            if (method == nil) {
                method = class_getInstanceMethod(superc, sel);
            }
            char ret[256];
            method_getReturnType(method, ret, 256);
            d = getDispatch([NSCommon signatureToType:ret]);
            enc = method_getTypeEncoding(method);
        } else {
            id r = [ClojureLangRT firstWithId:types];
            d = getDispatch(to_char(r));
            enc = [NSCommon makeSignature:types];
        }
        class_addMethod(clazz, sel, d, enc);
        seq = [ClojureLangRT nextWithId:seq];
    }
    
    objc_registerClassPair(clazz);
    return clazz;
}

@end
