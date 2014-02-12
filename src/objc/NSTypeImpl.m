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
#import "clojure/lang/APersistentMap.h"
#import "clojure/lang/PersistentVector.h"
#import "clojure/lang/PersistentHashMap.h"
#import "clojure/lang/RT.h"
#import "NSCommon.h"
#import <UIKit/UIKit.h>

static ClojureLangAtom *dynamicClasses;

#define dispatch_args(self, sel) \
va_list ap; \
va_start(ap, sel); \
id o = [ClojureLangRT getWithId:[dynamicClasses deref] withId:NSStringFromClass([self class])]; \
id s = [ClojureLangRT getWithId:o withId:NSStringFromSelector(sel)]; \
id fn = [ClojureLangRT firstWithId:s]; \
id types = [NSCommon signaturesToTypes:[[self class] methodSignatureForSelector:sel]]; \
types = [ClojureLangRT nextWithId:types]; \
NSInvocation *i = [NSInvocation new]; \
[i setSelector:sel]; \
[i setArgument:&self atIndex:0]; \
int n = 1; \
while (types != nil) { \
    void * p; \
    switch (to_char([ClojureLangRT firstWithId:types])) { \
        case float_type: { \
            float v = va_arg(ap, double); \
            p = &v; \
            break; \
        } \
        case longlong_type: { \
            long long v = va_arg(ap, long long); \
            p = &v; \
            break; \
        } \
        case long_type: { \
            long v = va_arg(ap, long); \
            p = &v; \
            break; \
        } \
        case char_type: { \
            char v = va_arg(ap, int); \
            p = &v; \
            break; \
        } \
        case short_type: { \
            short v = va_arg(ap, int); \
            p = &v; \
            break; \
        } \
        case int_type: { \
            int v = va_arg(ap, int); \
            p = &v; \
            break; \
        } \
        case double_type: { \
            double v = va_arg(ap, double); \
            p = &v; \
            break; \
        } \
        case ulong_type: { \
            unsigned long v = va_arg(ap, unsigned long); \
            p = &v; \
            break; \
        } \
        case ulonglong_type: { \
            unsigned long long v = va_arg(ap, unsigned long long); \
            p = &v; \
            break; \
        } \
        case uchar_type: { \
            unsigned char v = va_arg(ap, unsigned int); \
            p = &v; \
            break; \
        } \
        case ushort_type: { \
            unsigned short v = va_arg(ap, unsigned int); \
            p = &v; \
            break; \
        } \
        case uint_type: { \
            unsigned int v = va_arg(ap, unsigned int); \
            p = &v; \
            break; \
        } \
        case bool_type: { \
            BOOL v = va_arg(ap, int) == 1 ? YES : NO; \
            p = &v; \
            break; \
        } \
        case id_type: { \
            void * v = va_arg(ap, void *); \
            p = &v; \
            break; \
        } \
        case cgpoint_type: { \
            CGPoint v = va_arg(ap, CGPoint); \
            p = &v; \
            break; \
        } \
        case nsrange_type: { \
            NSRange v = va_arg(ap, NSRange); \
            p = &v; \
            break; \
        } \
        case uiedge_type: { \
            UIEdgeInsets v = va_arg(ap, UIEdgeInsets); \
            p = &v; \
            break; \
        } \
        case cgsize_type: { \
            CGSize v = va_arg(ap, CGSize); \
            p = &v; \
            break; \
        } \
        case cgafflinetransform_type: { \
            CGAffineTransform v = va_arg(ap, CGAffineTransform); \
            p = &v; \
            break; \
        } \
        case catransform3d_type: { \
            CATransform3D v = va_arg(ap, CATransform3D); \
            p = &v; \
            break; \
        } \
        case uioffset_type: { \
            UIOffset v = va_arg(ap, UIOffset); \
            p = &v; \
            break; \
        } \
        case cgrect_type: { \
            CGRect v = va_arg(ap, CGRect); \
            p = &v; \
            break; \
        } \
        case pointer_type: { \
            void* v = va_arg(ap, void*); \
            p = &v; \
            break; \
        } \
    } \
    [i setArgument:&p atIndex:n]; \
    n++; \
    types = [ClojureLangRT nextWithId:types]; \
} \
va_end(ap); \
[NSCommon callWithInvocation:i withTypes:types withFn:fn]; \

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
    [dynamicClasses swapWithClojureLangIFn:[[ClojureLangRT varWithNSString:@"clojure.core" withNSString:@"assoc"] deref] withId:name withId:m];
    Class superc = NSClassFromString(s);
    Class clazz = objc_allocateClassPair(superc, [name UTF8String], 0);
    
    id seq = [m seq];
    while (seq != nil) {
        id f = [ClojureLangRT firstWithId:seq];
        SEL sel = NSSelectorFromString([ClojureLangRT firstWithId:f]);
        Method method = class_getInstanceMethod(superc, sel);
        char ret[256];
        method_getReturnType(method, ret, 256);
        
        class_addMethod(clazz, sel, getDispatch([NSCommon signatureToType:ret]), method_getTypeEncoding(method));
        seq = [ClojureLangRT nextWithId:seq];
    }
    
    // Add state ivar
    /*char *enc = @encode(id);
    NSUInteger pos, align;
    NSGetSizeAndAlignment(enc, &pos, &align);
    class_addIvar(clazz, "state", align, align, enc);*/
    
    objc_registerClassPair(clazz);
    return clazz;
}

@end
