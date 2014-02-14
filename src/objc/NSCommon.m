//
//  NSCommon.m
//  sample
//
//  Created by Gal Dolber on 2/4/14.
//  Copyright (c) 2014 clojure-objc. All rights reserved.
//

#import "NSCommon.h"
#import "clojure/lang/AFn.h"
#import "clojure/lang/RT.h"
#import "clojure/lang/Atom.h"
#import "clojure/lang/PersistentVector.h"
#import "clojure/lang/PersistentHashMap.h"
#import "clojure/lang/Selector.h"
#import "clojure/lang/Var.h"
#import "java/lang/Character.h"
#import "java/lang/Boolean.h"
#import "java/lang/Integer.h"
#import "java/lang/Double.h"
#import "java/lang/Float.h"
#import "java/lang/Long.h"
#import "java/lang/Short.h"
#import "ffi_mini.h"
#import "objc/runtime.h"
#import "objc/message.h"
#import <UIKit/UIKit.h>
#import "WeakRef.h"

static ClojureLangAtom *publiccfunctions;

static id cons;
static id conj;
static id assoc;

#if CGFLOAT_IS_DOUBLE
#define CGFloatFFI &ffim_type_double
#else
#define CGFloatFFI &ffim_type_float
#endif

static ffim_type CGPointFFI = (ffim_type){
    .size = 0,
    .alignment = 0,
    .type = FFIM_TYPE_STRUCT,
    .elements = (ffim_type * [3]){CGFloatFFI, CGFloatFFI, NULL}};

static ffim_type CGSizeFFI = (ffim_type){
    .size = 0,
    .alignment = 0,
    .type = FFIM_TYPE_STRUCT,
    .elements = (ffim_type * [3]){CGFloatFFI, CGFloatFFI, NULL}};

static ffim_type CGRectFFI = (ffim_type){
    .size = 0,
    .alignment = 0,
    .type = FFIM_TYPE_STRUCT,
    .elements = (ffim_type * [3]){&CGPointFFI, &CGSizeFFI, NULL}};

static ffim_type NSRangeFFI = (ffim_type){
    .size = 0,
    .alignment = 0,
    .type = FFIM_TYPE_STRUCT,
    .elements = (ffim_type * [3]){&ffim_type_uint, &ffim_type_uint, NULL}};

static ffim_type UIEdgeInsetsFFI = (ffim_type){
    .size = 0,
    .alignment = 0,
    .type = FFIM_TYPE_STRUCT,
    .elements = (ffim_type * [5]){CGFloatFFI, CGFloatFFI, CGFloatFFI, CGFloatFFI, NULL}};

static ffim_type UIOffsetFFI = (ffim_type){
    .size = 0,
    .alignment = 0,
    .type = FFIM_TYPE_STRUCT,
    .elements = (ffim_type * [3]){CGFloatFFI, CGFloatFFI, NULL}};

static ffim_type CATransform3DFFI = (ffim_type){
    .size = 0,
    .alignment = 0,
    .type = FFIM_TYPE_STRUCT,
    .elements = (ffim_type * [17]){
        CGFloatFFI, CGFloatFFI, CGFloatFFI, CGFloatFFI,
        CGFloatFFI, CGFloatFFI, CGFloatFFI, CGFloatFFI,
        CGFloatFFI, CGFloatFFI, CGFloatFFI, CGFloatFFI,
        CGFloatFFI, CGFloatFFI, CGFloatFFI, CGFloatFFI,
        NULL}};

static ffim_type CGAffineTransformFFI = (ffim_type){
    .size = 0,
    .alignment = 0,
    .type = FFIM_TYPE_STRUCT,
    .elements = (ffim_type * [7]){
        CGFloatFFI, CGFloatFFI, CGFloatFFI, CGFloatFFI, CGFloatFFI, CGFloatFFI, NULL}};

const char* encode_type(char d) {
    switch (d) {
        case void_type: return @encode(void);
        case float_type: return @encode(float);
        case long_type: return @encode(long);
        case longlong_type: return @encode(long long);
        case char_type: return @encode(char);
        case short_type: return @encode(short);
        case int_type: return @encode(int);
        case double_type: return @encode(double);
        case ulong_type: return @encode(unsigned long);
        case ulonglong_type: return @encode(unsigned long long);
        case uchar_type: return @encode(unsigned char);
        case ushort_type: return @encode(unsigned short);
        case uint_type: return @encode(unsigned int);
        case bool_type: return @encode(BOOL);
        case id_type: return @encode(id);
        case cgpoint_type: return @encode(CGPoint);
        case nsrange_type: return @encode(NSRange);
        case uiedge_type: return @encode(UIEdgeInsets);
        case cgsize_type: return @encode(CGSize);
        case cgafflinetransform_type: return @encode(CGAffineTransform);
        case catransform3d_type: return @encode(CATransform3D);
        case uioffset_type: return @encode(UIOffset);
        case cgrect_type: return @encode(CGRect);
        case pointer_type: return @encode(void*);
    }
    return @encode(id);
}

void * ffi_type_for_type(char type) {
    switch (type) {
        case void_type: return &ffim_type_void;
        case float_type: return &ffim_type_float;
        case longlong_type: return &ffim_type_sint64;
        case long_type: return &ffim_type_slong;
        case char_type: return &ffim_type_schar;
        case short_type: return &ffim_type_sshort;
        case int_type: return &ffim_type_sint;
        case double_type: return &ffim_type_double;
        case ulonglong_type: return &ffim_type_uint64;
        case ulong_type: return &ffim_type_ulong;
        case uchar_type: return &ffim_type_uchar;
        case ushort_type: return &ffim_type_ushort;
        case uint_type: return &ffim_type_uint;
        case bool_type: return &ffim_type_schar;
        case cgpoint_type: return &CGPointFFI;
        case nsrange_type: return &NSRangeFFI;
        case uiedge_type: return &UIEdgeInsetsFFI;
        case cgsize_type: return &CGSizeFFI;
        case cgafflinetransform_type: return &CGAffineTransformFFI;
        case catransform3d_type: return &CATransform3DFFI;
        case uioffset_type: return &UIOffsetFFI;
        case cgrect_type: return &CGRectFFI;
        default: return &ffim_type_pointer;
    }
}

int sizeof_type(char c) {
    switch (c) {
        case void_type: return sizeof(void);
        case float_type: return sizeof(float);
        case long_type: return sizeof(long);
        case longlong_type: return sizeof(long long);
        case char_type: return sizeof(char);
        case short_type: return sizeof(short);
        case int_type: return sizeof(int);
        case double_type: return sizeof(double);
        case ulong_type: return sizeof(unsigned long);
        case ulonglong_type: return sizeof(unsigned long long);
        case uchar_type: return sizeof(unsigned char);
        case ushort_type: return sizeof(unsigned short);
        case uint_type: return sizeof(unsigned int);
        case bool_type: return sizeof(BOOL);
        case cgpoint_type: return sizeof(CGPoint);
        case nsrange_type: return sizeof(NSRange);
        case uiedge_type: return sizeof(UIEdgeInsets);
        case cgsize_type: return sizeof(CGSize);
        case cgafflinetransform_type: return sizeof(CGAffineTransform);
        case catransform3d_type: return sizeof(CATransform3D);
        case uioffset_type: return sizeof(UIOffset);
        case cgrect_type: return sizeof(CGRect);
        case pointer_type: return sizeof(void*);
    }
    return sizeof(id);
}

void * malloc_ret(char c) {
    if (c == void_type) {
        return malloc(sizeof_type(id_type));
    }
    return malloc(sizeof_type(c));
}

static int r0_size = sizeof(unsigned long long);

// https://developer.apple.com/library/ios/documentation/Xcode/Conceptual/iPhoneOSABIReference/iPhoneOSABIReference.pdf
BOOL use_stret(id object, NSString* selector) {
    SEL sel = NSSelectorFromString(selector);
    Method method = class_getInstanceMethod([([object isKindOfClass:[WeakRef class]] ?
                                              [(WeakRef*)object deref] : object) class], sel);
    if (method == nil) {
        method = class_getClassMethod([([object isKindOfClass:[WeakRef class]] ?
                                        [(WeakRef*)object deref] : object) class], sel);
    }
    char ret[256];
    method_getReturnType(method, ret, 256);
    char t = [NSCommon signatureToType:ret];
    return sizeof_type(t) > r0_size;
}

@implementation NSCommon

+(BOOL)cgfloatIsDouble {
#if CGFLOAT_IS_DOUBLE
    return YES;
#else
    return NO;
#endif
}

+(void)initialize {
    assoc = [ClojureLangRT varWithNSString:@"clojure.core" withNSString:@"assoc"];
    cons = [ClojureLangRT varWithNSString:@"clojure.core" withNSString:@"cons"];
    conj = [ClojureLangRT varWithNSString:@"clojure.core" withNSString:@"conj"];
    publiccfunctions = [[ClojureLangAtom alloc] initWithId:[ClojureLangPersistentHashMap EMPTY]];
    
    // Register main functions
    reg_c(objc_msgSend);
    reg_c(objc_msgSendSuper);
#ifndef __arm64__
    reg_c(objc_msgSend_stret);
    reg_c(objc_msgSendSuper_stret);
#endif
    reg_c(NSStringFromClass);
    reg_c(NSSelectorFromString);
    reg_c(NSClassFromString);
    reg_c(printf);
    reg_c(NSLog);
    reg_c(CGRectMake);
    reg_c(CGSizeMake);
    reg_c(CGPointMake);
    reg_c(objc_getAssociatedObject);
    reg_c(objc_setAssociatedObject);
}

+(void)registerCFunction:(NSString*)name fn:(void*)fn {
    [publiccfunctions swapWithClojureLangIFn:assoc
                                      withId:name
                                      withId:[NSValue valueWithPointer:FFI_FN(fn)]];
}

#define make_pointer(e,type)\
    type o = e;\
    type *p = malloc(sizeof(type));\
    memcpy(p, &o, sizeof(type));\
    ret = p;\

+(id)ccall:(id)name types:(id)types args:(id)args {
    char retType = to_char([ClojureLangRT firstWithId:types]);
    void *result_value = malloc_ret(retType);

    int count = [ClojureLangRT countFromWithId:args];
    ffim_type **argument_types = (ffim_type **) malloc (count * sizeof(ffim_type *));
    void **argument_values = (void **) malloc (count * sizeof(void *));
    for (int n=0; n < count; n++) {
        char type = to_char([ClojureLangRT nthFromWithId:types withInt:n+1]);
        argument_types[n] = ffi_type_for_type(type);
        id v = [ClojureLangRT nthFromWithId:args withInt:n];
        void * ret;
        switch (type) {
            case void_type: {
                make_pointer([NSNull null], id);
                break;
            }
            case float_type: {
                make_pointer([ClojureLangRT floatCastWithId:v], float);
                break;
            }
            case longlong_type: {
                make_pointer([ClojureLangRT longCastWithId:v], long long);
                break;
            }
            case long_type: {
                make_pointer((long)[ClojureLangRT longCastWithId:v], long);
                break;
            }
            case char_type: {
                if ([v isKindOfClass:[JavaLangBoolean class]]) {
                    make_pointer([ClojureLangRT booleanCastWithId:v], BOOL);
                } else {
                    make_pointer([ClojureLangRT charCastWithId:v], char);
                }
                break;
            }
            case short_type: {
                make_pointer([ClojureLangRT shortCastWithId:v], short);
                break;
            }
            case int_type: {
                make_pointer([ClojureLangRT intCastWithId:v], int);
                break;
            }
            case double_type: {
                make_pointer([ClojureLangRT doubleCastWithId:v], double);
                break;
            }
            case ulong_type: {
                make_pointer((unsigned long)[ClojureLangRT longCastWithId:v], unsigned long);
                break;
            }
            case ulonglong_type: {
                make_pointer([ClojureLangRT longCastWithId:v], unsigned long long);
                break;
            }
            case uchar_type: {
                make_pointer([ClojureLangRT charCastWithId:v], unsigned char);
                break;
            }
            case ushort_type: {
                make_pointer([ClojureLangRT shortCastWithId:v], unsigned short);
                break;
            }
            case uint_type: {
                make_pointer([ClojureLangRT intCastWithId:v], unsigned int);
                break;
            }
            case bool_type: {
                make_pointer([ClojureLangRT booleanCastWithId:v], BOOL);
                break;
            }
            case id_type: {
                make_pointer([v isKindOfClass:[WeakRef class]] ? [(WeakRef*)v deref] : v, id);
                break;
            }
            case cgpoint_type: {
                make_pointer([((NSValue*) v) CGPointValue], CGPoint);
                break;
            }
            case nsrange_type: {
                make_pointer([((NSValue*) v) rangeValue], NSRange);
                break;
            }
            case uiedge_type: {
                make_pointer([((NSValue*) v) UIEdgeInsetsValue], UIEdgeInsets);
                break;
            }
            case cgsize_type: {
                make_pointer([((NSValue*) v) CGSizeValue], CGSize);
                break;
            }
            case cgafflinetransform_type: {
                make_pointer([((NSValue*) v) CGAffineTransformValue], CGAffineTransform);
                break;
            }
            case catransform3d_type: {
                make_pointer([((NSValue*) v) CATransform3DValue], CATransform3D);
                break;
            }
            case uioffset_type: {
                make_pointer([((NSValue*) v) UIOffsetValue], UIOffset);
                break;
            }
            case cgrect_type: {
                make_pointer([((NSValue*) v) CGRectValue], CGRect);
                break;
            }
            case pointer_type: {
                if ([v isKindOfClass:[ClojureLangSelector class]]) {
                    make_pointer(NSSelectorFromString([(ClojureLangSelector*)v getName]), SEL);
                } else {
                    make_pointer([((NSValue*) v) pointerValue], void*);
                }
                break;
            }
        }
        argument_values[n] = ret;
    }
    
    ffim_cif c;
    ffim_type *result_type = ffi_type_for_type(retType);
    int status = ffi_mini_prep_cif(&c, FFIM_DEFAULT_ABI, (unsigned int) count, result_type, argument_types);
    
    void *fn = [(NSValue*)[ClojureLangRT getWithId:[publiccfunctions deref] withId:name] pointerValue];
    if (fn == nil) {
        @throw [[NSException alloc] initWithName:@"Function not registered. Register with: reg_c({function});" reason:name userInfo:nil];
    }
    ffi_mini_call(&c, fn, result_value, argument_values);

    for (int n=0; n < count; n++) {
        free(argument_values[n]);
    }
    free(argument_types);
    free(argument_values);
    
    id result;
    switch (retType) {
        case void_type: {
            result = [NSNull null];
            break;
        }
        case float_type: {
            float v = *(float*)result_value;
            result = [[[JavaLangFloat alloc] initWithFloat:v] autorelease];
            break;
        }
        case long_type: {
            long v = (long)*(long*)result_value;
            result = [[[JavaLangLong alloc] initWithLong:v] autorelease];
            break;
        }
        case longlong_type: {
            long long v = *(long long*)result_value;
            result = [[[JavaLangLong alloc] initWithLong:v] autorelease];
            break;
        }
        case char_type: {
            if (*(char*)result_value == YES) {
                result = [JavaLangBoolean getTRUE];
            } else if (*(char*)result_value == NO) {
                result = [JavaLangBoolean getFALSE];
            } else {
                result = [[[JavaLangCharacter alloc] initWithChar:*(char*)result_value] autorelease];
            }
            break;
        }
        case short_type: {
            short v = *(short*)result_value;
            result = [[[JavaLangShort alloc] initWithShort:v] autorelease];
            break;
        }
        case int_type: {
            int v = *(int*)result_value;
            result = [[[JavaLangInteger alloc] initWithInt:v] autorelease];
            break;
        }
        case double_type: {
            double v = *(double*)result_value;
            result = [[[JavaLangDouble alloc] initWithDouble:v] autorelease];
            break;
        }
        case ulong_type: {
            unsigned long v = (unsigned long)*(unsigned long long*)result_value;
            result = [[[JavaLangLong alloc] initWithLong:v] autorelease];
            break;
        }
        case ulonglong_type: {
            unsigned long long v = *(unsigned long long*)result_value;
            result = [[[JavaLangLong alloc] initWithLong:v] autorelease];
            break;
        }
        case uchar_type: {
            unsigned char v = *(unsigned char*)result_value;
            result = [[[JavaLangCharacter alloc] initWithChar:v] autorelease];
            break;
        }
        case ushort_type: {
            unsigned short v = *(unsigned short*)result_value;
            result = [[[JavaLangShort alloc] initWithShort:v] autorelease];
            break;
        }
        case uint_type: {
            unsigned int v = *(unsigned int*)result_value;
            result = [[[JavaLangInteger alloc] initWithInt:v] autorelease];
            break;
        }
        case bool_type: {
            result = *(char*)result_value == YES ? [JavaLangBoolean getTRUE] : [JavaLangBoolean getFALSE];
            break;
        }
        case cgpoint_type: {
            CGPoint v = *(CGPoint*)result_value;
            result = [NSValue valueWithCGPoint:v];
            break;
        }
        case nsrange_type: {
            NSRange v = *(NSRange*)result_value;
            result = [NSValue valueWithRange:v];
            break;
        }
        case uiedge_type: {
            UIEdgeInsets v = *(UIEdgeInsets*)result_value;
            result = [NSValue valueWithUIEdgeInsets:v];
            break;
        }
        case cgsize_type: {
            CGSize v = *(CGSize*)result_value;
            result = [NSValue valueWithCGSize:v];
            break;
        }
        case cgafflinetransform_type: {
            CGAffineTransform v = *(CGAffineTransform*)result_value;
            result = [NSValue valueWithCGAffineTransform:v];
            break;
        }
        case catransform3d_type: {
            CATransform3D v = *(CATransform3D*)result_value;
            result = [NSValue valueWithCATransform3D:v];
            break;
        }
        case uioffset_type: {
            UIOffset v = *(UIOffset*)result_value;
            result = [NSValue valueWithUIOffset:v];
            break;
        }
        case cgrect_type: {
            CGRect v = *(CGRect*)result_value;
            result = [NSValue valueWithCGRect:v];
            break;
        }
        case pointer_type: {
            void * v = *(void**)result_value;
            result = [NSValue valueWithPointer:v];
            break;
        }
        default: {
            result = *(void**)result_value;
        }
    }
    free(result_value);
    return result;
}

+(void)callWithInvocation:(NSInvocation *)invocation withSelf:(id)sself withTypes:(id)types withFn: (ClojureLangAFn*) fn
{
    id retType = [ClojureLangRT firstWithId: types];
    types = [ClojureLangRT nextWithId:types];
    id args = [ClojureLangPersistentVector EMPTY];
    args = [conj invokeWithId:args withId:[[[WeakRef alloc] initWith:sself] autorelease]];
    for (int n = 0; n < [ClojureLangRT countFromWithId:types]; n++) {
        id val = nil;
        int j = n + 2;
        switch (to_char([ClojureLangRT nthFromWithId:types withInt:n])) {
            case void_type: {
                break;
            }
            case float_type: {
                float v;
                [invocation getArgument:&v atIndex: j];
                val = [[[JavaLangFloat alloc] initWithFloat:v] autorelease];
                break;
            }
            case longlong_type: {
                long long v;
                [invocation getArgument:&v atIndex: j];
                val = [[[JavaLangLong alloc] initWithLong:v] autorelease];
                break;
            }
            case long_type: {
                long v;
                [invocation getArgument:&v atIndex: j];
                val = [[[JavaLangLong alloc] initWithLong:v] autorelease];
                break;
            }
            case char_type: {
                char v;
                [invocation getArgument:&v atIndex: j];
                val = [[[JavaLangCharacter alloc] initWithChar:v] autorelease];
                break;
            }
            case short_type: {
                short v;
                [invocation getArgument:&v atIndex: j];
                val = [[[JavaLangShort alloc] initWithShort:v] autorelease];
                break;
            }
            case int_type: {
                int v;
                [invocation getArgument:&v atIndex: j];
                val = [[[JavaLangInteger alloc] initWithInt:v] autorelease];
                break;
            }
            case double_type: {
                double v;
                [invocation getArgument:&v atIndex: j];
                val = [[[JavaLangDouble alloc] initWithDouble:v] autorelease];
                break;
            }
            case ulong_type: {
                unsigned long v;
                [invocation getArgument:&v atIndex: j];
                val = [[[JavaLangLong alloc] initWithLong:v] autorelease];
                break;
            }
            case ulonglong_type: {
                unsigned long long v;
                [invocation getArgument:&v atIndex: j];
                val = [[[JavaLangLong alloc] initWithLong:v] autorelease];
                break;
            }
            case uchar_type: {
                unsigned char v;
                [invocation getArgument:&v atIndex: j];
                val = [[[JavaLangCharacter alloc] initWithChar:v] autorelease];
                break;
            }
            case ushort_type: {
                unsigned short v;
                [invocation getArgument:&v atIndex: j];
                val = [[[JavaLangShort alloc] initWithShort:v] autorelease];
                break;
            }
            case uint_type: {
                unsigned int v;
                [invocation getArgument:&v atIndex: j];
                val = [[[JavaLangInteger alloc] initWithInt:v] autorelease];
                break;
            }
            case bool_type: {
                char v;
                [invocation getArgument:&v atIndex: j];
                if (v == YES) {
                    val = [JavaLangBoolean getTRUE];
                } else {
                    val = [JavaLangBoolean getFALSE];
                }
                break;
            }
            case id_type: {
                void * v;
                [invocation getArgument:&v atIndex:j];
                val = v;
                break;
            }
            case cgpoint_type: {
                CGPoint v;
                [invocation getArgument:&v atIndex: j];
                val = [NSValue valueWithCGPoint:v];
                break;
            }
            case nsrange_type: {
                NSRange v;
                [invocation getArgument:&v atIndex: j];
                val = [NSValue valueWithRange:v];
                break;
            }
            case uiedge_type: {
                UIEdgeInsets v;
                [invocation getArgument:&v atIndex: j];
                val = [NSValue valueWithUIEdgeInsets:v];
                break;
            }
            case cgsize_type: {
                CGSize v;
                [invocation getArgument:&v atIndex: j];
                val = [NSValue valueWithCGSize:v];
                break;
            }
            case cgafflinetransform_type: {
                CGAffineTransform v;
                [invocation getArgument:&v atIndex: j];
                val = [NSValue valueWithCGAffineTransform:v];
                break;
            }
            case catransform3d_type: {
                CATransform3D v;
                [invocation getArgument:&v atIndex: j];
                val = [NSValue valueWithCATransform3D:v];
                break;
            }
            case uioffset_type: {
                UIOffset v;
                [invocation getArgument:&v atIndex: j];
                val = [NSValue valueWithUIOffset:v];
                break;
            }
            case cgrect_type: {
                CGRect v;
                [invocation getArgument:&v atIndex: j];
                val = [NSValue valueWithCGRect:v];
                break;
            }
            case pointer_type: {
                void* v;
                [invocation getArgument:&v atIndex: j];
                val = [NSValue valueWithPointer:v];
                break;
            }
            default: @throw [NSException exceptionWithName:@"Error"
                                                    reason:[NSString stringWithFormat:@"%@",
                                                            [ClojureLangRT nthFromWithId:types withInt:n]] userInfo:nil];
        }
        args = [conj invokeWithId:args withId:val];
    }
    
    id v = [fn applyToWithClojureLangISeq:[ClojureLangRT seqWithId:args]];
    
    void * ret;
    
    switch (to_char(retType)) {
        case void_type: {
            return;
        }
        case float_type: {
            float o = [ClojureLangRT floatCastWithId:v];
            ret = &o;
            break;
        }
        case longlong_type: {
            long long o = [ClojureLangRT longCastWithId:v];
            ret = &o;
            break;
        }
        case long_type: {
            long o = (long)[ClojureLangRT longCastWithId:v];
            ret = &o;
            break;
        }
        case char_type: {
            char o = [ClojureLangRT charCastWithId:v];
            ret = &o;
            break;
        }
        case short_type: {
            short o = [ClojureLangRT shortCastWithId:v];
            ret = &o;
            break;
        }
        case int_type: {
            int o = [ClojureLangRT intCastWithId:v];
            ret = &o;
            break;
        }
        case double_type: {
            double o = [ClojureLangRT doubleCastWithId:v];
            ret = &o;
            break;
        }
        case ulong_type: {
            unsigned long o = (unsigned long)[ClojureLangRT longCastWithId:v];
            ret = &o;
            break;
        }
        case ulonglong_type: {
            unsigned long long o = [ClojureLangRT longCastWithId:v];
            ret = &o;
            break;
        }
        case uchar_type: {
            unsigned char o = [ClojureLangRT charCastWithId:v];
            ret = &o;
            break;
        }
        case ushort_type: {
            unsigned short o = [ClojureLangRT shortCastWithId:v];
            ret = &o;
            break;
        }
        case uint_type: {
            unsigned int o = [ClojureLangRT intCastWithId:v];
            ret = &o;
            break;
        }
        case bool_type: {
            BOOL o = [ClojureLangRT booleanCastWithId:v];
            ret = &o;
            break;
        }
        case id_type: {
            ret = &v;
            break;
        }
        case cgpoint_type: {
            CGPoint o = [((NSValue*) v) CGPointValue];
            ret = &o;
            break;
        }
        case nsrange_type: {
            NSRange o = [((NSValue*) v) rangeValue];
            ret = &o;
            break;
        }
        case uiedge_type: {
            UIEdgeInsets o = [((NSValue*) v) UIEdgeInsetsValue];
            ret = &o;
            break;
        }
        case cgsize_type: {
            CGSize o = [((NSValue*) v) CGSizeValue];
            ret = &o;
            break;
        }
        case cgafflinetransform_type: {
            CGAffineTransform o = [((NSValue*) v) CGAffineTransformValue];
            ret = &o;
            break;
        }
        case catransform3d_type: {
            CATransform3D o = [((NSValue*) v) CATransform3DValue];
            ret = &o;
            break;
        }
        case uioffset_type: {
            UIOffset o = [((NSValue*) v) UIOffsetValue];
            ret = &o;
            break;
        }
        case cgrect_type: {
            CGRect o = [((NSValue*) v) CGRectValue];
            ret = &o;
            break;
        }
        case pointer_type: {
            void* o = [((NSValue*) v) pointerValue];
            ret = &o;
            break;
        }
    }
    [invocation setReturnValue:ret];
}

+(const char*)makeSignature:(id)types {
    BOOL first = YES;
    NSString *s = @"";
    while (types != nil) {
        s = [s stringByAppendingString:[[[NSString alloc] initWithUTF8String:encode_type(to_char([ClojureLangRT firstWithId:types]))] autorelease]];
        if (first) {
            s = [s stringByAppendingString:@"@:"];
            first = NO;
        }
        types = [ClojureLangRT nextWithId:types];
    }
    return [s UTF8String];
}

// https://developer.apple.com/library/mac/documentation/cocoa/conceptual/ObjCRuntimeGuide/Articles/ocrtTypeEncodings.html
+(char) signatureToType:(const char*)c {
    switch (*c) {
        case _C_CONST:                        // const
        case 'n':                             // in
        case 'N':                             // inout
        case 'o':                             // out
        case 'O':                             // bycopy
        case 'R':                             // byref
        case 'V':                             // oneway
            c++;
            return [NSCommon signatureToType:c];
        case _C_FLT: return float_type;
        case _C_LNG_LNG: return longlong_type;
        case _C_LNG: return long_type;
        case _C_CHR: return char_type;
        case _C_SHT: return short_type;
        case _C_INT: return int_type;
        case _C_BOOL: return bool_type;
        case _C_DBL: return double_type;
        case _C_ULNG_LNG: return ulonglong_type;
        case _C_ULNG: return ulong_type;
        case _C_UCHR: return uchar_type;
        case _C_USHT: return ushort_type;
        case _C_UINT: return uint_type;
        case _C_VOID: return void_type;
        case _C_CHARPTR:
        case _C_SEL:
        case _C_PTR:
            return pointer_type;
        case _C_CLASS:
        case _C_ID:
        case _C_UNDEF:
            return id_type;
        case _C_STRUCT_B: {
            if (strcmp(c, @encode(CGPoint)) == 0) {
                return cgpoint_type;
            } else if (strcmp(c, @encode(NSRange)) == 0) {
                return nsrange_type;
            } else if (strcmp(c, @encode(UIEdgeInsets)) == 0) {
                return uiedge_type;
            } else if (strcmp(c, @encode(CGSize)) == 0) {
                return  cgsize_type;
            } else if (strcmp(c, @encode(CGAffineTransform)) == 0) {
                return cgafflinetransform_type;
            } else if (strcmp(c, @encode(CATransform3D)) == 0) {
                return catransform3d_type;
            } else if (strcmp(c, @encode(UIOffset)) == 0) {
                return uioffset_type;
            } else if (strcmp(c, @encode(CGRect)) == 0) {
                return cgrect_type;
            }
        }
    }
    @throw [NSException exceptionWithName:@"Type signature not found" reason:[NSString stringWithUTF8String:c] userInfo:nil];
}

+ (id) invokeSel:(id)object withSelector:(NSString*)selector withArgs:(id<ClojureLangISeq>)arguments {
#ifndef __arm64__
    NSString *s = (use_stret(object, selector) ? @"objc_msgSend_stret" : @"objc_msgSend");
#else
    NSString *s = @"objc_msgSend";
#endif
    return [NSCommon invokeFun:s withSelf:object withSelector:selector withArgs:arguments];
}

+ (id) invokeSuperSel:(id)object withSelector:(NSString*)selector withArgs:(id<ClojureLangISeq>)arguments {
    struct objc_super superData = {object, [object superclass]};
    SEL sel = NSSelectorFromString(selector);
    NSMethodSignature *sig = [object methodSignatureForSelector:sel];
    if (sig == nil) {
        @throw([NSException exceptionWithName:@"Error invoking superclass objc method. Selector not found" reason:selector userInfo:nil]);
    }
    id types = [assoc invokeWithId:[NSCommon signaturesToTypes:sig skipSel:NO] withId:[[[JavaLangInteger alloc] initWithInt:1] autorelease] withId:[[[JavaLangCharacter alloc] initWithChar:pointer_type] autorelease]];
    id args = [cons invokeWithId:[NSValue valueWithPointer:NSSelectorFromString(selector)] withId:arguments];
    args = [cons invokeWithId:[NSValue valueWithPointer:(void*)&superData] withId:args];
#ifndef __arm64__
    NSString *s = (use_stret(object, selector) ? @"objc_msgSendSuper_stret" : @"objc_msgSendSuper");
#else
    NSString *s = @"objc_msgSendSuper";
#endif
    return [NSCommon ccall:s types:types args:args];
}

+ (id) signaturesToTypes:(NSMethodSignature*)sig skipSel:(BOOL)skip {
    id types = [ClojureLangPersistentVector EMPTY];
    types = [conj invokeWithId:types withId:[[[JavaLangCharacter alloc] initWithChar:[NSCommon signatureToType:[sig methodReturnType]]] autorelease]];
    for (int n = 0; n < [sig numberOfArguments]; n++) {
        if (!skip || (n != 0 && n != 1)) {
            types = [conj invokeWithId:types withId:[[[JavaLangCharacter alloc] initWithChar:[NSCommon signatureToType:[sig getArgumentTypeAtIndex:n]]] autorelease]];
        }
    }
    return types;
}

+ (id) invokeFun:(NSString*)fun withSelf:(id)object withSelector:(NSString*)selector withArgs:(id<ClojureLangISeq>)arguments {
    SEL sel = NSSelectorFromString(selector);
    NSMethodSignature *sig = [([object isKindOfClass:[WeakRef class]] ? [(WeakRef*)object deref] : object) methodSignatureForSelector:sel];
    if (sig == nil) {
        @throw([NSException exceptionWithName:@"Error invoking objc method. Selector not found" reason:selector userInfo:nil]);
    }
    return [NSCommon invokeFun:fun withSig:sig withSelf:object withSelector:selector withArgs:arguments];
}

+ (id) invokeFun:(NSString*)fun withSig:(NSMethodSignature*)sig
        withSelf:(id)object
    withSelector:(NSString*)selector
        withArgs:(id<ClojureLangISeq>)arguments {
    id args = [cons invokeWithId:[NSValue valueWithPointer:NSSelectorFromString(selector)] withId:arguments];
    args = [cons invokeWithId:object withId:args];
    return [NSCommon ccall:fun types:[NSCommon signaturesToTypes:sig skipSel:NO] args:args];
}

@end
