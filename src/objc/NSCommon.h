//
//  NSCommon.h
//  sample
//
//  Created by Gal Dolber on 2/4/14.
//  Copyright (c) 2014 clojure-objc. All rights reserved.
//

// Supported types
// -------------------
// float
// long long
// long
// char
// short
// int
// double
// unsigned long long
// unsigned long
// unsigned char
// unsigned short
// unsigned int
// bool
// CGPoint
// NSRange
// UIEdgeInsets
// CGSize
// CGAffineTransform
// CATransform3D
// UIOffset
// CGRect
// id
// void

#import <Foundation/Foundation.h>
#import "clojure/lang/AFn.h"

#define to_char(c)\
[ClojureLangRT charCastWithId:c]\

static const char void_type = 'v';
static const char float_type = 'f';
static const char longlong_type = 'q';
static const char long_type = 'l';
static const char char_type = 'c';
static const char short_type = 's';
static const char int_type = 'i';
static const char double_type = 'd';
static const char ulonglong_type = 'Q';
static const char ulong_type = 'L';
static const char uchar_type = 'C';
static const char ushort_type = 'S';
static const char uint_type = 'I';
static const char bool_type = 'b';
static const char cgpoint_type = 'P';
static const char nsrange_type = 'N';
static const char uiedge_type = 'E';
static const char cgsize_type = 'Z';
static const char cgafflinetransform_type = 'A';
static const char catransform3d_type = 'T';
static const char uioffset_type = 'O';
static const char cgrect_type = 'R';
static const char id_type = 'p';
static const char pointer_type = 'Y';

#define reg_c(f) \
    [NSCommon registerCFunction:@ #f fn:f];\

@interface NSCommon : NSObject

+(BOOL)cgfloatIsDouble;

+(void)registerCFunction:(NSString*)name fn:(void*)fn;

+(id)ccall:(id)name types:(id)types args:(id)args;

+(const char*)makeSignature:(id)types;

+(void)callWithInvocation:(NSInvocation *)invocation withSelf:(id)sself withTypes:(id)types withFn:(ClojureLangAFn*)fn;

+(char)signatureToType:(const char*)c;

+(id)signaturesToTypes:(NSMethodSignature*)sig skipSel:(BOOL)skip;

+(id)invokeFun:(NSString*)fun withSelf:(id)object withSelector:(NSString*)selector withArgs:(id<ClojureLangISeq>)arguments;

+(id)invokeSel:(id)object withSelector:(NSString*)selector withArgs:(id<ClojureLangISeq>)arguments;

+(id)invokeSuperSel:(id)object withSelector:(NSString*)selector withArgs:(id<ClojureLangISeq>)arguments;

@end
