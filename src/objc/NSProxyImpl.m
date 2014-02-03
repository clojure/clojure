#import "NSProxyImpl.h"
#import "clojure/lang/AFn.h"
#import "clojure/lang/RT.h"
#import "clojure/lang/PersistentVector.h"
#import "clojure/lang/Selector.h"
#import "clojure/lang/Var.h"
#import "java/lang/Character.h"
#import "java/lang/Boolean.h"
#import "java/lang/Integer.h"
#import "java/lang/Double.h"
#import "java/lang/Float.h"
#import "java/lang/Long.h"
#import "java/lang/Short.h"
#import <UIKit/UIKit.h>

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

@implementation NSProxyImpl {
    ClojureLangAPersistentMap *map;
    Class clazz;
    id instance;
}

- (id) initWithClass:(NSString*)c map:(ClojureLangAPersistentMap*) m
{
    self = [super init];
    if (self) {
        map = [m retain];
        if (c == nil) {
            clazz = [NSObject class];
        } else {
            clazz = NSClassFromString(c);
        }
        instance = [[clazz alloc] init];
    }
    return self;
}

- (void) forwardInvocation:(NSInvocation *)invocation;
{
    id r = [map valAtWithId:NSStringFromSelector([invocation selector])];
    if (r == nil) {
        [invocation invokeWithTarget:instance];
    } else {
        id types = [ClojureLangRT firstWithId:r];
        NSString *retType = [ClojureLangRT firstWithId: types];
        types = [ClojureLangRT nextWithId:types];
        id args = [ClojureLangPersistentVector EMPTY];
        for (int n = 0; n < [ClojureLangRT countFromWithId:types]; n++) {
            NSString *f = [ClojureLangRT firstWithId:types];
            id val = nil;
            if ([f isEqualToString:@"float"]) {
                float v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [[JavaLangFloat alloc] initWithFloat:v];
            } else if ([f isEqualToString:@"long long"]) {
                long long v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [[JavaLangLong alloc] initWithLong:v];
            } else if ([f isEqualToString:@"long"]) {
                long v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [[JavaLangLong alloc] initWithLong:v];
            } else if ([f isEqualToString:@"char"]) {
                char v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [[JavaLangCharacter alloc] initWithChar:v];
            } else if ([f isEqualToString:@"short"]) {
                short v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [[JavaLangShort alloc] initWithShort:v];
            } else if ([f isEqualToString:@"int"]) {
                int v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [[JavaLangInteger alloc] initWithInt:v];
            } else if ([f isEqualToString:@"double"]) {
                double v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [[JavaLangDouble alloc] initWithDouble:v];
            } else if ([f isEqualToString:@"unsigned long long"]) {
                unsigned long long v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [[JavaLangLong alloc] initWithLong:v];
            } else if ([f isEqualToString:@"unsigned long"]) {
                unsigned long v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [[JavaLangLong alloc] initWithLong:v];
            } else if ([f isEqualToString:@"unsigned char"]) {
                unsigned char v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [[JavaLangCharacter alloc] initWithChar:v];
            } else if ([f isEqualToString:@"unsigned short"]) {
                unsigned short v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [[JavaLangShort alloc] initWithShort:v];
            } else if ([f isEqualToString:@"unsigned int"]) {
                unsigned int v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [[JavaLangInteger alloc] initWithInt:v];
            } else if ([f isEqualToString:@"bool"]) {
                char v;
                [invocation getArgument:&v atIndex: 2 + n];
                if (v == YES) {
                    val = [JavaLangBoolean getTRUE];
                } else {
                    val = [JavaLangBoolean getFALSE];
                }
            } else if ([f isEqualToString:@"CGPoint"]) {
                CGPoint v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [NSValue valueWithCGPoint:v];
            } else if ([f isEqualToString:@"NSRange"]) {
                NSRange v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [NSValue valueWithRange:v];
            } else if ([f isEqualToString:@"UIEdgeInsets"]) {
                UIEdgeInsets v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [NSValue valueWithUIEdgeInsets:v];
            } else if ([f isEqualToString:@"CGSize"]) {
                CGSize v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [NSValue valueWithCGSize:v];
            } else if ([f isEqualToString:@"CGAffineTransform"]) {
                CGAffineTransform v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [NSValue valueWithCGAffineTransform:v];
            } else if ([f isEqualToString:@"CATransform3D"]) {
                CATransform3D v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [NSValue valueWithCATransform3D:v];
            } else if ([f isEqualToString:@"UIOffset"]) {
                UIOffset v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [NSValue valueWithUIOffset:v];
            } else if ([f isEqualToString:@"CGRect"]) {
                CGRect v;
                [invocation getArgument:&v atIndex: 2 + n];
                val = [NSValue valueWithCGRect:v];
            } else {
                [invocation getArgument:&val atIndex: 2 + n];
            }
            args = [ClojureLangRT conjWithClojureLangIPersistentCollection:args withId:val];
        }
        
        id v = [[ClojureLangRT secondWithId:r] applyToWithClojureLangISeq:[ClojureLangRT seqWithId:args]];
        void * ret;
        if ([retType isEqualToString:@"void"]) {
            id v = nil;
            ret = &v;
        } else if ([retType isEqualToString:@"float"]) {
            float o = [((JavaLangFloat*) v) floatValue];
            ret = &o;
        } else if ([retType isEqualToString:@"long long"]) {
            long long o = [ClojureLangRT longCastWithId:v];
            ret = &o;
        } else if ([retType isEqualToString:@"long"]) {
            long long o = [ClojureLangRT longCastWithId:v];
            ret = &o;
        } else if ([retType isEqualToString:@"char"]) {
            char o = [ClojureLangRT charCastWithId:v];
            ret = &o;
        } else if ([retType isEqualToString:@"short"]) {
            short o = [ClojureLangRT shortCastWithId:v];
            ret = &o;
        } else if ([retType isEqualToString:@"int"]) {
            int o = [ClojureLangRT intCastWithId:v];
            ret = &o;
        } else if ([retType isEqualToString:@"double"]) {
            double o = [ClojureLangRT doubleCastWithId:v];
            ret = &o;
        } else if ([retType isEqualToString:@"unsigned long long"]) {
            unsigned long long o = [ClojureLangRT longCastWithId:v];
            ret = &o;
        } else if ([retType isEqualToString:@"unsigned long"]) {
            unsigned long long o = [ClojureLangRT longCastWithId:v];
            ret = &o;
        } else if ([retType isEqualToString:@"unsigned char"]) {
            unsigned char o = [ClojureLangRT charCastWithId:v];
            ret = &o;
        } else if ([retType isEqualToString:@"unsigned short"]) {
            unsigned short o = [ClojureLangRT shortCastWithId:v];
            ret = &o;
        } else if ([retType isEqualToString:@"unsigned int"]) {
            unsigned int o = [ClojureLangRT intCastWithId:v];
            ret = &o;
        } else if ([retType isEqualToString:@"bool"]) {
            BOOL o = [ClojureLangRT booleanCastWithId:v];
            ret = &o;
        } else if ([retType isEqualToString:@"CGPoint"]) {
            CGPoint o = [((NSValue*) v) CGPointValue];
            ret = &o;
        } else if ([retType isEqualToString:@"NSRange"]) {
            NSRange o = [((NSValue*) v) rangeValue];
            ret = &o;
        } else if ([retType isEqualToString:@"UIEdgeInsets"]) {
            UIEdgeInsets o = [((NSValue*) v) UIEdgeInsetsValue];
            ret = &o;
        } else if ([retType isEqualToString:@"CGSize"]) {
            CGSize o = [((NSValue*) v) CGSizeValue];
            ret = &o;
        } else if ([retType isEqualToString:@"CGAffineTransform"]) {
            CGAffineTransform o = [((NSValue*) v) CGAffineTransformValue];
            ret = &o;
        } else if ([retType isEqualToString:@"CATransform3D"]) {
            CATransform3D o = [((NSValue*) v) CATransform3DValue];
            ret = &o;
        } else if ([retType isEqualToString:@"UIOffset"]) {
            UIOffset o = [((NSValue*) v) UIOffsetValue];
            ret = &o;
        } else if ([retType isEqualToString:@"CGRect"]) {
            CGRect o = [((NSValue*) v) CGRectValue];
            ret = &o;
        } else {
            ret = &v;
        }
        [invocation setReturnValue:ret];
    }
}

-(NSMethodSignature *)methodSignatureForSelector:(SEL)sel {
    if ([NSStringFromSelector(sel) isEqualToString: @"initWithClass:map:"] ||
        [NSStringFromSelector(sel) isEqualToString: @"class"] ||
        [NSStringFromSelector(sel) isEqualToString: @"alloc"] ||
        [NSStringFromSelector(sel) isEqualToString: @"retain"] ||
        [NSStringFromSelector(sel) isEqualToString: @"retainCount"] ||
        [NSStringFromSelector(sel) isEqualToString: @"release"] ||
        [NSStringFromSelector(sel) isEqualToString: @"dealloc"]) {
        return [NSProxyImpl instanceMethodSignatureForSelector:sel];
    }
    id r = [map valAtWithId:NSStringFromSelector(sel)];
    if (r == nil) {
        return [clazz instanceMethodSignatureForSelector:sel];
    } else {
        const char * types = [self makeSignature:[ClojureLangRT firstWithId:r]];
        NSMethodSignature *s = [NSMethodSignature signatureWithObjCTypes:types];
        return s;
    }
}

-(BOOL)respondsToSelector:(SEL)sel {
    if ([map valAtWithId:NSStringFromSelector(sel)] != nil) {
        return YES;
    } else if ([instance respondsToSelector:sel]) {
        return YES;
    }
    return NO;
}

-(const char*) makeSignature:(id) types {
    BOOL first = YES;
    NSString *s = @"";
    for (; types != nil; types = [ClojureLangRT nextWithId:types]) {
        NSString *f = [ClojureLangRT firstWithId:types];
        NSString *a = @"@";
        if ([f isEqualToString:@"float"]) {
            a = @"f";
        } else if ([f isEqualToString:@"long long"]) {
            a = @"q";
        } else if ([f isEqualToString:@"long"]) {
            a = @"l";
        } else if ([f isEqualToString:@"char"]) {
            a = @"c";
        } else if ([f isEqualToString:@"short"]) {
            a = @"s";
        } else if ([f isEqualToString:@"int"]) {
            a = @"i";
        } else if ([f isEqualToString:@"double"]) {
            a = @"d";
        } else if ([f isEqualToString:@"unsigned long long"]) {
            a = @"Q";
        } else if ([f isEqualToString:@"unsigned long"]) {
            a = @"L";
        } else if ([f isEqualToString:@"unsigned char"]) {
            a = @"C";
        } else if ([f isEqualToString:@"unsigned short"]) {
            a = @"S";
        } else if ([f isEqualToString:@"unsigned int"]) {
            a = @"I";
        } else if ([f isEqualToString:@"bool"]) {
            a = @"c";
        } else if ([f isEqualToString:@"CGPoint"]) {
#ifdef __x86_64__
            a = @"{CGPoint=dd}";
#else
            a = @"{CGPoint=ff}";
#endif
        } else if ([f isEqualToString:@"NSRange"]) {
#ifdef __x86_64__
            a = @"{_NSRange=QQ}";
#else
            a = @"{_NSRange=II}";
#endif
        } else if ([f isEqualToString:@"UIEdgeInsets"]) {
#ifdef __x86_64__
            a = @"{UIEdgeInsets=dddd}";
#else
            a = @"{UIEdgeInsets=ffff}";
#endif
        } else if ([f isEqualToString:@"CGSize"]) {
#ifdef __x86_64__
            a = @"{CGSize=dd}";
#else
            a = @"{CGSize=ff}";
#endif
        } else if ([f isEqualToString:@"CGAffineTransform"]) {
#ifdef __x86_64__
            a = @"{CGAffineTransform=dddddd}";
#else
            a = @"{CGAffineTransform=ffffff}";
#endif
        } else if ([f isEqualToString:@"CATransform3D"]) {
#ifdef __x86_64__
            a = @"{CATransform3D=dddddddddddddddd}";
#else
            a = @"{CATransform3D=ffffffffffffffff}";
#endif
        } else if ([f isEqualToString:@"UIOffset"]) {
#ifdef __x86_64__
            a = @"{UIOffset=dd}";
#else
            a = @"{UIOffset=ff}";
#endif
        } else if ([f isEqualToString:@"CGRect"]) {
#ifdef __x86_64__
            a = @"{CGRect={CGPoint=dd}{CGSize=dd}}";
#else
            a = @"{CGRect={CGPoint=ff}{CGSize=ff}}";
#endif
        }
        s = [s stringByAppendingString:a];
        if (first) {
            s = [s stringByAppendingString:@"@:"];
            first = NO;
        }
    }
    return [s UTF8String];
}

-(id)retain {
    id r = [map valAtWithId:@"retain"];
    if (r != nil) {
        [(ClojureLangAFn*)[ClojureLangRT secondWithId:r] invoke];
    }
    return [super retain];
}

-(oneway void)release {
    id r = [map valAtWithId:@"release"];
    if (r != nil) {
        [(ClojureLangAFn*)[ClojureLangRT secondWithId:r] invoke];
    }
    [super release];
}

-(NSUInteger)retainCount {
    id r = [map valAtWithId:@"retainCount"];
    if (r != nil) {
        [(ClojureLangAFn*)[ClojureLangRT secondWithId:r] invoke];
    }
    return [super retainCount];
}

-(void)dealloc {
    id r = [map valAtWithId:@"dealloc"];
    if (r != nil) {
        [(ClojureLangAFn*)[ClojureLangRT secondWithId:r] invoke];
    }
    [instance release];
    [map release];
    [super dealloc];
}

-(NSString*)description {
    id r = [map valAtWithId:@"description"];
    if (r != nil) {
        return [(ClojureLangAFn*)[ClojureLangRT secondWithId:r] invoke];
    } else {
        return [instance description];
    }
}

@end
