#import "NSProxyImpl.h"
#import "clojure/lang/AFn.h"
#import "clojure/lang/RT.h"
#import "clojure/lang/PersistentVector.h"
#import "clojure/lang/Selector.h"
#import "clojure/lang/Var.h"
#import "NSCommon.h"
#import <UIKit/UIKit.h>

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

- (void) forwardInvocation:(NSInvocation *)invocation
{
    id r = [map valAtWithId:NSStringFromSelector([invocation selector])];
    if (r == nil) {
        [invocation invokeWithTarget:instance];
    } else {
        [NSCommon callWithInvocation:invocation withSelf:self withTypes:[ClojureLangRT firstWithId:r] withFn:[ClojureLangRT secondWithId:r]];
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
        return [NSMethodSignature signatureWithObjCTypes:[NSCommon makeSignature:[ClojureLangRT firstWithId:r]]];
    }
}

-(BOOL)respondsToSelector:(SEL)sel {
    if ([map valAtWithId:NSStringFromSelector(sel)] != nil || [instance respondsToSelector:sel]) {
        return YES;
    }
    return NO;
}


-(id)retain {
    id r = [map valAtWithId:@"retain"];
    if (r != nil) {
        [(ClojureLangAFn*)[ClojureLangRT secondWithId:r] invokeWithId:self];
    }
    return [super retain];
}

-(oneway void)release {
    id r = [map valAtWithId:@"release"];
    if (r != nil) {
        [(ClojureLangAFn*)[ClojureLangRT secondWithId:r] invokeWithId:self];
    }
    [super release];
}

-(NSUInteger)retainCount {
    id r = [map valAtWithId:@"retainCount"];
    if (r != nil) {
        [(ClojureLangAFn*)[ClojureLangRT secondWithId:r] invokeWithId:self];
    }
    return [super retainCount];
}

-(void)dealloc {
    id r = [map valAtWithId:@"dealloc"];
    if (r != nil) {
        [(ClojureLangAFn*)[ClojureLangRT secondWithId:r] invokeWithId:self];
    }
    [instance release];
    [map release];
    [super dealloc];
}

-(NSString*)description {
    id r = [map valAtWithId:@"description"];
    if (r != nil) {
        return [(ClojureLangAFn*)[ClojureLangRT secondWithId:r] invokeWithId:self];
    } else {
        return [instance description];
    }
}

@end
