package clojure.lang;


/*-[ 
#import "java/lang/Character.h"
#import "java/lang/Boolean.h"
#import <objc/runtime.h>
#import <UIKit/UIKit.h>
 ]-*/

public class Selector extends RestFn implements Named {

  public final String sel;

  public Selector(Symbol sel) {
    this.sel = sel.name;
  }
  
  public Selector(String sel) {
    this.sel = sel;
  }

  @Override
  public String getNamespace() {
    return null;
  }

  @Override
  public String getName() {
    return sel;
  }
  
  @Override
  protected Object doInvoke(Object o, Object args) {
    if (!RT.ios) {
      System.out.println("Warning! objc selectors always return nil on the jvm");
      return null;
    } else {
      String sel = this.sel;
      if (args != null && !sel.endsWith(":")) {
        sel = sel + ":";
      }
      return invokeObjc(o, sel, RT.seq(args));
    }
  }
  
  public native Object invokeObjc(Object object, String selector,
      ISeq arguments) /*-[
  SEL selector_ = NSSelectorFromString(selector);
  BOOL isClass = class_isMetaClass(object_getClass(object));
  NSMethodSignature *methodSignature_ = isClass ? [object methodSignatureForSelector:selector_] : [[object class] instanceMethodSignatureForSelector:selector_];
  if (methodSignature_ == nil) {
    @throw([NSException exceptionWithName:@"Error invoking objc method. Selector not found" reason:selector userInfo:nil]);
  }
  NSInvocation *invocation = [NSInvocation invocationWithMethodSignature:methodSignature_];
  [invocation setSelector:selector_];
  
  if (arguments != nil) {
    int n = 2;
    while (arguments != nil) {
      const char *c = [methodSignature_ getArgumentTypeAtIndex:n];
      NSObject *val = [arguments first];
      NSString *cs = [NSString stringWithUTF8String:c];
      if ([val isKindOfClass:[ClojureLangSelector class]]) {
        SEL i = NSSelectorFromString([((ClojureLangSelector*)val) getName]);
        [invocation setArgument:&i atIndex:n];
      } else if ([cs isEqualToString:@"^{CGColor=}"]) {
        CGColorRef ref = (CGColorRef)val;
        [invocation setArgument:&ref atIndex:n];
      } else if ([cs isEqualToString:@"{CGPoint=ff}"]) {
        CGPoint point;
        [((NSValue*) val) getValue:&point];
        [invocation setArgument:&point atIndex:n];
      } else if ([cs isEqualToString:@"{UIEdgeInsets=ffff}"]) {
        UIEdgeInsets e;
        [((NSValue*) val) getValue:&e];
        [invocation setArgument:&e atIndex:n];
      } else if ([cs isEqualToString:@"{CGSize=ff}"]) {
        CGSize size;
        [((NSValue*) val) getValue:&size];
        [invocation setArgument:&size atIndex:n];
      } else if ([cs isEqualToString:@"{CGAffineTransform=ffffff}"]) {
        CGAffineTransform transform;
        [((NSValue*) val) getValue:&transform];
        [invocation setArgument:&transform atIndex:n];
      } else if ([cs isEqualToString:@"{CGRect={CGPoint=ff}{CGSize=ff}}"]) {
        CGRect rect;
        [((NSValue*) val) getValue:&rect];
        [invocation setArgument:&rect atIndex:n];
      } else if (*c == 'f') {
        float f = [ClojureLangRT floatCastWithId:val];
        [invocation setArgument:&f atIndex:n];
      } else if (*c == 'q') {
        long long i = [ClojureLangRT doubleCastWithId:val]; // TODO ??
        [invocation setArgument:&i atIndex:n];
      } else if (*c == 'l') {
        long i = [ClojureLangRT longCastWithId:val];
        [invocation setArgument:&i atIndex:n];
      } else if (*c == 'c') {
        BOOL i = [((JavaLangBoolean*) val) booleanValue];
        [invocation setArgument:&i atIndex:n];
      } else if (*c == 's') {
        short i = [ClojureLangRT shortCastWithId:val];
        [invocation setArgument:&i atIndex:n];
      } else if (*c == 'i') {
        int i = [ClojureLangRT intCastWithId:val];
        [invocation setArgument:&i atIndex:n];
      } else if (*c == 'd') {
        double f = [ClojureLangRT doubleCastWithId:val];
        [invocation setArgument:&f atIndex:n];
      } else if (*c == 'D') {
        double f = [ClojureLangRT doubleCastWithId:val];
        [invocation setArgument:&f atIndex:n];
      } else if (*c == 'Q') {
        long long i = [ClojureLangRT doubleCastWithId:val]; // TODO ??
        [invocation setArgument:&i atIndex:n];
      } else if (*c == 'L') {
        long i = [ClojureLangRT longCastWithId:val];
        [invocation setArgument:&i atIndex:n];
      } else if (*c == 'C') {
        char i = [ClojureLangRT charCastWithId:val];
        [invocation setArgument:&i atIndex:n];
      } else if (*c == 'S') {
        short i = [ClojureLangRT shortCastWithId:val];
        [invocation setArgument:&i atIndex:n];
      } else if (*c == 'I') {
        int i = [ClojureLangRT intCastWithId:val];
        [invocation setArgument:&i atIndex:n];
      } else {
        [invocation setArgument:&val atIndex:n];
      }
      arguments = [arguments next];
      n++;
    }
  }
  
  if (object == nil) {
    [invocation setTarget:[object class]];
  } else {
    [invocation setTarget:object];
  }
  
  [invocation invoke];
  const char *returnType = [methodSignature_ methodReturnType];
  NSString *retStr = [NSString stringWithUTF8String:returnType];
  if (*returnType == 'v' || [retStr isEqualToString: @"Vv"]) {
    return nil;
  } else if (*returnType == 'f') {
    float returnValue;
    [invocation getReturnValue:&returnValue];
    return @(returnValue);
  } else if (*returnType == 'q') {
    long returnValue;
    [invocation getReturnValue:&returnValue];
    return @(returnValue);
  } else if (*returnType == 'l') {
    long returnValue;
    [invocation getReturnValue:&returnValue];
    return @(returnValue);
  } else if (*returnType == 'c') {
    BOOL returnValue;
    [invocation getReturnValue:&returnValue];
    return returnValue ? @1 : nil;
  } else if (*returnType == 'i') {
    int returnValue;
    [invocation getReturnValue:&returnValue];
    return @(returnValue);
  } else if (*returnType == 'd') {
    double returnValue;
    [invocation getReturnValue:&returnValue];
    return @(returnValue);
  } else if (*returnType == 'F') {
    float returnValue;
    [invocation getReturnValue:&returnValue];
    return @(returnValue);
  } else if (*returnType == 'Q') {
    long returnValue;
    [invocation getReturnValue:&returnValue];
    return @(returnValue);
  } else if (*returnType == 'L') {
    long returnValue;
    [invocation getReturnValue:&returnValue];
    return @(returnValue);
  } else if (*returnType == 'C') { // ??
  char returnValue;
  [invocation getReturnValue:&returnValue];
  return [[JavaLangCharacter alloc] initWithChar:returnValue];
} else if (*returnType == 'I') {
  int returnValue;
  [invocation getReturnValue:&returnValue];
  return @(returnValue);
} else if (*returnType == 'D') {
  double returnValue;
  [invocation getReturnValue:&returnValue];
  return @(returnValue);
} else if ([retStr isEqualToString:@"{CGRect={CGPoint=ff}{CGSize=ff}}"]) {
  CGRect returnValue;
  [invocation getReturnValue:&returnValue];
  return [NSValue valueWithCGRect:returnValue];
} else if ([retStr isEqualToString:@"{CGPoint=ff}"]) {
  CGPoint returnValue;
  [invocation getReturnValue:&returnValue];
  return [NSValue valueWithCGPoint:returnValue];
} else if ([retStr isEqualToString:@"{CGSize=ff}"]) {
  CGSize returnValue;
  [invocation getReturnValue:&returnValue];
  return [NSValue valueWithCGSize:returnValue];
} else if ([retStr isEqualToString:@"^{CGColor=}"]) {
  CGColorRef returnValue;
  [invocation getReturnValue:&returnValue];
  return (id)returnValue;
} else if ([retStr isEqualToString:@"{UIEdgeInsets=ffff}"]) {
  UIEdgeInsets returnValue;
  [invocation getReturnValue:&returnValue];
  return [NSValue valueWithUIEdgeInsets:returnValue];
} else if ([retStr isEqualToString:@"{CGAffineTransform=ffffff}"]) {
  CGAffineTransform returnValue;
  [invocation getReturnValue:&returnValue];
  return [NSValue valueWithCGAffineTransform:returnValue];
} else if ([retStr isEqualToString:@"#"]) {
  return nil;
} else if ([retStr isEqualToString:@"@"]) {
  NSObject *returnValue;
  [invocation getReturnValue:&returnValue];
  return returnValue;
} else {
  @throw [NSException exceptionWithName:@"Type not found" reason:retStr userInfo:nil];
}
    ]-*/;

  @Override
  public int getRequiredArity() {
    return 1;
  }
}
