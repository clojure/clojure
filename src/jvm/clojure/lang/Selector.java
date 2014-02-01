package clojure.lang;


/*-[ 
#import "java/lang/Character.h"
#import "java/lang/Boolean.h"
#import "java/lang/Integer.h"
#import "java/lang/Double.h"
#import "java/lang/Float.h"
#import "java/lang/Long.h"
#import "java/lang/Short.h"
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
    if (!ObjC.objc) {
      if (RemoteRepl.connected) { 
        return RemoteRepl.callRemote(this, RT.cons(o, args));
      } else {
        return null;
      }
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
    NSMethodSignature *methodSignature_ = [object methodSignatureForSelector:selector_];
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
            if ([val isKindOfClass:[ClojureLangSelector class]]) {
                SEL i = NSSelectorFromString([((ClojureLangSelector*)val) getName]);
                [invocation setArgument:&i atIndex:n];
            } else {
skipArgModifiers:
                switch (*c) {
                    case 'r':                             // const
                    case 'n':                             // in
                    case 'N':                             // inout
                    case 'o':                             // out
                    case 'O':                             // bycopy
                    case 'R':                             // byref
                    case 'V':                             // oneway
                        c++;
                        goto skipArgModifiers;
                    case 'f': {
                        float f = [ClojureLangRT floatCastWithId:val];
                        [invocation setArgument:&f atIndex:n];
                        break;
                    }
                    case 'q': {
                        long long i = [ClojureLangRT doubleCastWithId:val];
                        [invocation setArgument:&i atIndex:n];
                        break;
                    }
                    case 'l': {
                        long long i = [ClojureLangRT longCastWithId:val];
                        [invocation setArgument:&i atIndex:n];
                        break;
                    }
                    case 'c': {
                        char i;
                        if ([val isKindOfClass: [JavaLangBoolean class]]) {
                            i = [(JavaLangBoolean*) val booleanValue];
                        } else {
                            i = [((JavaLangCharacter*) val) charValue];
                        }
                        [invocation setArgument:&i atIndex:n];
                        break;
                    }
                    case 's': {
                        short i = [ClojureLangRT shortCastWithId:val];
                        [invocation setArgument:&i atIndex:n];
                        break;
                    }
                    case 'i': {
                        int i = [ClojureLangRT intCastWithId:val];
                        [invocation setArgument:&i atIndex:n];
                        break;
                    }
                    case 'D':
                    case 'd': {
                        double f = [ClojureLangRT doubleCastWithId:val];
                        [invocation setArgument:&f atIndex:n];
                        break;
                    }
                    case 'Q': {
                        unsigned long long i = [ClojureLangRT doubleCastWithId:val];
                        [invocation setArgument:&i atIndex:n];
                        break;
                    }
                    case 'L': {
                        unsigned long long i = [ClojureLangRT longCastWithId:val];
                        [invocation setArgument:&i atIndex:n];
                        break;
                    }
                    case 'C': {
                        unsigned char i = [ClojureLangRT charCastWithId:val];
                        [invocation setArgument:&i atIndex:n];
                        break;
                    }
                    case 'S': {
                        unsigned short i = [ClojureLangRT shortCastWithId:val];
                        [invocation setArgument:&i atIndex:n];
                        break;
                    }
                    case 'I': {
                        unsigned int i = [ClojureLangRT intCastWithId:val];
                        [invocation setArgument:&i atIndex:n];
                        break;
                    }
                    case 'B': {
                        BOOL i = [ClojureLangRT booleanCastWithId:val];
                        [invocation setArgument:&i atIndex:n];
                        break;
                    }
                    case '@':
                    case '#':
                    case '*':
                    case ':':
                    case '^': {
                        [invocation setArgument:&val atIndex:n];
                        break;
                    }
                    case '{': {
                        // {_NSSize {_NSPoint {_NSRect ?
                        NSString *cs = [NSString stringWithUTF8String:c];
                        if ([cs hasPrefix:@"{CGPoint"]) {
                            CGPoint point;
                            [((NSValue*) val) getValue:&point];
                            [invocation setArgument:&point atIndex:n];
                        } else if ([cs hasPrefix:@"{_NSRange"]) {
                            NSRange e;
                            [((NSValue*) val) getValue:&e];
                            [invocation setArgument:&e atIndex:n];
                        } else if ([cs hasPrefix:@"{UIEdgeInsets"]) {
                            UIEdgeInsets e;
                            [((NSValue*) val) getValue:&e];
                            [invocation setArgument:&e atIndex:n];
                        } else if ([cs hasPrefix:@"{CGSize"]) {
                            CGSize size;
                            [((NSValue*) val) getValue:&size];
                            [invocation setArgument:&size atIndex:n];
                        } else if ([cs hasPrefix:@"{CGAffineTransform"]) {
                            CGAffineTransform transform;
                            [((NSValue*) val) getValue:&transform];
                            [invocation setArgument:&transform atIndex:n];
                        } else if ([cs hasPrefix:@"{CATransform3D"]) {
                            CATransform3D transform;
                            [((NSValue*) val) getValue:&transform];
                            [invocation setArgument:&transform atIndex:n];
                        } else if ([cs hasPrefix:@"{UIOffset"]) {
                            UIOffset off;
                            [((NSValue*) val) getValue:&off];
                            [invocation setArgument:&off atIndex:n];
                        } else if ([cs hasPrefix:@"{CGRect"]) {
                            CGRect rect;
                            [((NSValue*) val) getValue:&rect];
                            [invocation setArgument:&rect atIndex:n];
                        } else {
                            @throw [NSException exceptionWithName:@"Type not found" reason:cs userInfo:nil];
                        }
                        break;
                    }
                    default: {
                        @throw [NSException exceptionWithName:@"Type not found" reason:[NSString stringWithUTF8String:c] userInfo:nil];
                    }
                }
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
    
skipReturnModifiers:
    switch (*returnType) {
        case 'r':                             // const
        case 'n':                             // in
        case 'N':                             // inout
        case 'o':                             // out
        case 'O':                             // bycopy
        case 'R':                             // byref
        case 'V':                             // oneway
            returnType++;
            goto skipReturnModifiers;
        case 'v': {
            return nil;
        }
        case 'F':
        case 'f': {
            float returnValue;
            [invocation getReturnValue:&returnValue];
            return [[JavaLangFloat alloc] initWithFloat:returnValue];
        }
        case 'q': {
            long long returnValue;
            [invocation getReturnValue:&returnValue];
            return [[JavaLangLong alloc] initWithLong:returnValue];
        }
        case 'l': {
            long returnValue;
            [invocation getReturnValue:&returnValue];
            return [[JavaLangLong alloc] initWithLong:returnValue];
        }
        case 's': {
            short returnValue;
            [invocation getReturnValue:&returnValue];
            return [[JavaLangShort alloc] initWithShort:returnValue];
        }
        case 'S': {
            unsigned short returnValue;
            [invocation getReturnValue:&returnValue];
            return [[JavaLangShort alloc] initWithShort:returnValue];
        }
        case 'c': {
            char returnValue;
            [invocation getReturnValue:&returnValue];
            if (returnValue == YES) {
                return [JavaLangBoolean getTRUE];
            } else if (returnValue == NO) {
                return [JavaLangBoolean getFALSE];
            } else {
                return [[JavaLangCharacter alloc] initWithChar:returnValue];
            }
        }
        case 'i': {
            int returnValue;
            [invocation getReturnValue:&returnValue];
            return [[JavaLangInteger alloc] initWithInt:returnValue];
        }
        case 'D':
        case 'd': {
            double returnValue;
            [invocation getReturnValue:&returnValue];
            return [[JavaLangDouble alloc] initWithDouble:returnValue];
        }
        case 'B': {
            BOOL returnValue;
            [invocation getReturnValue:&returnValue];
            return returnValue ? [JavaLangBoolean getTRUE] : [JavaLangBoolean getFALSE];
        }
        case 'Q': {
            unsigned long long returnValue;
            [invocation getReturnValue:&returnValue];
            return [[JavaLangLong alloc] initWithLong:returnValue];
        }
        case 'L': {
            unsigned long returnValue;
            [invocation getReturnValue:&returnValue];
            return [[JavaLangLong alloc] initWithLong:returnValue];
        }
        case 'C': {
            unsigned char returnValue;
            [invocation getReturnValue:&returnValue];
            return [[JavaLangCharacter alloc] initWithChar:returnValue];
        }
        case 'I': {
            unsigned int returnValue;
            [invocation getReturnValue:&returnValue];
            return [[JavaLangInteger alloc] initWithInt:returnValue];
        }
        case '@':
        case '#':
        case '*':
        case ':':
        case '^': {
            NSObject *returnValue;
            [invocation getReturnValue:&returnValue];
            return returnValue;
        }
        case '{': {
            // {_NSSize {_NSPoint {_NSRect ?
            NSString *cs = [NSString stringWithUTF8String:returnType];
            if ([cs hasPrefix:@"{CGPoint"]) {
                CGPoint returnValue;
                [invocation getReturnValue:&returnValue];
                return [NSValue valueWithCGPoint:returnValue];
            } else if ([cs hasPrefix:@"{_NSRange"]) {
                NSRange e;
                [invocation getReturnValue:&e];
                return [NSValue valueWithRange:e];
            } else if ([cs hasPrefix:@"{UIEdgeInsets"]) {
                UIEdgeInsets returnValue;
                [invocation getReturnValue:&returnValue];
                return [NSValue valueWithUIEdgeInsets:returnValue];
            } else if ([cs hasPrefix:@"{CGSize"]) {
                CGSize returnValue;
                [invocation getReturnValue:&returnValue];
                return [NSValue valueWithCGSize:returnValue];
            } else if ([cs hasPrefix:@"{CGAffineTransform"]) {
                CGAffineTransform returnValue;
                [invocation getReturnValue:&returnValue];
                return [NSValue valueWithCGAffineTransform:returnValue];
            } else if ([cs hasPrefix:@"{CATransform3D"]) {
                CATransform3D returnValue;
                [invocation getReturnValue:&returnValue];
                return [NSValue valueWithCATransform3D:returnValue];
            } else if ([cs hasPrefix:@"{UIOffset"]) {
                UIOffset returnValue;
                [invocation getReturnValue:&returnValue];
                return [NSValue valueWithUIOffset:returnValue];
            } else if ([cs hasPrefix:@"{CGRect"]) {
                CGRect returnValue;
                [invocation getReturnValue:&returnValue];
                return [NSValue valueWithCGRect:returnValue];
            } else {
                @throw [NSException exceptionWithName:@"Type not found" reason:cs userInfo:nil];
            }
            break;
        }
    }
    @throw [NSException exceptionWithName:@"Type not found" reason:[NSString stringWithUTF8String:returnType] userInfo:nil];
  ]-*/;

  @Override
  public int getRequiredArity() {
    return 1;
  }
}
