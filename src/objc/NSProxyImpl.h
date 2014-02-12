#import <Foundation/Foundation.h>
#import "clojure/lang/APersistentMap.h"

@interface NSProxyImpl: NSObject

- (id) initWithClass:(NSString*)clazz map:(ClojureLangAPersistentMap*) m;

@end
