//
//  NSTypeImpl.h
//  sample
//
//  Created by Gal Dolber on 2/4/14.
//  Copyright (c) 2014 clojure-objc. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "clojure/lang/APersistentMap.h"
#import "clojure/lang/PersistentVector.h"
#import "clojure/lang/PersistentHashMap.h"

@interface NSTypeImpl : NSObject

+(Class) makeClassWithName:(NSString*)name superclass:(NSString*)s map:(ClojureLangAPersistentMap*)m;

@end
