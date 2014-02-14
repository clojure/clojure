//
//  WeakRef.h
//  sample
//
//  Created by Gal Dolber on 2/14/14.
//  Copyright (c) 2014 clojure-objc. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface WeakRef : NSObject

-(id)initWith:(id)o;

-(id)deref;

@end
