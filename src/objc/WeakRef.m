//
//  WeakRef.m
//  sample
//
//  Created by Gal Dolber on 2/14/14.
//  Copyright (c) 2014 clojure-objc. All rights reserved.
//

#import "WeakRef.h"

@implementation WeakRef {
    NSValue *val;
}

-(id)initWith:(id)o {
    self = [super init];
    if (self) {
        val = [NSValue valueWithNonretainedObject:o];
    }
    return self;
}

-(id)deref {
    return [val nonretainedObjectValue];
}

@end
