//
//  NSSocketImpl.h
//  sample
//
//  Created by Gal Dolber on 2/1/14.
//  Copyright (c) 2014 clojure-objc. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface NSSocketImpl : NSObject

- (id) initWithHost:(NSString*)hostName withPort:(NSString*)portNum;

- (id) read;

- (void) println: (NSString*) s;

-(void)close;

@end
