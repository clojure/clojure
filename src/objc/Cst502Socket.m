#import "Cst502Socket.h"
#define PORT "4444"

/**
 * Cst502Socket.m - objective-c class for manipulating stream sockets.
 * Purpose: demonstrate stream sockets in Objective-C.
 * These examples are buildable on MacOSX and GNUstep on top of Windows7
 * Cst502 Emerging Languages and Programming Technologies
 * See http://pooh.poly.asu.edu/Cst502
 * @author Tim Lindquist (Tim.Lindquist@asu.edu), ASU Polytechnic, Engineering
 * based on the simple server and client sockets in C by Jeez.
 * @version December 2011
 */

// get sockaddr, IPv4 or IPv6:
void *get_in_addr(struct sockaddr *sa){
    if (sa->sa_family == AF_INET) {
        return &(((struct sockaddr_in*)sa)->sin_addr);
    }
    return &(((struct sockaddr_in6*)sa)->sin6_addr);
}

@implementation Cst502ServerSocket

- (id) initWithPort: (NSString*) port{
   self = [super init];
   int ret = 0;
   memset(&hints, 0, sizeof hints);
   hints.ai_family = AF_INET;
   hints.ai_socktype = SOCK_STREAM;
   hints.ai_flags = AI_PASSIVE; // use my IP
   const char* portStr = [port UTF8String];
   if ((rv = getaddrinfo(NULL, portStr, &hints, &servinfo)) != 0) {
      fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(rv));
      ret = 1;
   }else{
      for(p = servinfo; p != NULL; p = p->ai_next) {
         if ((sockfd = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP))==-1){
            perror("server: socket create error");
            continue;
         }
         if (bind(sockfd, p->ai_addr, p->ai_addrlen) == -1) {
#if defined(WINGS)
         closesocket(sockfd);
#else
         close(sockfd);
#endif
            perror("server: bind error");
            continue;
         }
         break;
      }
      if (p == NULL)  {
         fprintf(stderr, "server: failed to bind\n");
         ret = 2;
      }else{
         freeaddrinfo(servinfo); // all done with this structure
         if (listen(sockfd, BACKLOG) == -1) {
            perror("server: listen error");
            ret = 3;
         }
      }
      if (ret == 0){
         return self;
      }
   }
   return nil;
}

- (BOOL) accept {
   BOOL ret = YES;
#if defined(WINGS)
   new_fd = accept(sockfd, NULL, NULL);
#else
   new_fd = accept(sockfd, (struct sockaddr *)&their_addr, &sin_size);
#endif
   if (new_fd == -1) {
      perror("server: accept error");
      ret = NO;
   }
   connected = ret;
   return ret;
}

- (int) sendBytes: (char*) byteMsg OfLength: (int) msgLength Index: (int) at{
   int ret = send(new_fd, byteMsg, msgLength, 0);
   if(ret == -1){
      NSLog(@"error sending bytes");
   }
   return ret;
}

- (NSString* ) receiveBytes: (char*) byteMsg
                   maxBytes: (int) max
                    beginAt: (int) at {
   int ret = recv(new_fd, byteMsg, max-1, at);
   if(ret == -1){
      NSLog(@"server error receiving bytes");
   }
   byteMsg[ret+at] = '\0';
   NSString * retStr = [NSString stringWithUTF8String: byteMsg];
   return retStr;
}

- (BOOL) close{
#if defined(WINGS)
   closesocket(new_fd);
#else
   close(new_fd);
#endif
   connected = NO;
   return YES;
}

- (void) dealloc {
#if defined(WINGS)
   closesocket(sockfd);
#else
   close(sockfd);
#endif
   [super dealloc];
}

@end

@implementation Cst502ClientSocket
- (id) initWithHost: (NSString*) host portNumber: (NSString*) port {
   self = [super init];
   hostName = host;
   [hostName retain];
   portNum = port;
   [portNum retain];
   return self;
}

- (BOOL) connect {
   connected = YES;
   memset(&hints, 0, sizeof hints);
   hints.ai_family = AF_UNSPEC;
   hints.ai_socktype = SOCK_STREAM;
   if ((rv = getaddrinfo([hostName UTF8String], [portNum UTF8String],
                         &hints, &servinfo)) != 0) {
      fprintf(stderr, "client error getting host address: %s\n",
              gai_strerror(rv));
      connected = NO;
   }
   // loop through all the results and connect to the first we can
   for(p = servinfo; p != NULL; p = p->ai_next) {
      if ((sockfd = socket(p->ai_family,p->ai_socktype,p->ai_protocol)) == -1){
         perror("client error creating socket");
         connected = NO;
         continue;
      }
      int callret = connect(sockfd, p->ai_addr, p->ai_addrlen);
      if (callret == -1) {
#if defined(WINGS)
         closesocket(sockfd);
#else
         close(sockfd);
#endif
#if defined(WINGS)
         //printf("client failed to connect.\n");
#else
         inet_ntop(p->ai_family, get_in_addr((struct sockaddr *)p->ai_addr),
                   s, sizeof s);
         printf("client failed to connect to %s\n", s);
#endif
         //perror("client error connecting");
         connected = NO;
         continue;
      }
      break;
   }
   if (p == NULL) {
      printf("client failed to connect\n");
      connected = NO;
   }else{
#if defined(WINGS)
      //printf("client connected\n");
#else
      inet_ntop(p->ai_family, get_in_addr((struct sockaddr *)p->ai_addr),
                s, sizeof s);
      printf("client connected to %s\n", s);
#endif
      connected = YES;
   }
   return connected;
}

- (int) sendBytes: (char*) byteMsg OfLength: (int) msgLength Index: (int) at{
   int ret = send(sockfd, byteMsg, msgLength, 0);
   if(ret == -1){
      NSLog(@"client error sending bytes");
   }
   return ret;
}

- (NSString*) receiveBytes: (char*) byteMsg
                  maxBytes: (int) max
                   beginAt: (int) at {
   int ret = recv(sockfd, byteMsg, max-1, at);
   if(ret == -1){
      NSLog(@"client error receiving bytes");
   }
   byteMsg[ret+at] = '\0';
   NSString * retStr = [NSString stringWithUTF8String: byteMsg];
   return retStr;
}

- (void) sendString: (NSString*)str {
    [self sendBytes:(char*)[str UTF8String] OfLength:[str length] Index:0];
}

- (BOOL) close{
   connected = NO;
   return YES;
}

- (void) dealloc {
   [hostName release];
   [portNum release];
   [super dealloc];
}

@end

