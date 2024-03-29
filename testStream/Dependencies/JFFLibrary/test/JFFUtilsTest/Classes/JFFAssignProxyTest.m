#import <JFFUtils/JFFAssignProxy.h>

@interface ProxyTargetTest : NSObject

@end

@implementation ProxyTargetTest

-(NSUInteger)justReturnFiveNumber
{
   return 5;
}

@end

@interface JFFAssignProxyTest : GHTestCase
@end

@implementation JFFAssignProxyTest

-(void)testAssignProxyDealloc
{
   JFFAssignProxy* proxy_ = nil;
   __block BOOL target_deallocated_ = NO;

   @autoreleasepool
   {
      ProxyTargetTest* target_ = [ ProxyTargetTest new ];
      [ target_ addOnDeallocBlock: ^void( void )
      {
         target_deallocated_ = YES;
      } ];

      proxy_ = [ [ JFFAssignProxy assignProxyWithTarget: target_ ] retain ];

      [ target_ release ];
   }

   GHAssertTrue( target_deallocated_, @"Target should be dealloced" );

   [ proxy_ release ];
}

-(void)testAssignProxyMethodCalls
{
   ProxyTargetTest* target_ = [ ProxyTargetTest new ];

   id proxy_ = [ JFFAssignProxy assignProxyWithTarget: target_ ];
   GHAssertTrue( 5 == [ proxy_ justReturnFiveNumber ], @"Target should be dealloced" );

   [ target_ release ];
}

@end
