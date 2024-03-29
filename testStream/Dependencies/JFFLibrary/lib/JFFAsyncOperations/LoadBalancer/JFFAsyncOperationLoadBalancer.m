#import "JFFAsyncOperationLoadBalancer.h"

#import "JFFContextLoaders.h"
#import "JFFPedingLoaderData.h"
#import "JFFAsyncOperationLoadBalancerContexts.h"
#import "JFFAsyncOperationProgressBlockHolder.h"
#import "JFFCancelAyncOperationBlockHolder.h"
#import "JFFDidFinishAsyncOperationBlockHolder.h"
#import "JFFAsyncOperationsPredefinedBlocks.h"

static const NSUInteger max_operation_count_ = 5;
static const NSUInteger total_max_operation_count_ = 7;

static NSUInteger global_active_number_ = 0;

static JFFAsyncOperationLoadBalancerContexts* sharedBalancer()
{
   return [ JFFAsyncOperationLoadBalancerContexts sharedBalancer ];
}

static void setBalancerCurrentContextName( NSString* context_name_ )
{
   sharedBalancer().currentContextName = context_name_;
}

static BOOL canPeformAsyncOperationForContext( JFFContextLoaders* context_loaders_ );
static BOOL findAndTryToPerformNextNativeLoader( void );

void setBalancerActiveContextName( NSString* context_name_ )
{
   if ( [ sharedBalancer().activeContextName isEqualToString: context_name_ ] )
      return;

   NSLog( @"!!!SET ACTIVE CONTEXT NAME: %@", context_name_ );
   sharedBalancer().activeContextName = context_name_;
   setBalancerCurrentContextName( context_name_ );

   while ( findAndTryToPerformNextNativeLoader() );
}

static void peformBlockWithinContext( JFFSimpleBlock block_, JFFContextLoaders* context_loaders_ )
{
   NSString* current_context_name_ = sharedBalancer().currentContextName;
   sharedBalancer().currentContextName = context_loaders_.name;

   block_();

   sharedBalancer().currentContextName = current_context_name_;
}

static JFFAsyncOperation wrappedAsyncOperationWithContext( JFFAsyncOperation native_loader_
                                                          , JFFContextLoaders* context_loaders_ );

static void performInBalancerPedingLoaderData( JFFPedingLoaderData* pending_loader_data_
                                              , JFFContextLoaders* context_loaders_ )
{
   JFFAsyncOperation balanced_loader_ = wrappedAsyncOperationWithContext( pending_loader_data_.nativeLoader, context_loaders_ );

   balanced_loader_( pending_loader_data_.progressCallback
                    , pending_loader_data_.cancelCallback
                    , pending_loader_data_.doneCallback );
}

static BOOL performLoaderFromContextIfPossible( JFFContextLoaders* context_loaders_ )
{
   BOOL have_pending_loaders_ = ( context_loaders_.pendingLoadersNumber > 0 );
   if ( have_pending_loaders_
       && canPeformAsyncOperationForContext( context_loaders_ ) )
   {
      JFFPedingLoaderData* pending_loader_data_ = [ context_loaders_ popPendingLoaderData ];
      performInBalancerPedingLoaderData( pending_loader_data_, context_loaders_ );
      //TODO remove empty context_loaders_ (without tasks)
      return YES;
   }
   return NO;
}

static BOOL findAndTryToPerformNextNativeLoader( void )
{
   JFFAsyncOperationLoadBalancerContexts* balancer_ = sharedBalancer();

   JFFContextLoaders* active_loaders_ = [ balancer_ activeContextLoaders ];
   if ( performLoaderFromContextIfPossible( active_loaders_ ) )
      return YES;

   for ( NSString* name_ in balancer_.allContextNames )
   {
      JFFContextLoaders* context_loaders_ = [ balancer_.contextLoadersByName objectForKey: name_ ];
      if ( performLoaderFromContextIfPossible( context_loaders_ ) )
         return YES;
   }

   return NO;
}

static void logBalancerState()
{
   return;
   NSLog( @"|||||LOAD BALANCER|||||" );
   JFFAsyncOperationLoadBalancerContexts* balancer_ = sharedBalancer();
   JFFContextLoaders* active_loaders_ = [ balancer_ activeContextLoaders ];
   NSLog( @"Active context name: %@", active_loaders_.name );
   NSLog( @"pending count: %d", active_loaders_.pendingLoadersNumber );
   NSLog( @"active  count: %d", active_loaders_.activeLoadersNumber );

   for ( NSString* name_ in balancer_.allContextNames )
   {
      JFFContextLoaders* context_loaders_ = [ balancer_.contextLoadersByName objectForKey: name_ ];

      if ( [ name_ isEqualToString: active_loaders_.name ] )
         continue;

      NSLog( @"context name: %@", context_loaders_.name );
      NSLog( @"pending count: %d", context_loaders_.pendingLoadersNumber );
      NSLog( @"active  count: %d", context_loaders_.activeLoadersNumber );
   }
   NSLog( @"|||||END LOG|||||" );
}

static void finishExecuteOfNativeLoader( JFFAsyncOperation native_loader_
                                        , JFFContextLoaders* context_loaders_ )
{
   if ( [ context_loaders_ removeActiveNativeLoader: native_loader_ ] )
   {
      --global_active_number_;
      logBalancerState();
   }
}

static JFFCancelAsyncOperationHandler cancelCallbackWrapper( JFFCancelAsyncOperationHandler native_cancel_callback_
                                                            , JFFAsyncOperation native_loader_
                                                            , JFFContextLoaders* context_loaders_ )
{
   native_cancel_callback_ = [ [ native_cancel_callback_ copy ] autorelease ];
   return [ [ ^void( BOOL canceled_ )
   {
      if ( !canceled_ )
      {
         assert( NO );// @"balanced loaders should not be unsubscribed from native loader"
      }

      [ [ native_cancel_callback_ copy ] autorelease ];

      finishExecuteOfNativeLoader( native_loader_, context_loaders_ );

      if ( native_cancel_callback_ )
      {
         peformBlockWithinContext( ^
         {
            native_cancel_callback_( canceled_ );
         }, context_loaders_ );
      }

      findAndTryToPerformNextNativeLoader();
   } copy ] autorelease ];
}

static JFFDidFinishAsyncOperationHandler doneCallbackWrapper( JFFDidFinishAsyncOperationHandler native_done_callback_
                                                             , JFFAsyncOperation native_loader_
                                                             , JFFContextLoaders* context_loaders_ )
{
   native_done_callback_ = [ [ native_done_callback_ copy ] autorelease ];
   return [ [ ^void( id result_, NSError* error_ )
   {
      [ [ native_done_callback_ copy ] autorelease ];

      finishExecuteOfNativeLoader( native_loader_, context_loaders_ );

      if ( native_done_callback_ )
      {
         peformBlockWithinContext( ^
         {
            native_done_callback_( result_, error_ );
         }, context_loaders_ );
      }

      findAndTryToPerformNextNativeLoader();
   } copy ] autorelease ];
}

static JFFAsyncOperation wrappedAsyncOperationWithContext( JFFAsyncOperation native_loader_
                                                          , JFFContextLoaders* context_loaders_ )
{
   native_loader_ = [ [ native_loader_ copy ] autorelease ];
   return [ [ ^JFFCancelAsyncOperation( JFFAsyncOperationProgressHandler native_progress_callback_
                                       , JFFCancelAsyncOperationHandler native_cancel_callback_
                                       , JFFDidFinishAsyncOperationHandler native_done_callback_ )
   {
      //progress holder for unsubscribe
      JFFAsyncOperationProgressBlockHolder* progress_block_holder_ = [ [ JFFAsyncOperationProgressBlockHolder new ] autorelease ];
      progress_block_holder_.progressBlock = native_progress_callback_;
      JFFAsyncOperationProgressHandler wrapped_progress_callback_ = ^void( id progress_info_ )
      {
         peformBlockWithinContext( ^
         {
            [ progress_block_holder_ performProgressBlockWithArgument: progress_info_ ];
         }, context_loaders_ );
      };

      __block BOOL done_ = NO;

      //cancel holder for unsubscribe
      JFFCancelAyncOperationBlockHolder* cancel_callback_block_holder_ = [ [ JFFCancelAyncOperationBlockHolder new ] autorelease ];
      cancel_callback_block_holder_.cancelBlock = native_cancel_callback_;
      JFFCancelAsyncOperation wrapped_cancel_callback_ = ^void( BOOL canceled_ )
      {
         done_ = YES;
         cancel_callback_block_holder_.onceCancelBlock( canceled_ );
      };

      //finish holder for unsubscribe
      JFFDidFinishAsyncOperationBlockHolder* finish_block_holder_ = [ [ JFFDidFinishAsyncOperationBlockHolder new ] autorelease ];
      finish_block_holder_.didFinishBlock = native_done_callback_;
      JFFDidFinishAsyncOperationHandler wrapped_done_callback_ = ^void( id result_, NSError* error_ )
      {
         done_ = YES;
         finish_block_holder_.onceDidFinishBlock( result_, error_ );
      };

      wrapped_cancel_callback_ = cancelCallbackWrapper( wrapped_cancel_callback_
                                                       , native_loader_
                                                       , context_loaders_ );

      wrapped_done_callback_ = doneCallbackWrapper( wrapped_done_callback_
                                                   , native_loader_
                                                   , context_loaders_ );

      //TODO check native loader no within balancer !!!
      JFFCancelAsyncOperation cancel_block_ = native_loader_( wrapped_progress_callback_
                                                             , wrapped_cancel_callback_
                                                             , wrapped_done_callback_ );

      if ( done_ )
      {
         return JFFEmptyCancelAsyncOperationBlock;
      }

      ++global_active_number_;

      JFFCancelAsyncOperation wrapped_cancel_block_ = [ [ ^void( BOOL canceled_ )
      {
         if ( canceled_ )
         {
            cancel_block_( YES );
         }
         else
         {
            cancel_callback_block_holder_.onceCancelBlock( NO );

            progress_block_holder_.progressBlock = nil;
            finish_block_holder_.didFinishBlock = nil;
         }
      } copy ] autorelease ];

      [ context_loaders_ addActiveNativeLoader: native_loader_
                                 wrappedCancel: wrapped_cancel_block_ ];
      logBalancerState();

      return wrapped_cancel_block_;
   } copy ] autorelease ];
}

static BOOL canPeformAsyncOperationForContext( JFFContextLoaders* context_loaders_ )
{
   //TODO check condition yet
   BOOL is_active_context_ = [ sharedBalancer().activeContextName isEqualToString: context_loaders_.name ];
   return ( ( is_active_context_ && context_loaders_.activeLoadersNumber < max_operation_count_ )
           || 0 == global_active_number_ )
      && global_active_number_ <= total_max_operation_count_;
}

JFFAsyncOperation balancedAsyncOperation( JFFAsyncOperation native_loader_ )
{
   JFFContextLoaders* context_loaders_ = [ sharedBalancer() currentContextLoaders ];

   native_loader_ = [ [ native_loader_ copy ] autorelease ];
   return [ [ ^JFFCancelAsyncOperation( JFFAsyncOperationProgressHandler progress_callback_
                                       , JFFCancelAsyncOperationHandler cancel_callback_
                                       , JFFDidFinishAsyncOperationHandler done_callback_ )
   {
      if ( canPeformAsyncOperationForContext( context_loaders_ ) )
      {
         JFFAsyncOperation context_loader_ = wrappedAsyncOperationWithContext( native_loader_, context_loaders_ );
         return context_loader_( progress_callback_, cancel_callback_, done_callback_ );
      }

      cancel_callback_ = [ [ cancel_callback_ copy ] autorelease ];
      [ context_loaders_ addPendingNativeLoader: native_loader_
                               progressCallback: progress_callback_
                                 cancelCallback: cancel_callback_
                                   doneCallback: done_callback_ ];

      logBalancerState();

      JFFCancelAsyncOperation cancel_ = [ [ ^void( BOOL canceled_ )
      {
         if ( ![ context_loaders_ containsPendingNativeLoader: native_loader_ ] )
         {
            //cancel only wrapped cancel block
            [ context_loaders_ cancelActiveNativeLoader: native_loader_ cancel: canceled_ ];
            return;
         }

         if ( canceled_ )
         {
            [ context_loaders_ removePendingNativeLoader: native_loader_ ];
            cancel_callback_( YES );
         }
         else
         {
            cancel_callback_( NO );

            [ context_loaders_ unsubscribePendingNativeLoader: native_loader_ ];
         }
      } copy ] autorelease ];

      return cancel_;
   } copy ] autorelease ];
}
