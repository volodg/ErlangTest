-module(counter).
-export([sleep/0,flush_buffer/0,priority_receive/1,priority_receive/2]).

sleep(Time) ->
	receive
    	after Time ->
        	true
	end.

flush_buffer() ->
	receive
		AnyMessage ->
			flush_buffer()
	    after 0 ->
	    	true
	end.

priority_receive( Term ) ->
	receive
		Term ->
			Term
	    after 0 ->
	    	receive
	   			AnyMessage ->
					AnyMessage
			end
	end

priority_receive( Term, Otherwise ) ->
	receive
		Term ->
			Term
	    after 0 ->
	    	Otherwise()
	end
