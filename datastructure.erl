-module(datastructure).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

stack_push(Element, Stack) -> [Element|Stack].

stack_pop(Stack) -> 
    case Stack of
        [H|_] -> H;
        [] -> throw(empty_stack);
        _ -> throw(should_not_be_here)
    end.

stack_size(Stack) -> length(Stack).

stack_isemtpy(Stack) -> stack_size(Stack) =:= 0.

make_stack() -> [].

make_queue() -> {[], []}.

queue_push(Element, Queue) ->
    case Queue of 
        {A, B} -> {[Element|A], B}
    end.

queue_pop(Queue) ->
    case Queue of 
        {[], []} -> throw(empty_queue);
        {In, []} -> queue_pop({[], lists:reverse(In)});
        {In, [H|T]} -> {{In,T}, H}
    end.

queue_size({A, B}) -> length(A) + length(B).

queue_push_pop_test() ->
    Elements = [rand:uniform()|| _ <- lists:seq(1, 10)],
    Queue = lists:foldl(fun(E, Q)->queue_push(E,Q) end, make_queue(), Elements),
    {_,Elements_reversed} = lists:foldl(fun(_, {Q,L})->
                                            {Qr,H} = queue_pop(Q),
                                            {Qr, [H|L]}
                                    end,
                        {Queue,[]},
                        lists:seq(1, queue_size(Queue))),
    ?assert(Elements =:= lists:reverse(Elements_reversed)).
