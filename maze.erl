-module(maze).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-import(lists, [seq/2]).
-import(datastructure, [make_queue/0, queue_push/2, queue_pop/1, queue_size/1]).

get_neighbor(X, Y, N, M) -> get_neighbor1(X, Y, N, M, [], up).
get_neighbor1(X, Y, N, M, Accu, Direction) ->
    case {X, Y, Direction} of
         {1, _, up} -> get_neighbor1(X, Y, N, M, Accu, down);
         {_, _, up} -> get_neighbor1(X, Y, N, M, [{X-1, Y}|Accu], down);
         {N, _, down} -> get_neighbor1(X, Y, N, M, Accu, left);
         {_, _, down} -> get_neighbor1(X, Y, N, M, [{X +1, Y}|Accu], left);
         {_, 1, left} -> get_neighbor1(X, Y, N, M, Accu, right);
         {_, _, left} -> get_neighbor1(X, Y, N, M, [{X, Y-1}|Accu], right);
         {_, M, right} -> Accu; 
         {_, _, right} -> [{X, Y+1}|Accu]
    end.

get_neighbor_test() ->
    Cases = [get_neighbor(5,6,10,20),
             get_neighbor(0,13,10,20),
             get_neighbor(0,0,10,20),
             get_neighbor(3,0,10,20),
             get_neighbor(10,13,10,20),
             get_neighbor(4,20,10,20),
             get_neighbor(10,20,10,20)
            ],
    Result =
        [[{5,7},{5,5},{6,6},{4,6}],
        [{0,14},{0,12},{1,13}],
        [{0,1},{1,0}],
        [{3,1},{4,0},{2,0}],
        [{10,14},{10,12},{9,13}],
        [{4,19},{5,20},{3,20}],
        [{10,19},{9,20}]],
    ?assert(Cases =:= Result).

test_equal_generator(Function, Args, Result) ->
    Ret = apply(?MODULE, Function, Args),
    fun() -> ?assert(Result=:=Ret) end.

test_args_against_results(Function, ArgsList, Results) ->
    [test_equal_generator(Function, Args, Result)|| Args<-ArgsList, Result<-Results].

create_grid(N, M) ->  
    Create_node = 
        fun(X, Y) ->
            {{X, Y}, get_neighbor(X, Y, N, M), not_visited, parent}    
        end,
    Graphnodes = [{{X,Y},Create_node(X, Y)}||X <-lists:seq(1,N), Y<-lists:seq(1,M)],
    maps:from_list(Graphnodes).

print_nodes([])->[];
print_nodes([H|T])->
    io:format("~w~n", [H]),
    print_nodes(T).

list_shuffle(L)->
    [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- L])].

print_maze_tree(Graph, N, M) -> 
    Edge_direction = fun({X1,Y1}= _Nodeid_from, {X2,Y2} = _Nodeid_to) ->
            case {X1, Y1, X2, Y2} of
                {X, _, X, _} when Y1 =:= Y2 + 1 -> left;
                {X, _, X, _} when Y1 =:= Y2 - 1 -> right;
                {_, Y, _, Y} when X1 =:= X2 - 1 -> down;
                {_, Y, _, Y} when X1 =:= X2 + 1 -> up 
            end
        end,
    Get_direction = fun(Node) ->
            Listfff = 
            case Node of 
                {_,[]} -> [];
                {Nodeid,Neighbors} -> [Edge_direction(Nodeid, Neighbor)||Neighbor<-Neighbors]
            end,
            ordsets:to_list(ordsets:from_list(Listfff))
        end,
    Get_charactor = fun(Directions) ->
        case Directions of
            [down,left,right,up] -> "┼";
            [left,right,up] -> "┴";
            [down,right,up] -> "├";
            [down,left,up] -> "┤";
            [down,left,right] -> "┬";
            [left,up] -> "┘";
            [down,left] -> "┐";
            [right,up] -> "└";
            [down,right] -> "┌";
            [down,up] -> "│";
            [left,right] -> "─";
            [up] -> "╵";
            [down] -> "╷";
            [left] -> "╴";
            [right] -> "╶";
            [] -> " "; %nothing, for root
            _ -> throw(should_not_no_direction)
        end
    end,
    Update_edge = fun(Graphin, Node)->
            case Node of
                {_,_,_,nil} -> Graphin;
                {Nodeid,_,_,P} -> 
                    {_,Neighbors}= maps:get(P, Graphin),
                    maps:update(P, {P,[Nodeid|Neighbors]}, Graphin)
            end
        end,
    Graph_with_direction_1 = maps:map(fun(_,Node)->
                                            case Node of
                                                {Nodeid,_,_,nil} -> {Nodeid, []};
                                                {Nodeid,_,_,P} ->{Nodeid,[P]}
                                            end
                                        end,
                                      Graph),
    Graph_with_direction_2 = maps:fold(fun(_, Node,G)->Update_edge(G,Node) end,
                                   Graph_with_direction_1, Graph),
    Graph_to_print = maps:map(fun(_,Node) ->
                                        Get_charactor(Get_direction(Node))
                              end,
                              Graph_with_direction_2),
    Strings_to_print = [lists:flatten([maps:get({X,Y},Graph_to_print)||Y<-seq(1,M)])||X<-seq(1,N)],
    String_to_print_ = lists:flatten(lists:join("\n", Strings_to_print)),
    String_to_print = unicode:characters_to_binary(String_to_print_),
    io:format("~s~n",[String_to_print]).

maze_encode(Graph, N, M) ->

    Edge_direction = fun({X1,Y1}= _Nodeid_from, {X2,Y2} = _Nodeid_to) ->
            case {X1, Y1, X2, Y2} of
                {X, _, X, _} when Y1 =:= Y2 + 1 -> left;
                {X, _, X, _} when Y1 =:= Y2 - 1 -> right;
                {_, Y, _, Y} when X1 =:= X2 - 1 -> down;
                {_, Y, _, Y} when X1 =:= X2 + 1 -> up 
            end
        end,
    Get_direction = fun(Node) ->
            Listfff = 
            case Node of 
                {_,[]} -> [];
                {Nodeid,Neighbors} -> [Edge_direction(Nodeid, Neighbor)||Neighbor<-Neighbors]
            end,
            ordsets:to_list(ordsets:from_list(Listfff))
        end,
    Get_charactor = fun(Directions) ->
        case Directions of
            [down,left,right,up] -> "00";
            [left,right,up] -> "00";
            [down,right,up] -> "00";
            [down,left,up] -> "01";
            [down,left,right] -> "10";
            [left,up] -> "01";
            [down,left] -> "11";
            [right,up] -> "00";
            [down,right] -> "10";
            [down,up] -> "01";
            [left,right] -> "10";
            [up] -> "01";
            [down] -> "11";
            [left] -> "11";
            [right] -> "10";
            [] -> "11"; %nothing, for root
            _ -> throw(should_not_no_direction)
        end
    end,
    Update_edge = fun(Graphin, Node)->
            case Node of
                {_,_,_,nil} -> Graphin;
                {Nodeid,_,_,P} -> 
                    {_,Neighbors}= maps:get(P, Graphin),
                    maps:update(P, {P,[Nodeid|Neighbors]}, Graphin)
            end
        end,
    Graph_with_direction_1 = maps:map(fun(_,Node)->
                                            case Node of
                                                {Nodeid,_,_,nil} -> {Nodeid, []};
                                                {Nodeid,_,_,P} ->{Nodeid,[P]}
                                            end
                                        end,
                                      Graph),
    Graph_with_direction_2 = maps:fold(fun(_, Node,G)->Update_edge(G,Node) end,
                                   Graph_with_direction_1, Graph),
    Graph_to_print = maps:map(fun(_,Node) ->
                                        Get_charactor(Get_direction(Node))
                              end,
                              Graph_with_direction_2),
    Strings_to_print = [lists:flatten(lists:join(",",[maps:get({X,Y}, Graph_to_print)||Y<-seq(1,M)]))||X<-seq(1,N)],
    String_to_print = lists:flatten(lists:join("\n", Strings_to_print)),
    io:format("~s~n",[String_to_print]).

set_parent(Graph, Nodeid, Parent) ->
    Node = maps:get(Nodeid, Graph),
    case Node of
        {Nodeid, Neighbors, not_visited, _} -> 
            NewNode = {Nodeid, Neighbors, visited, Parent},
            {true, NewNode};
        _ -> false
    end.

dfs(Rootid, Graph, Parent) ->
    Root = maps:get(Rootid, Graph),
    case Root of
         {Nodeid, Neighbors, not_visited, parent} -> 
            {true, NewSelf} = set_parent(Graph, Nodeid, Parent),
            Nodestovisit = list_shuffle(Neighbors),
            NewGraph = maps:update(Nodeid, NewSelf,Graph),
            lists:foldl(fun (Elem,G) -> dfs(Elem, G, Nodeid) end, NewGraph, Nodestovisit);
         {_, _, visited, _} -> Graph;
         _ -> throw({should_not_be_here, Root})
    end.

dfs_from_start(Graph) ->
    First_node_id = {1,1},
    dfs(First_node_id, Graph, nil).

bfs_loop(Queue, Graph) ->
    case queue_size(Queue) of
        0 -> Graph;
        _ ->
            {Queue1, Nodeid} = queue_pop(Queue),
            {_, Neighbors, _, _} = maps:get(Nodeid, Graph),
            Nodestovisit_ = lists:filtermap(fun(Elem)->set_parent(Graph, Elem, Nodeid) end, Neighbors),
            Nodestovisit = list_shuffle(Nodestovisit_),
            NewGraph = lists:foldl(fun(Elem={Nodeid1,_,_,_}, Graphacc) ->
                                           maps:update(Nodeid1,Elem,Graphacc)
                                        end,
                                   Graph,
                                   Nodestovisit),
            NewQueue = lists:foldl(fun({Nodeid_local,_,_,_}, Q)->queue_push(Nodeid_local,Q) end,
                                   Queue1, Nodestovisit),
            bfs_loop(NewQueue, NewGraph)
    end.

help()->
    io:format("usage: erl ... gen maze SideLength...~n").

gen() ->
    help().

gen(Args) ->
    case Args of
        [Arg1, Arg2] -> 
            try [list_to_integer(Arg1), list_to_integer(Arg2)] of
                [N, M] -> 
                    G = dfs_from_start(create_grid(N, M)),
                    %% print_maze_tree(G, S, S),
                    %% io:format("~n"),
                    maze_encode(G, N, M)

            catch
                error:badarg -> help()
            end;
        _ -> help() 
    end.

bfs_from_start(Graph) ->
    First_node_id = {1,1},
    {true, First_node} = set_parent(Graph, First_node_id, nil),
    Queue = queue_push(First_node_id, make_queue()),
    bfs_loop(Queue, maps:update(First_node_id, First_node, Graph)).
                                       
