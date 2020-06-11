-module(the_sound_of_erlang).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_) ->
    Wave = wave(),
    save("out/first_wave.raw", Wave),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

wave() ->
    [math:sin(X) || X <- lists:seq(1, 48000)].

save(Filename, Wave) ->
    Content = lists:foldl(
        fun(Elem, Acc) ->
            <<Acc/binary, Elem/float>> end,
        <<"">>, Wave),
    ok = file:write_file(Filename, Content).
