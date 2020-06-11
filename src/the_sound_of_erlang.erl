-module(the_sound_of_erlang).

-define(PITCH_STANDARD, 440.0).
-define(SAMPLE_RATE, 48000).
-define(BEATS_PER_MINUTE, 120).
-define(BEAT_DURATION, 60 / ?BEATS_PER_MINUTE).

-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_) ->
    Wave = wave(),
    Filename = "out/increasingSemitones.raw",
    save(Filename, Wave),
    play(Filename),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

wave() ->
   lists:flatten([
       sound(SemiTone, 1) || SemiTone <- lists:seq(0, 11)
   ]).

sound(Semitones, Seconds) ->
    frequency(get_tone(Semitones), Seconds * ?BEAT_DURATION).

get_tone(Semitones) ->
    TwelfthRootOfTwo = math:pow(2, 1.0 / 12.0),
    ?PITCH_STANDARD * math:pow(TwelfthRootOfTwo, Semitones).

frequency(Hz, Duration) ->
    Signals = lists:seq(1, round(?SAMPLE_RATE * Duration)),
    Step = Hz * 2 * math:pi() / ?SAMPLE_RATE,
    [ math:sin(Step * Signal) || Signal <- Signals ].

save(Filename, Wave) ->
    Content = lists:foldl(
        fun(Elem, Acc) ->
            <<Acc/binary, Elem/float>> end,
        <<"">>, Wave),
    ok = file:write_file(Filename, Content).

play(Filename) ->
    StrRate = integer_to_list(?SAMPLE_RATE),
    Cmd = "ffplay -f f64be -ar " ++ StrRate ++ " " ++ Filename,
    os:cmd(Cmd).
