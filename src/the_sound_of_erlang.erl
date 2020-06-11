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
    Filename = "out/increasingNotes.raw",
    save(Filename, Wave),
    play(Filename),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

wave() ->
   lists:flatten([
       sound(Note, 1) || Note <- [c, d, e, f, g, a]
]).

note(Note) ->
    SemitonesShift = semitones_shift(Note),
    get_tone(SemitonesShift).

semitones_shift(c) -> -9;
semitones_shift(d) -> -8;
semitones_shift(e) -> -7;
semitones_shift(f) -> -4;
semitones_shift(g) -> -1;
semitones_shift(a) -> 0.

sound(Note, Beats) ->
    frequency(note(Note), Beats * ?BEAT_DURATION).

get_tone(Semitones) ->
    TwelfthRootOfTwo = math:pow(2, 1.0 / 12.0),
    ?PITCH_STANDARD * math:pow(TwelfthRootOfTwo, Semitones).

frequency(Hz, Duration) ->
    Signals = lists:seq(1, round(?SAMPLE_RATE * Duration)),
    Step = Hz * 2 * math:pi() / ?SAMPLE_RATE,
    RawOutput = [ math:sin(Step * Signal) || Signal <- Signals ],
    OutputLen = length(RawOutput),
    lists:zipwith3(
        fun(Attack, Release, Out) -> Attack * Release * Out end,
        attack(OutputLen), release(OutputLen), RawOutput).

attack(Len) ->
    [min(Multi / 1000, 1) || Multi <- lists:seq(1, Len)].

release(Len) ->
    lists:reverse(attack(Len)).


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
