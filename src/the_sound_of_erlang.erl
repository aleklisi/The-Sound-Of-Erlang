-module(the_sound_of_erlang).

-define(PITCH_STANDARD, 440.0).
-define(SAMPLE_RATE, 48000).
-define(SONG, star_wars_main_theme).

-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_) ->
    Wave = wave(),
    Filename = "out/the_sound_of_Erlang.raw",
    save(Filename, Wave),
    play(Filename),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

beats_per_minute() ->
    ?SONG:beats_per_minute().

beat_duration() ->
    60 / beats_per_minute().

wave() ->
    RawSounds = ?SONG:sounds(),
    Sounds = lists:map(
        fun({Note, Duration}) ->
            sound(Note, Duration)
        end, RawSounds),
   lists:flatten(Sounds).

note(Note) ->
    SemitonesShift = semitones_shift(Note),
    get_tone(SemitonesShift).

semitones_shift(c4)         -> -9;
semitones_shift(c4sharp)    -> -8;
semitones_shift(d4flat)     -> -8;
semitones_shift(d4)         -> -7;
semitones_shift(d4sharp)    -> -6;
semitones_shift(e4flat)     -> -6;
semitones_shift(e4)         -> -5;
semitones_shift(f4)         -> -4;
semitones_shift(f4sharp)    -> -3;
semitones_shift(g4flat)     -> -3;
semitones_shift(g4)         -> -2;
semitones_shift(g4sharp)    -> -1;
semitones_shift(a4flat)     -> -1;
semitones_shift(a4)         -> 0;
semitones_shift(a4sharp)    -> 1;
semitones_shift(b4flat)     -> 1;
semitones_shift(b4)         -> 2;
semitones_shift(c5)         -> 3;
semitones_shift(c5sharp)    -> 4;
semitones_shift(d5flat)     -> 4;
semitones_shift(d5)         -> 5;
semitones_shift(d5sharp)    -> 6;
semitones_shift(e5flat)     -> 6;
semitones_shift(e5)         -> 7;
semitones_shift(f5)         -> 8;
semitones_shift(f5sharp)    -> 9;
semitones_shift(g5flat)     -> 9;
semitones_shift(g5)         -> 10;
semitones_shift(g5sharp)    -> 11;
semitones_shift(a5flat)     -> 11;
semitones_shift(a5)         -> 12.

sound(Note, Beats) ->
    frequency(note(Note), Beats * beat_duration()).

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
