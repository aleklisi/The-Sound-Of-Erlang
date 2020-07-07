-module(the_sound_of_silence).

-behaviour(melody).

-export([sounds/0, beats_per_minute/0]).

beats_per_minute() ->
    120.

sounds() ->
    [
        {e4, 0.5}
    ,   {e4, 0.5}
    ,   {g4, 0.5}
    ,   {g4, 0.5}
    ,   {b4, 0.5}
    ,   {a4, 4}
    ,   {d4, 0.5}
    ,   {d4, 0.5}
    ,   {d4, 0.5}
    ,   {f4, 0.5}
    ,   {f4, 0.5}
    ,   {a4, 0.5}
    ,   {a4, 0.5}
    ,   {g4, 4}
    ,   {g4, 0.5}
    ,   {g4, 0.5}
    ,   {b4, 0.5}
    ,   {b4, 0.5}
    ,   {d4, 0.5}
    ,   {d4, 0.5}
    ,   {e4, 1}
    ,   {e4, 0.5}
    ,   {d4, 2}
    ,   {g4, 0.5}
    ,   {g4, 0.5}
    ,   {b4, 0.5}
    ,   {b4, 0.5}
    ,   {d4, 0.5}
    ,   {d4, 0.5}
    ,   {e4, 1}
    ,   {e4, 0.5}
    ,   {d4, 2}
    ].