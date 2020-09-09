-module(star_wars_main_theme).

% Based on:
% https://www.musicnotes.com/images/productimages/large/mtd/MN0127456.gif

-behaviour(melody).

-export([sounds/0, beats_per_minute/0]).

beats_per_minute() ->
    120.

sounds() ->
    [
        {d4, 0.5}
    ,   {d4, 0.5}
    ,   {d4, 0.5}

    ,   {g4, 2}
    ,   {d5, 2}

    ,   {c4, 0.5}
    ,   {b4, 0.5}
    ,   {a4, 0.5}
    ,   {g5, 2}
    ,   {d5, 1}

    ,   {c4, 0.5}
    ,   {b4, 0.5}
    ,   {a4, 0.5}
    ,   {g5, 2}
    ,   {d5, 1}

    ,   {c4, 0.5}
    ,   {b4, 0.5}
    ,   {c4, 0.5}
    ,   {a4, 2}
    ,   {d4, 1}
    ,   {d4, 0.5}

    ,   {g4, 2}
    ,   {d5, 2}

    ,   {c4, 0.5}
    ,   {b4, 0.5}
    ,   {a4, 0.5}
    ,   {g5, 2}
    ,   {d5, 1}

    ,   {c4, 0.5}
    ,   {b4, 0.5}
    ,   {a4, 0.5}
    ,   {g5, 2}
    ,   {d5, 1}

    ,   {c4, 0.5}
    ,   {b4, 0.5}
    ,   {c4, 0.5}
    ,   {a4, 2}
    ,   {d4, 1}
    ,   {d4, 0.5}

    ,   {e4, 1.5}
    ,   {e4, 0.5}
    ,   {c4, 0.5}
    ,   {b4, 0.5}
    ,   {a4, 0.5}
    ,   {g4, 0.5}

    ,   {g4, 0.5}
    ,   {a4, 0.5}
    ,   {b4, 0.5}
    ,   {a4, 1}
    ,   {e4, 0.5}
    ,   {f4sharp, 1}
    ,   {d4, 1}
    ,   {d4, 0.5}
    ].
