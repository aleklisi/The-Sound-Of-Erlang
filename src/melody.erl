-module(melody).

-type note() :: c4 | c4sharp | d4flat | d4 | d4sharp | e4flat | e4 |
                f4 | f4sharp | g4flat | g4 | g4sharp | a4flat | a4 |
                a4sharp | b4flat | b4 | c5 | c5sharp | d5flat | d5 |
                d5sharp | e5flat | e5 | f5 | f5sharp | g5flat | g5 |
                g5sharp | a5flat | a5.
-type duration() :: float().

-callback beats_per_minute() -> non_neg_integer().

-callback sounds() -> {note(), duration()}.
