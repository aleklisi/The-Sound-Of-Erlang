# The sound of Erlang

This article demonstrates how simple it is to make music.

## Requirements

This is a table of all used software with their versions.
| Software      | Version   |
|---            |---        |
| Erlang/OTP    | 22        |
| rebar3        | 3.13.2    |
| ffmpeg        | 4.2.2     |
| ffplay        | 4.2.2     |

### The theory behind
*Sound* is a vibration that propagates as an acoustic wave.

*Frequency* is the number of occurrences of a repeating event per unit of time. Its basic unit is *Hz* which is the number of occurrences in one second.

The *period* is the duration of time of one cycle in a repeating event. The *period* is the reciprocal of the *frequency*.

![equation](http://www.sciweavers.org/tex2img.php?eq=f%20%3D%20%5Cfrac%7B1%7D%7BT%7D%20&bc=White&fc=Black&im=jpg&fs=12&ff=arev&edit=0)

where:
 - f is a frequency in *Hz*
 - T is a period in *seconds*

The simplest way of generating a wave is by providing a sinus signal with the given frequency.

Computers work in a discrete domain and sinus is in a continuous domain therefore *sampling* is the operation to go from continuous to the discrete domain.

*Sampling* sound is the process of converting a sound wave into a signal.

![alt text](https://www.101computing.net/wp/wp-content/uploads/sampling-interval-40.png)

The more samples we approximate our sound with the better quality will be, but also the file in which we store such approximation becomes bigger.

If you want to know more about sampling I recommend: https://youtu.be/f53m72uLa2I

## Starting a project

Start with creating a new project.
Since the script is small I will use `escript` template:

```bash
rebar3 new escript the_sound_of_erlang
```

Check if the project worked correctly with:

```bash
cd the_sound_of_erlang
rebar3 escriptize
./_build/default/bin/the_sound_of_erlang
mkdir out
```

If everything is ok, then you should see the following output:

```bash
$ rebar3 escriptize
===> Verifying dependencies...
===> Compiling the_sound_of_erlang
===> Building escript...

$ ./_build/default/bin/the_sound_of_erlang
Args: []
```

Well done!

## First wave

Lets now generate an example wave. To do it we can just:

```erlang
wave() ->
    [math:sin(X) || X <- lists:seq(1, 48000)].
```

To save a generated wave we need to transform a list of floats to binary representation and just write this binary to a file.

```erlang
save(Filename, Wave) ->
    Content = lists:foldl(
        fun(Elem, Acc) ->
            <<Acc/binary, Elem/float>> end,
        <<"">>, Wave),
    ok = file:write_file(Filename, Content).
```

Call the above 2 functions in the `main/1` function as follows:

```erlang
main(_) ->
    Wave = wave(),
    save("out/first_wave.raw", Wave),
    erlang:halt(0).
```

Build the script binary with:

```bash
rebar3 escriptize
```

And run it with:

```bash
./_build/default/bin/the_sound_of_erlang
```

A new file called **out/first_wave.raw** should show up in a repository.
Then use `ffplay` to listen to the result:

```bash
ffplay -f f64be -ar 48000 out/first_wave.raw
```

Options give:
 - `-f f64be` means that input format is 64-bit big-endian float
 - `-ar 48000` means that the input audio sampling rate is 48000 per second

You can listen to the result [here](/out/first_wave.mp3).
The sound is not the most pleasant but we can hear something.

For the sake of convenience lets now try playing the result sound from the script:

```erlang
play(Filename) ->
    Cmd = "ffplay -f f64be -ar 48000 " ++ Filename,
    os:cmd(Cmd).
```

and add it to end of `main/1`:

```erlang
main(_) ->
    Wave = wave(),
    Filename = "out/first_wave.raw",
    save(Filename, Wave),
    play(Filename),
    erlang:halt(0).
```

Now we can recompile the script and run it with:

```bash
rebar3 escriptize && ./_build/default/bin/the_sound_of_erlang
```

We can also convert a raw file to a .mp3 format with:

```bash
ffmpeg -f f64be -ar 48000 -i out/first_wave.raw out/first_wave.mp3
```

So that it can be played with any music player.

We managed to generate a wave, save it to a file an play it.

## Tuning in

We can now improve our wave not to be just any random sound but a fixed frequency for a given amount of time.
What we want to achieve is to be able to eg. play a sound of a frequency = 440 Hz for 2 seconds.
To get a number of samples played in a given amount of time we need to multiply sample rate times sound duration, since number of samples is an integer we should then round then multiplication result.

```erlang
NumberOfSamples = round(SampleRate * Duration)
```

Sinus period is 2 * PI, knowing that and sample rate we can calculate how long will each signal step last for a given frequency *HZ*.

```erlang
Step = Hz * 2 * math:pi() / SampleRate
```

Knowing number of samples and Step we can map the time domain to a signal as follows:

```erlang
frequency(Hz, Duration, SampleRate) ->
    Signals = lists:seq(1, round(SampleRate * Duration)),
    Step = Hz * 2 * math:pi() / SampleRate,
    [ math:sin(Step * Signal) || Signal <- Signals ].
```

Lets now modify a `wave/0` function to get a sound of 440 Hz played for 2 seconds with sampling rate of 48000 samples per second:

```erlang
wave() ->
    frequency(440, 2, 48000).
```

I change the `Filename` to in the `main/1` function to

```erlang
Filename = "out/2Sec440Hz.raw",
```

just to add it to a repository.
You can listen to the result [here](/out/2Sec440Hz.mp3).
So now lats play the new sound and compare it with https://youtu.be/xGXYFJmvIvk the same frequency sound from YouTube.
For me they sound identical

```bash
rebar3 escriptize && ./_build/default/bin/the_sound_of_erlang
```

We can now try playing 2 sounds with different frequencies and lasting times, but we need to flatten the list of signals to make a list of signals from a list of lists of signals.


```erlang
Filename = "out/2Sec440HzAnd1Sec500Hz.raw",
...

wave() ->
    lists:flatten([
        frequency(440, 2, 48000)
      , frequency(500, 1, 48000)
    ]).
```

You can listen to the result [here](/out/2Sec440HzAnd1Sec500Hz.mp3).

## Frequency to note

Now we are able to play a give frequency for a given amount of time but how to make music out of that?

From [here](https://pages.mtu.edu/~suits/notefreqs.html) we can see that the frequency of an *A4* note is `440 Hz` which is also known as **pitch standard**.
We can also lookup all other needed notes in the same way, but there is an alternative called the frequency ratio of a semitone and it is equal to the twelfth root of two `2 ** (1 / 12)`.
To calculate *A#4* which is 1 semitone higher than *A4* we just multiply `440 * (2 ** (1/12)) = 466.16` which after comparing to a [table](https://pages.mtu.edu/~suits/notefreqs.html) value is *A#4* corresponding frequency.

Lets now try translating the maths into code:

We can add a macro for pitch standard at the top of the file just below the module directive, Add the twel
We can also extract sampling rate there.

```erlang
-module(the_sound_of_erlang).

-define(PITCH_STANDARD, 440.0).
-define(SAMPLE_RATE, 48000).

-export([main/1]).
```

Now we need to use macro for sampling rate in the code.
Lets modify `frequency/3` to `frequency/2`:

```erlang
frequency(Hz, Duration) ->
    Signals = lists:seq(1, round(?SAMPLE_RATE * Duration)),
    Step = Hz * 2 * math:pi() / ?SAMPLE_RATE,
    [ math:sin(Step * Signal) || Signal <- Signals ].
```

Do not forget to change the calls of the frequency function by removing the last argument:

```erlang
wave() ->
    lists:flatten([
        frequency(440, 2)
      , frequency(500, 1)
    ]).
```

and `play/1` as follows:

```erlang
play(Filename) ->
    StrRate = integer_to_list(?SAMPLE_RATE),
    Cmd = "ffplay -f f64be -ar " ++ StrRate ++ " " ++ Filename,
    os:cmd(Cmd).
```

Following function takes number of semitones shift and returns a shifted sound's frequency:

```erlang
get_tone(Semitones) ->
    TwelfthRootOfTwo = math:pow(2, 1.0 / 12.0),
    ?PITCH_STANDARD * math:pow(TwelfthRootOfTwo, Semitones).
```

We need to introduce one more concept which beats per minute which is the base unit for a note to be played. Each note is played in a given number of beats and number of beats per minute is fixed, so we can calculate how long does each beat last (in seconds) by dividing 60 by beats per minute.

Lets now introduce new macro for that:

```erlang
-define(BEATS_PER_MINUTE, 120).
-define(BEAT_DURATION, 60 / ?BEATS_PER_MINUTE).
```

So now we can generate notes for a given amount of time with following function:

```erlang
sound(SemitonesShift, Beats) ->
    frequency(get_tone(SemitonesShift), Beats * ?BEAT_DURATION).
```

 Now lets try it out by providing the following wave:

```erlang
wave() ->
   lists:flatten([
       sound(SemiTone, 1) || SemiTone <- lists:seq(0, 11)
   ]).
 ```

and play it by recompiling and running a script.
I saved my output as `"out/increasingSemitones.raw"`.
You can listen to the result [here](/out/increasingSemitones.mp3).
I only need `[d, f, a, g, c, e]` notes to play my song, but you might need some more so feel free to extend the semitones_shift/1 function.
Lets provide a helper function for easier sound notation:

```erlang
note(Note) ->
    SemitonesShift = semitones_shift(Note),
    get_tone(SemitonesShift).

semitones_shift(c) -> -9;
semitones_shift(d) -> -8;
semitones_shift(e) -> -7;
semitones_shift(f) -> -4;
semitones_shift(g) -> -1;
semitones_shift(a) -> 0.
```

and lets modify slightly `sound/2` function as follows:

```erlang
sound(Note, Beats) ->
   frequency(note(Note), Beats * ?BEAT_DURATION).
```

to use more convenient, newly created `note/1` function instead of `get_tone/1`.
Now we can try out the sounds played by modifying `wave/0` function as follow:

```erlang
wave() ->
   lists:flatten([
       sound(Note, 1) || Note <- [c, d, e, f, g, a]
]).
```

I will save the result in `"out/increasingNotes.raw"` file.
You can listen to the result [here](/out/increasingNotes.mp3).

## ADSR

When listening to the result there is very string tick when changing the note.
It is because the sound increases and decreases too rapidly.
To resolve this issue we can implement so called *ADSR* which stands for *a*ttack *d*ecay *s*ustain *r*elease and works by modifying sound amplitude (volume) according to a following chart:

![alt text](https://i.stack.imgur.com/lazZO.png)

For the sake of simplicity it is enough to implement only Attack and Release parts since we already have Sustain part.
To do it lets implement attack we will consider minimum of sequence of natural numbers divided by 1000 or 1:

```erlang
attack(Len) ->
    [min(Multi / 1000, 1) || Multi <- lists:seq(1, Len)].
```

such list would be something like:

```erlang
[0.001, 0.002, ... 0.999, 1, 1, 1, ..., 1]
```

Now lets generate release the lazy way:

```erlang
release(Len) ->
    lists:reverse(attack(Len)).
```

so the release function generates the following list:
```erlang
[1, 1, 1, ..., 1, 0.999, ..., 0.002, 0.001]
```

Now we need to slightly modify the `frequency/2` to adjust the sound volume:

```erlang
frequency(Hz, Duration) ->
    Signals = lists:seq(1, round(?SAMPLE_RATE * Duration)),
    Step = Hz * 2 * math:pi() / ?SAMPLE_RATE,
    RawOutput = [ math:sin(Step * Signal) || Signal <- Signals ],
    OutputLen = length(RawOutput),
    lists:zipwith3(
        fun(Attack, Release, Out) -> Attack * Release * Out end,
        attack(OutputLen), release(OutputLen), RawOutput).
```

I saved the result as `out/increasingNotesASR.raw`. Now when you rebuild and run the script you will hear smooth pass between the notes.
You can listen to the result [here](/out/increasingNotesASR.mp3).

## The sh!t just got real

Now we will try to play an actual song.
Lets modify the `wave/0` function as follows:

```erlang
wave() ->
   lists:flatten([
        sound(f, 0.5)
      , sound(f, 0.5)
      , sound(a, 0.5)
      , sound(a, 0.5)
      , sound(g, 4)
      , sound(c, 0.5)
      , sound(c, 0.5)
      , sound(c, 0.5)
      , sound(e, 0.5)
      , sound(e, 0.5)
      , sound(g, 0.5)
      , sound(g, 0.5)
      , sound(f, 4)
]).
```



Also change beat per minute to 88. Explanation why should you do this change can be found [here](https://courses.lumenlearning.com/suny-musicappreciationtheory/chapter/introduction-to-tempo) but it it is out of scope of this article so I will not go into further details.

```erlang
-define(BEATS_PER_MINUTE, 88).
```

Recompile and run the script.
You can listen to the result [here](/out/the_sound_of_Erlang.mp3).
I save the the result to `out/the_sound_of_Erlang.raw` I hope you recognise the masterpiece I picked [Disturbed - The Sound Of Silence](https://i.pinimg.com/originals/32/89/9a/32899a5d903aa1650ca1d5ebb5dab9dd.gif) the following lines "Hello darkness, my old friend
I've come to talk with you again" part.

I am looking forward to listen to your favorite song played with this tool.

## Sources:
 - https://en.wikipedia.org/wiki/Frequency
 - https://en.wikipedia.org/wiki/Sound
 - https://en.wikipedia.org/wiki/Sampling_(signal_processing)
 - https://youtu.be/f53m72uLa2I
 - https://pages.mtu.edu/~suits/notefreqs.html
 - https://en.wikipedia.org/wiki/Twelfth_root_of_two
 - https://courses.lumenlearning.com/suny-musicappreciationtheory/chapter/introduction-to-tempo/
