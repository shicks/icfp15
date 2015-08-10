# ICFP Contest 2015

## Our Team: mi-goto

Our team consisted of

* Stephen Hicks (stephenhicks@gmail.com)
* Peter Gavin (pgavin@gmail.com)

who both work at Google, Inc., on the Data Center Power team.
Stephen has been participating in ICFP contests since 2006;
this was Peter's first contest.

The name "mi-goto" is a play on "Mi-go", Lovecraft's Yuggothian
fungi, who transported humans through the Æther via so-called
"brain cylinders."


## Running the Solution

The following dependencies are required for the Haskell and C++
programs, respectively:

* libghc-aeson-dev
* libjsoncpp-dev
* libboost-all-dev

Only the former is required to build the top-level Haskell binary.

In addition to the required flags, we also support the following:

* `--verbose`: outputs all the intermediate state along the way,
  providing a nice visualization of the game.
* `--tag TAG`: allows specifying the tag field for the solution.
* `--score`: outputs the score for solutions immediately after
  computing them.

While we accept the time, memory, and CPU count flags, we do not
actually do anything with them.  This means that if run with many
`-f` flags, a single long-running problem will result in no output
at all.  This is unfortunate, so we would humbly prefer that you
only run one problem at a time if the total run time is constrained.


## Our Approach

Our initial approach was to diversify, with one of us writing an
implementation in Haskell, and the other in C++, so that we could
try several approaches and learn what works.  Ultimately the
Haskell approach scored significantly better and was half as much
code (800 lines compared to 1800), at the cost of a significant
(orders of magnitude) speed penalty.

As we implemented the basic data structures, we developed some
tools to inspect different elements: `showboard` to display the
contents of a problem (printing out more elements as they developed),
`score` to run an output through a simulator and estimate the final
score (thus making us independent of the leaderboard for most tasks,
once we verified that the results were the same).

We quickly found the first several Phrases of Power hidden in the
problems: "Ei!", "Ia! Ia!", "R'lyeh", and "Yuggoth".  These we
verified by hand-submitting a simple solution with just the word
and a bunch of downward move commands (to ensure non-zero points
and thus detect other errors).  We also figured out the longest
phrase early on, though we had difficulty successfully integrating
it into any solutions due to its unweildy length.  The hint about
the Formless Spawn's master led us to a Lovecraft bestiary, and
after trying a few spellings we found "tsathoggua".  Eventually we
learned that the submission server accepted GET requests, which
allowed us to automate the search for power phrases.  We tried all
the Lovecraftian gods and other named entities to find "yogsothoth",
but nothing else.  The clues about the Johns and the letters and
digits proved elusive, and we wondered and marvelled at the teams
who were finding so many of them while still coding up solid
solutions in their own right.

Other useful tools included a script to run problems that stored
the current state of the source code and the command-line parameters
in a file next to the output (i.e. a `git rev-parse HEAD` and a
`git diff HEAD`).  Together with the score simulator, this allowed
us to easily figure out what caused any score regressions.  We also
write a quick script to combine our highest scoring qualifier
solutions into a single submission, allowing us to maximize our
qualifier score as much as possible.


## Our Solution

Algorithmically, we opted for a relatively naive solution.  The
basic idea was to heuristically score the possible end positions
for any given piece in order to choose the best option.  Rather
than picking a position first and then determining a path, we
simply did a depth-first search over all possible paths from the
starting position.  This worked well with the "no-repeats" rule
since we could immediately prune any already-visited state (we
defined a state as the pivot position (x, y), and the rotation,
which was an integer modulo the order of rational symmetry of the
current unit).

For the  phrases of power, we front-loaded the DFS queue with
all of the power words.  To ensure all the words were possible
(particularly because of the opportunistic pruning) it was
necessary to shuffle the list of words, though a uniform
distribution resulted in poorer scores because too-frequently
attempting the longer words interfered with the shorter ones.
We therefore used a weighted distribution 1:3:5:7:... so that
the longest words came up less often.

We tried a number of components for scoring the placement
heuristic.  The ones that ended up being the most successful
were:

* the awarded points according to the rules, including both
  points from the piece and from any words of power in the
  commands to maneuver it,
* a direction-dependent penalty for any unoccupoed spaces
  neighboring the unit (1:2:5 for neighboring on top, sides,
  or bottom),
* an additional penalty if any "holes" in the row beneath
  are covered, increasing as the lower row becomes more full,
* a bonus for moving the piece as low on the board as possible,
  and finally
* a bonus for adding blocks to a row with more blocks already
  in it.

We also tried a number of components that didn't end up
working well:

* a penalty for slanting the wrong direction: boards like
  #23 suggested that slanting upward toward the edges was
  generally a better habit (since it prevented creating
  awkward gaps in the middle), but this didn't end up
  working very well;
* a penalty for only eliminating a single line: the
  multi-line bonus is very valuable, but we were unable
  to find a way to correlate the gaps on neighboring
  lines in such a way as to make multi-line eliminations
  possible.

We found that the exhaustive search was particularly slow,
and that it spent most of its time in the least interesting
part of the exploration space: the wide-open areas with no
obstacles.  We therefore added an extra optimization to
maintain a list of empty rows and "fast-track" pieces through
these rows (rather than exploring the east/west directions),
until the piece was within its "radius" of a non-empty row.
In practice, any speed boost from this optimization was not
actually measurable.


## Final Thoughts

We found the programming part of the problem to be very
interesting and rewarding.  It was particularly rewarding
for the several hours when our naive algorithm had the top
score for problem #24, though eventually it was supplanted
and never quite recovered.

The search for hidden words of power, on the other hand,
was much more frustrating and we could have done without
it.  Hopefully we meet the qualification bar and therefore
won't be penalized for not being able to figure out the
silly puzzles.

It was surprising to see how poorly the computer was at
playing Tetris.  Watching it fill the pieces in, we could
see tons of rookie mistakes, and yet figuring out how to
teach it not to do that was unfathomably tricky.  On the
other hand, it's humbling to think that if Google's DeepMind
can master Breakout from 300 games *ex nihilo*, that a basic
(though ridiculously large) neural network could probably
outperform our heuristics without even trying.


## Thanks

We would like to thank the organizing committee for the
excellent work they put into this contest.  It was loads
of fun!
