Model

generalized-pitch (gpitch)
pitch | spitch

The following classes is based on generalized-pitch
movement
voice-group
voice
note

Pitch-paradigm:
pitch [p]
chrome [chr]
chrome-base [chrb]
octave [o]
interval [i]
pchord '(p1 p2...)
chord (chr1 chr2...)
nchord i.e. normalized-chord
chord-skeleton [chosk]
key

Spitch-paradigm:
spitch
spitch-class (set-pitch-class, spc)
sinterval
spchord (spitch1 spitch2...)
schord (spc1 spc2...)
nschord  i.e. normalized-spc-chord
schord-skeleton [schosk]



Key analysis

Definitions

True function: 
Given a key k, all chords containing only pitch classes contained in
k, has a /true function/ in k. The root, then, defines the function
name:

Root	Name	German
1	I	T
2	II	Ss
3	III	Ds/Tm
4	IV	S
5	V	D
6	VI	Ts
7	VII	Dm

Note that an altered chord does not have a true function, by
definition.  

Function:
Function is given by the root (which should never altered). This
applies to altered chords as well.

Function classes: 
All functions belongs to a function class, as follows: I, VI, III to
the tonic class, V, VII, III to the dominant class, IV, II to the
subdomimant class.

Example: Ab-C-F# as the secondary dominant (of G major) in C major
(ie. the "Italian") has function VII (Dm or even Dm_3b) which hence
belongs to the dominant class of functions.

Secondary dominant:
See http://en.wikipedia.org/wiki/Secondary_dominant

Secondary dominant chain:

In modulation. Instead of writing a secondary dominant pendulum like
this: (D) D (D) D etc, 
it is (in my opinion) clearer include the whole pendulum in the
parenthesis: (D T D) D
In fact, the parenthes should be /as long as possible/ ie. from the
first secondary dominant class chord to the last one, and with only
chords of the dominant and tonical function classes in between.

Modulations

Some types
1 Simple secondary dominants
  If the secondary tonic has a true function, this is not really
  considered to be a modulation, and in the analysis the tonicization
  should be indicated with parenthesis around the secondary dominant
  (in fact, around the whole secondary dominant chain, see above)

2 Cadenza modulation, true modulation
  A secondary subdominant breaks the secondary dominant chain. Because
  in this case, it is usually considered a true modulation.

3 Leap modulation 
  This is a true modulation, without preparatory cadenza or even
  dominant structure. This modulation typically occurs at the
  beginning of a new music segment, for instance a new phrase,
  a movement section like the trio section in marches, and, obviously,
  a new movement.


Modulation 1

k1: ... k2: S D T constellation, where S has a true function in k1


1. Identify split points, ie. positions where two or more different
   key could occur

Other stuff
===========

Dorian can typically be identified with clausure on dominant, at least
at hint to this modality shift could be provided.


Tree
====
 Goal: parse a Bach choral from MIDI and try to find the correct chrome values from set-pitch-classes

 1x find segmentation on schords of the movement (handle passing notes etc)
   * must somehow keep track the original durations
 2o from the chord segment list, construct a tonality tree (i.e. a hierachical segmentation on tonality)
 3o convert all schords in tree to chords. Since all intermediate chords should be either pure major or
    minor, the local tonality keys are trivially identified.
 4o print the resulting tree using dot
 5  how to give an ordinary Riemann analysis from tree? Once key is established the rest is simple:
    A. Only leaf nodes of the tree is by tree definition segmented chords, and only those are given 
    a Riemann function according to the tonality of their respective parent node. 
    B. Traverse tree and identify overlapping tonicizations. Use rules overlapping rules 
    (max, min-left, min-right) to settle overlapping function output 
    But how to establish key?
    Approach 1
    All strictly dominant nodes inherit tonality from closest clause node (subdominatal)
    Approach 2 
    Same as 1, but also considers
    a. Big dominant subtree (must define /big/)
    b. Abrupt chrome key changes
    c. Picardic tonicizations
    d. Church modes

 1 is difficult, but we start with a /dense segmentation/ (ref. Rohrmeier), 
   just to have something to show. We probably just discard non-chords at first.
   Later we could the /harmonic segmentation/ described by Rohrmeier
 
all D->T relations. Those could
 be T->SD but we may apply secondary rules to accomodate this

Example (from 2-6):
 (D g D g g D g D As cis0 D g D g)
stack:
 D
 D g
 (g* 
  (D g))
 (g* 
  (D g)) D g
 (g* 
  (D g)) (g* (D g))
 (g* 
  (g* (D g)) (g* (D g))) etc 

Example choral:
http://www.jsbchorales.net/down/pdf/000206.pdf
Example dot file:
c:/Documents and Settings/matsb/My Documents/data/dot/graph1.gv

2013-05-06
TODO
* consider cleaning up in chord, chordx, schord, schordx
  * let chordx replace chord and make it also a struct.
  * let schordx replace schord and make it also a struct.
  (defstruct chord skeleton root inversion duration) 
  (defstruct schord skeleton root inversion duration)

* for now, only add duration to chord, chordx, schord, schordx

* until Wednesday, try to avoid start time etc, instead rely on that
  segmentation is on
