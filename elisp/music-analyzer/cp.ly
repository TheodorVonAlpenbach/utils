% ****************************************************************
% Start cut-&-pastable-section
% ****************************************************************

\version "2.12.4"

\score {

\relative c'' {
  \new PianoStaff <<
    \new Staff { \key g \major \time 4/4
                                <<
                                {b4 c d d | c b a a | b c d b | a a g2 \bar "||"}  
                                \\
                                {g4 g a g8 fis | e fis g4 fis fis | g g a g8 fis | e4 fis g2}
                                >>
                       }
    \new Staff { \key a \minor \clef bass
                                <<
                                {d4 e d8 c b4 | c d d d | d e d d | e d8 c b2}
                                \\
                                {g4 e fis g | a b8 c d4 d | g, e fis g | c, d g2}
                                >>
                       }
 
  >>
}
 
\midi {
  \context {
       \Score
       tempoWholesPerMinute = #(ly:make-moment 72 4)
     }
}
 
\layout {}

}

\markup { 
  \vspace #1
  B
}

\score {
 
\relative c'' {
  \new PianoStaff <<
    \new Staff { \key g \minor \time 3/4
                                <<
                                {d4 bes a | g2 fis4 | g ees' d | bes2. | d4 c bes | a bes a | g2. ~ | g4 }
                                \\
                                {g4 g fis | d c d | d g fis | d2. | bes'4 a g | a g fis | g8 f ees d ees4 | d4 }
                                >>
                                r2 \bar "||"
                       }
    \new Staff { \key g \minor \clef bass
                                <<
                                {bes4 d d8 c | bes4 g a | d c8 bes a4 | g2. | f'4 f8 ees d4 | d d d8 c | bes2 c4 | bes}
                                \\
                                {g,4 g d' | g ees d8 c | bes4 c d | g2. | bes4 f g | fis g d | ees2 c4 | g'}
                                >>
                                r2
                       }
 
  >>
}
 
\midi {
  \context {
       \Score
       tempoWholesPerMinute = #(ly:make-moment 72 4)
     }
}
 
\layout {}

}
 
\markup { 
  \vspace #1
  C
}

\score {
 
\relative c'' {
  \new PianoStaff <<
    \new Staff { \key f \major \time 4/4
                                <<
                                {f,4 g8 a bes4 a | g g f2 | c'4 bes a g | f2. }
                                \\
                                {c4 c d8 e f4 | g8 f e4 c2 | a'4 g f e | c2. }
                                >>
                                r4 \bar "||"
                       }
    \new Staff { \key g \minor \clef bass
                                <<
                                {a4 c bes c | d c a2 | f'4 d c c | a2. }
                                \\
                                {f4 e8 f g4 a | bes c f,2 | f4 g a8 bes c4 | f,2. }
                                >>
                                r4
                       }
 
  >>
}
 
\midi {
  \context {
       \Score
       tempoWholesPerMinute = #(ly:make-moment 72 4)
     }
}
 
\layout {}

}
 
% ****************************************************************
% end ly snippet
% ****************************************************************
