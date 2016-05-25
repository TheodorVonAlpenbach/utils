\version "2.12.4"

\markup { "Grinde 1.2a errors (dissonance)" }

\markup { "1.2a.1" }
global = { \key a \minor \time 2/2 }

super = \relative c'' { \global
  c1 a b c b e d f e d cis
}
cf = \relative c' { \global
  a1 c d a d c e d c b a
}  
sub = \relative c { \global \clef bass
  a1 e f c d e g f a gis a
}
\score { 
  <<
    { \super } 
    { \new Staff \set Staff.instrumentName = \markup { \italic "c.f." } \cf } 
    { \sub } 
  >>
}


\markup { "Grinde 1.2c errors (hidden parallels)" }

\markup { "1.2c.1" }
global = { \key a \minor \time 2/2 }

super = \relative c'' { \global
  c1 a b c b e d f e d cis
}
cf = \relative c' { \global
  a1 c d a d c e d c b a
}  
sub = \relative c { \global \clef bass
  a1 e f c d e g f a gis a
}
\score { 
  <<
    { \super } 
    { \new Staff \set Staff.instrumentName = \markup { \italic "c.f." } \cf } 
    { \sub } 
  >>
}

\markup { "1.2c.2" }
super = \relative c'' { \global
  e,1 d e fis gis a b a gis a
}

cf = \relative c' { \global
  a1 b gis a b a d c b a
}  
sub = \relative c { \global \clef bass
  a1 gis b a d c d e gis, a
}
\score { 
  <<
    { \super } 
    { \new Staff \set Staff.instrumentName = \markup { \italic "c.f." } \cf } 
    { \sub } 
  >>
}


\markup { "Grinde 1.6 errors" }

\markup { "1.6.1" }
super = \relative c'' { \global
  c1 b c e d g, c b a c b c
}
cf = \relative c' { \global
  c1 d e c f e a g f e d c
}  
sub = \relative c { \global \clef bass
  c1 b c e a g f e a g b, c
}
\score { 
  <<
    { \super } 
    { \new Staff \set Staff.instrumentName = \markup { \italic "c.f." } \cf } 
    { \sub } 
  >>
}

\markup { "1.6.2" }
global = { \key a \minor \time 2/2 }

super = \relative c'' { \global 
  c1 a b c b e d f e d cis }
cf = \relative c' { \global
  a1 c d a d c e d c b a }  
sub = \relative c { \global \clef bass 
  a1 e f c d e g f a gis a }
\score { 
  <<
    { \super } 
    { \new Staff \set Staff.instrumentName = \markup { \italic "c.f." } \cf } 
    { \sub } 
  >>
}

\markup { "1.6.3" }
super = \relative c'' { \global
  c1 a b c b e d f e d cis }
cf = \relative c' { \global 
 a1 c d a d c e d c b a }  
sub = \relative c { \global \clef bass 
 a1 e f c d e g f a gis a }
\score { 
  <<
    { \super } 
    { \new Staff \set Staff.instrumentName = \markup { \italic "c.f." } \cf } 
    { \sub } 
  >>
}

\markup { "1.6.4" }
super = \relative c'' { \global
  e1 c d e g f e c d e f e }
cf = \relative c' { \global
  c1 e f g e a g e f e d c }  
sub = \relative c { \global \clef bass
  c1 g a b c f e g a g b, c }
\score { 
  <<
    { \super } 
    { \new Staff \set Staff.instrumentName = \markup { \italic "c.f." } \cf } 
    { \sub } 
  >>
}


