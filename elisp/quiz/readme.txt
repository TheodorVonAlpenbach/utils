· q-file/q-buffer (quiz buffer or summary quiz buffer)
  q-buffer <-- (q-list)*

· q-list (entire quiz-buffer or region of summary buffer)
  q-list <-- (q-list-header q-question)*

· q-question (entity containing at least a q-item, separated by at
  least one blank line)
  q-question <-- (q-item newline)*
    
· q-item (either Q (mandatory), A, S by now; later I might add C
  for comment, T: for the durations of the validity)
  q-item <-- q-item-header q-item-text

· q-item-text (section after q-item-header)


I analogi med paragraph, sentence etc holder vi oss til bare en type
navigasjon, nemlig forward/backward. Navigasjonsklassen
beginning/end-of faller sammen med denne hvis vi ser bort fra
muligheten av å beholde relativ plass innen q-question. Men dette ser
ikke ut til å være noe særlig nyttig. Dessuten må beginning/end-of
defineres spesielt for q-question.

I utgangspunktet lages ingen interaktiv kommando for å gå helt i
begynnelsen av q-question

Standard filter
===============

Brukes til navigering i q-items. Default argument skal alltid være
alle q-item-typer. F eks navigerer quiz-forward-item uten argument til
til begynnelsen av neste q-item, uansett type. Hvis argumentet er "QA"
ignoreres alle q-items som ikke er A eller Q.

2006-03-15
Kutter ut alle hjelpefiler, schmukker alt inn i quiz.el. Lettere å
oppdage dumme duplikater.
