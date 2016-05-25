* Scilab Debugger
=================

Scilab (https://help.scilab.org/) is not coming out of the box with a
debugger. However, it contains a few rudimenary functions, i.e. pausing,
resuming and stack examination.

Scipad (http://sourceforge.net/projects/scipad/), an editor for scilab
(not to be confused with Scinotes) contains a debugger GUI for Scilab.
But this does not (as of today, 2014-11-18) support 5.x.x-versions.

Since a visual debugger is very useful for understanding and
modifying existing code, I have decided to build one within Emacs. It
should at least contain the following functions:

+ Set/delete/run-to breakpoint
+ Evaluate scilab expressions
+ Jump to cursor (tempoary/soft breakpoint)

It would also be nice if it handled these functions
+ Step into functions called in a statement

* Implementation
================

Comint
Existing scilab mode

** Set breakpoint
This consists of two subtasks: 
+ setting the breakpoint in the scilab process (comint), and
+ showing the breakpoint in Emacs

*** Scilab
This is fairly easy. Scilab has a method
setbpt(FN, LINE) that does this straight away. The only problem is
that LINE is relative to FN, so Emacs needs to do some line
calculations in the code buffer.

Also, it seems that when scilab goes to line LINE, it also evaluates
this line straight away. So, when comparing with most other debugging
environments, the true line that scilab stops at, is the line after
LINE!.

NB! The implementaion of the debugger will respect the breakpoint
behavior of scilab. In order to set a break just before a statement is
evaluated, set the breakpoint on the line before.

** Step into
Since the Scilab debugger evaluates the statement before it stops at a
breakpoint line, in order to step into a function called in a
statement, this information must be given in advance before stopping
at that line. The implementation must therefore follow these steps:

'Step into next function call':
1. In Emacs:
   a. Delete the highlighting of current step line
   b. Find the next statement line, lineNS, (filtering comment and blank lines)
   c. Parse function name (the function to step into)
   d. If a function name was parsed and the corresponding code buffer
      could be succesfully opened
      I. go to this buffer,
         go to function's first line, lineFF, and 
	 set the highlighting on lineFF
     II. else, go to lineNS
   e. Remove soft breakpoints (i.e. bpt not in a hard-breakpoint-list)

Scilab's line classifications:
Physical line: Characters delimited by RET, \n, ^M and the like. I.e.
               it is what you see in an editor as a line
Logical line : A sequence of physical lines delimited by the
	       continuation mark '..'.

In addition, I will add
Last evaluation line
             : The physical line that was previously evaluated
Resume line  : The physical line from where the evaluation will
               resume. This is also the line that usually is
	       highlighted during an IDE's debugging session.

SciLab has an odd notion of stop lines. Specifying a visual stop point
with setbpt() is not trivial. setbpt() specifies last evaulation line,
not resume line. Consider the following if-block:

1  if (remainder == 0) then 
2    time = buffer(v);
3   elseif (remainder == 4) then
4    time(1:2:(2*sum(v)-1)) = buffer(v);
5  end

Suppose 

TODO
* when evaluating during debugging, step line is toggled off

* goto step line when it appears in some buffer
  applies to start of debugging session and when stepping into another function

* secure transactions. it now seems two requests could overlap in
time, i.e. the later is sent before the result of the former has been
completely received (and postprocessed).

* remove TEPPEABO-START/END

* display a scilab-ish prompt
  
* show soft breakpoints in left margin
  checked how to do this. The best is probably as in linum mode which
  uses overlays (with the 'before-string). Since we use linum-mode,
  one should locate existing overlay and add green background (or
  something)
  To make your own fringe indicator:
  http://emacs.stackexchange.com/questions/13134/emphasise-the-current-error-in-the-compilation-window
  And here is how to place arbitrary bitmaps in the fringe:
  (defun annotate-todo ()
  "put fringe marker on TODO: lines in the curent buffer

* nicer breakpoints, perhaps a big red background in the fringe
* nicer step line markings, perhaps a big green background in the fringe

o correctly read SciLab expression from minibuffer

* buffer is flickering when stepping. There is a snippet somewhere
  that is not using save-excursion properly
