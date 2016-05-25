(defvar scilab-string-start-regexp "\\(^\\|[^]})a-zA-Z0-9_.']\\)"
 "Regexp used to represent the character before the string char '.
The ' character has restrictions on what starts a string which is needed
when attempting to understand the current context.")

;; To quote a quote, put two in a row, thus we need an anchored
;; first quote.  In addition, we don't want to color strings in comments.
(defvar scilab-string-end-regexp "[^'\"\n]*\\(['\"]['\"][^'\"\n]*\\)*['\"]"
 "Regexp used to represent the character pattern for ending a string.
The ' character can be used as a transpose, and can transpose transposes.
Therefore, to end, we must check all that goop.")

(defun scilab-font-lock-string-match-normal (limit)
 "When font locking strings, call this function for normal strings.
Argument LIMIT is the maximum distance to scan."
 (scilab-font-lock-string-match-here
  (concat scilab-string-start-regexp
      "\\(['\"]" scilab-string-end-regexp "\\)"
      "\\([^'\"]\\|$\\)")
  limit))

(defun scilab-font-lock-string-match-unterminated (limit)
 "When font locking strings, call this function for normal strings.
Argument LIMIT is the maximum distance to scan."
 (scilab-font-lock-string-match-here
  (concat scilab-string-start-regexp
      "\\(['\"][^'\"\n]*\\(['\"]['\"][^'\"\n]*\\)*\\)$")
  limit))

(defun scilab-font-lock-string-match-here (regex limit)
 "When font-locking strings, call this function to determine a match.
Argument REGEX is the expression to scan for.  Match 2 must be the string.
Argument LIMIT is the maximum distance to scan."
 (let (e)
   (while (and (re-search-forward regex limit t)
       (progn
         ;; This gets us out of a comment after the string.
         (setq e (match-end 2))
         (goto-char (match-beginning 2))
         (prog1
             (or (scilab-cursor-in-comment)
             (if (bolp) nil
               (save-excursion
                 (forward-char -1)
                 (scilab-cursor-in-string))))
           (goto-char e))))
     (setq e nil))
   (if (null e)
   nil
     (goto-char e)
     t)))

(defun scilab-font-lock-comment-match (limit)
 "When font-locking comments, call this function to determine a match.
Argument LIMIT is the maximum distance to scan."
 (let (e)
  (while (and (re-search-forward "\\(//[^\n]*\\)" limit t)
       (progn
         (setq e (match-end 1))
         (member (get-text-property (match-beginning 0) 'face)
             '(scilab-string-face
               scilab-unterminated-string-face))))
     (setq e nil))
   (if (null e)
   nil
     (goto-char e)
     t)))

;;font-lock-solo-keywords

(defvar scilab-font-lock-solo-keywords
  (list
  '("\\<\\(break\\|case\\|e\\(lse\\(\\|if\\)\\|ndfunction\\)\
\\|for\\|global\\|clear\\|clearglobal\\|if\\|return\\|while\\|pause\\|function\\|select\\|then\\|quit\\|exit\\|stop\\|abort\\|do\\|resume\\|help\\)\\>"
    (0 scilab-keyword-face)))
"Font lock of keywords that appear on a line by themselves."
)
;; font-lock keywords
(defcustom scilab-builtin-list
;;; From the file $SCI/routines/default/fundef
 '("%i_abs" "%i_cumsum" "%i_diag" "%i_matrix" "%i_max" "%i_maxi" "%i_min" 
"%i_mini" "%i_p" "%i_sum" "%i_tril" "%i_triu" "%msp_full" "%msp_spget" 
"CreateLink" "DestroyLink" "ExecAppli" "GetMsg" "Matplot" "NumTokens" 
"SendMsg" "WaitMsg" "abs" "addf" "addinter" "addmenu" "amell" "apropos" 
"argn" "arl2_ius" "ascii" "atan" "balanc" "bdiag" "besseli" "besselj" 
"besselk" "bessely" "bezout" "bfinit" "bitand" 
"blkfc1i" "blkslvi" "bool2s" "buttmag" "bvode" 
"c2dex" "c_link" "calerf" "call" "cd" "cdfbet" "cdfbin" "cdfchi" "cdfchn" 
"cdff"
"cdffnc" "cdfgam" "cdfnbn" "cdfnor" "cdfpoi" "cdft" "ceil" "champ" "champ1" 
"chdir" "chol" "clc" "clean" "clear" "clearfun" "clearglobal" "code2str"
"coeff" "color" "comp" "completion" "cond" "conj" "contour" "contour2d" 
"contour2di" "contr"
"convol" "convstr" "corr" "cos" "cumprod" "cumsum" "curblock" "dasrt" "dassl" 
"debug" "deff" "definedfields" "degree" "delbpt" "delete" "delip" "delmenu" 
"denom" "det" 
"diag" "diary" "diff" "disp" "dispbpt" "dlgamma" "double" "driver" "emptystr"  
"ereduc" "erf" "erfc" "errcatch" "errclear" "error" "exec" "execstr" 
"exists" "exp" 
"expm" "eye" "fadj2sp" "fec" "feval" "fft" "file" "filter" "find" 
"floor" "format" 
"fort" "fprintfMat" "freq" "fscanfMat" "fsolve" "fstair" "full" "funcprot"
"funptr" "gamma" "gammaln" "gca" "gcf" "geom3d" "getblocklabel" "getcwd" 
"getdate"
"getenv" "getf" "getfield" "getpid" "getscicosvars" "glist" "global" 
"grand" "grayplot" "gschur" "gsort" "gspec" "gstacksize" "havewindow" 
"help" "hess" "histplot" "horner" "host" "iconvert" "ieee" "imag" "impl" 
"inpnvi" "int" "int16" 
"int2d" "int32" "int3d" "int8" "interp" "intg" "intppty" "inttype" "inv"
"isdef" "iserror" "isglobal" "isreal" "kron" "ldiv" "ldivf" "legend" "length" 
"lib" 
"lines" "linspace" "link" "list" "load" "loadwave" "log" "log2" "log10" "lsslist" 
"lstcat"  
"ltitr" "lu" "ludel" "lufact" "luget" "lusolve" "lyap" "m6bandred" "m6bmatch" 
"m6busack" "m6cent" "m6chcm" "m6clique" "m6clique1" "m6compc" "m6compfc" 
"m6concom" "m6deumesh" "m6dfs" "m6dfs2" "m6diam" "m6dijkst" "m6dmtree" 
"m6edge2st" "m6findiso" "m6flomax" "m6floqua" "m6fordfulk" "m6frang" "m6hamil"
"m6hullcvex" "m6inimet" "m6johns" "m6kilter" "m6knapsk" "m6loadg"
"m6lp2tad" "m6lp2tau" "m6mesh2b" "m6meshmesh" "m6metasync" "m6ns2p" "m6p2ns"
"m6pcchna" "m6permuto" "m6prevn2p" "m6prevn2st" "m6prfmatch" "m6relax" 
"m6saveg" "m6sconcom" "m6showg" "m6showns" "m6showp" "m6ta2lpd" 
"m6ta2lpu" "m6tconex" "m6transc" "m6umtree" "m6umtree1" "m6visitor" "macr2lst" 
"matrix" "max" "maxi" "mclearerr" "mclose" "mean" "meof" "mfprintf" "mfscanf" 
"mget" "mgeti" "mgetstr" "min" "mini" "mlist" "mode" "mopen" "mprintf" "mput" 
"mputstr" "mscanf" "mseek" "msprintf" "msscanf" "mtell" "mtlb_mode" 
"mtlb_sparse" "mulf" "netclose" "netwindow" "netwindows" "newfun" "nnz" "norm" 
"numer" "ode" "odedc" "oldload" "oldsave" "ones" "optim" "or" "ordmmd" 
"param3d" "param3d1" "part" "phasemag" "pinv" "plot" "plot2d" "plot2d1" 
"plot2d2" "plot2d3" "plot2d4" 
"plot3d" "plot3d1" "poly" "ppol" "pppdiv" "predef" "print" "printf" "prod" 
"pvm_addhosts" "pvm_barrier" "pvm_bcast" "pvm_config" "pvm_delhosts" 
"pvm_error" "pvm_exit" "pvm_get_timer" "pvm_getinst" "pvm_gettid" "pvm_gsize" 
"pvm_halt" "pvm_joingroup" "pvm_kill" "pvm_lvgroup" "pvm_mytid" "pvm_parent" 
"pvm_recv" "pvm_recv_var" "pvm_reduce" "pvm_send" "pvm_send_var" 
"pvm_set_timer" "pvm_spawn" "pvm_spawn_independentIN_pvm" "pvm_start" 
"pvm_tasks" "pvm_tidtohost" "qpqpqp" "qr" "rand" "rank" "rat" "rcond" "rdivf" 
"read" "read4b" "readb" "readgif" "readmps" "readxbm" "real" "remez" "residu"
"resume" "return" "ricc" "rlist" "roots" "round" "rpem" "rref" "rtitr"
"save" "savewave" "scf" "schur" "sci_tree2" "sci_tree3" "sci_tree4" "sciargs"
"scicosim" "sctree" "semidef" "setbpt" "setfield" "setmenu" 
"setscicosvars" "sfact" "sfinit" "sign" "simp" "simp_mode" "sin" "size" "sort" 
"sparse" "spchol" "spcompack" "spec" "spget" "splin" "sprintf" "sqrt" "stacksize" 
"stdev" "str2code" "strcat" "strindex" "string" "strsubst" "subf" "subplot" "sum" 
"sva" "svd" "sylv" "symfcti" "syredi" "testmatrix" "timer" "tlist" "tr_zer" "tril" 
"triu" "type" "typename" "typeof" "uint16" "uint32" "uint8" "ulink" "unix" 
"unsetmenu"
"user" "var2vec" "variance" "varn" "vec2var" "what" "where" "whereis" "who" 
"winsid" "writb" "write" "write4b" "x_choose" "x_dialog" "x_mdialog" 
"x_message" "xarc" "xarcs" "xarrows" "xaxis" "xchange" "xchoicesi" "xclea" 
"xclear" "xclick" "xdel" "xend" "xfarc" "xfarcs" "xfpoly" "xfpolys" 
"xfrect" "xg2ps" "xget" "xgetech" "xgetfile" "xgetmouse" "xgraduate" "xgrid" 
"xinfo" "xinit" "xlfont" "xload" "xname" "xnumb" "xpause" "xpoly" "xpolys"
"xrect" "xrects" "xs2eps" "xs2fig" "xs2ps" "xsave" "xsegs" "xselect" "xset" 
"xsetech" "xstring" "xstringl" "xtape" "xtitle" "zeros"
;;; from the file "$SCI/routines/fraclab/fundef.fraclab
"FWT" "FWT2D" "IWT" "IWT2D" "Koutrouvelis" "McCulloch" "WTDwnHi" "WTDwnLo"
"alphagifs" "bbch" "beep" "binom" "cfg1d" "cwt" "fcfg1d" "fch1d" "fif"
"gifs2wave" "gifseg" "holder2d" "lepskiiap" "linearlt" "mcfg1d" "mch1d"
"mdfl1d" "mdzq1d" "mdzq2d" "monolr" "multim1d" "multim2d" "prescalpha"
"readgif" "reynitq" "sbinom" "sgifs" "sim_stable" "smultim1d" "smultim2d"
"stable_cov" "stable_sm" "stable_test" "wave2gifs"
;;; from the file $SCI/routines/tksci/fundef.tksci
"TK_DoOneEvent" "TK_EvalFile" "TK_EvalStr" "TK_GetVar" "TK_SetVar" "close"
"essai" "figure" "findobj" "gcf" "get" "getgvar" "opentk" "set" "setgvar"
"uicontrol" "uimenu")
 "List of Scilab Builtin functions. Not so elegant, but stable. It is taken
from 3  files: $SCI/routines/default/fundef and  $SCI/routines/default/fundef
This list admits further extensions"
 :group 'scilab-shell
 :type '(repeat (string :tag "Name: "))
)

(defcustom scilab-lib-tag-table-delimiter ":"
"I modified genlib function that it also generates table of correspondence of 
format '<scifile> <delimiter> function' saved in lib_table file.One can do the similar 
thing.  Besides my genlib  produces full correpodnence bin-file scilab-function for multy function files. "
 :group 'scilab-shell
 :type 'string
)

(defcustom scilab-lib-tag-table-name "lib_table"
"I modified genlib function that it also generates table of correspondence of 
format '<scifile>: function' saved in lib_table file.One can do the similar 
thing.  Besides my genlib  produces full correpodnence bin-file scilab-function for multy function files. "
 :group 'scilab-shell
 :type 'string
)


(defcustom scilab-user-keywords-list
'("demos" "info" "help" "doc" "apropos" "what" "whos" "end" "cd" "end" "clear" "load" "save" "getf" "getd" "make" "whereis" "whereami" "where" "break" "pause" "resume " "quit" "exit" "stop" "abort" "do" "xset" "xget" "deff")
"List of words a user wants to higlight as keywords"
 :group 'scilab-shell
 :type '(repeat (string :tag "Name: ")
))



(defvar scilab-path-type-regexp "[\'\" ]?[~/.$]/*[a-zA-Z0-9_./%$-]*"
"Regexp describing possible path string when we are working with disk. Should
be changed for MSDOS")


(defun scilab-make-regexp-from-builtin ()
"Make regexp from builtin list of strings"
(regexp-opt scilab-builtin-list 'words)
;(concat "\\<\\(" (mapconcat 'regexp-quote scilab-builtin-list "\\|")
;    "\\)\\>")
)

(defcustom scilab-libfunc-list-path (if (getenv "SCILIBFUNC") (getenv "SCILIBFUNC") (concat (if (getenv "SCIHOME") (getenv "SCIHOME") (getenv "HOME")) "/libfunc"))
 "*The path to the file where all library functions are listed. Need for efficient completion mechanism. You don't have to build this file, it will be done automatically"
 :group 'scilab
 :group 'scilab-setup
 :type 'string)


(defun scilab-make-regexp-from-libfunc ()
"Make regexp from libfunc file"
(if (null (file-exists-p scilab-libfunc-list-path))
  "\\<\\(genlib\\)\\>"
  (let ((currbuf (current-buffer)) (buff nil) (lst nil))
  (find-file scilab-libfunc-list-path)
  (setq buff (current-buffer))
  (setq lst  (split-string (buffer-substring-no-properties (point-min) (point-max))))
  (kill-buffer buff)
  (switch-to-buffer currbuf)
  (concat "\\<\\(" (mapconcat 'regexp-quote lst "\\|")
      "\\)\\(\\s-*[()=\n,;~]"
          "\\|\\s-+" scilab-valid-variable-name
          "\\|\\s-+" scilab-path-type-regexp "\\)"))))

(defvar scilab-font-lock-keywords
 (list
  ;; String quote chars are also used as transpose, but only if directly
  ;; after characters, numbers, underscores, or closing delimiters.
  '(scilab-font-lock-string-match-normal 2 scilab-string-face)
  ;; A string with no termination is not currently highlighted.
  ;; This will show that the string needs some attention.
  '(scilab-font-lock-string-match-unterminated
    2 scilab-unterminated-string-face)
  ;; Comments must occur after the string, that way we can check to see
  ;; if the comment start char has occurred inside our string. (EL)
  '(scilab-font-lock-comment-match 1 scilab-comment-face)
;   ;; General keywords
;   '("\\<\\(break\\|ca\\(se\\|tch\\)\\|e\\(lse\\(\\|if\\)\\|ndfunction\\)\
;\\|for\\|global\\|if\\|return\\|while\\|pause\\|function\\|select\\|then\\|quit\\|exit\\|stop\\|abort\\|do\\|resume\\)\\>"
;     (0 scilab-keyword-face))
  ;; The end keyword is only a keyword when not used as an array
  ;; dereferencing part.
  '("\\(^\\|[;,]\\)[ \t]*\\(end\\)\\b"
    2 (if (scilab-valid-end-construct-p) scilab-keyword-face nil))
  ;; The global keyword defines some variables.  Mark them.
  '("^\\s-*global\\s-+"
    ("\\(\\w+\\)\\(\\s-*=[^,; \t\n]+\\|[, \t;]+\\|$\\)"
     nil nil (1 scilab-variable-name-face)))
  '("\\<\\(ax\\(es\\|is\\)\\|figure\\|get\\|image\\|li\\(ght\\|ne\\)\\|\
patch\\|s\\(et\\(\\|color\\|font\\)\\|urface\\)\\|text\\|\
ui\\(cont\\(ext\\(\\|menu\\)\\|rol\\)\\|menu\\|\
\\(toggle\\|push\\)tool\\|toolbar\\)\\)\\>"
    (0 scilab-type-face))
;;;punctuation
  '("[],.;:[)(^~=-]"  0 scilab-type-face))
 "Expressions to highlight in Scilab mode.")

(defvar scilab-gaudy-font-lock-keywords
 (append
  scilab-font-lock-keywords
  scilab-font-lock-solo-keywords
  (list
   ;; defining a function, a (possibly empty) list of assigned variables,
   ;; function name, and an optional (possibly empty) list of input variables
   (list (concat "^\\s-*\\(function\\)\\>[ \t\n.]*"
         "\\(\\[[^]=()]*\\]\\|" scilab-valid-variable-name "\\)"
                 "[ \t\n.]*"
         "=[ \t\n.]*\\("
                 scilab-valid-variable-name
                 "\\)[ \t\n.]*"
         "(?\\("
                 "[^)]*"
                 "\\))?"
                 "\\s-*[,;\n//]")
     '(1 scilab-keyword-face append)
     '(2 scilab-variable-name-face append)
     '(3 scilab-function-name-face append)
     '(4 scilab-variable-name-face append))
   ;; defining a function, a function name without output
     (list (concat "^\\s-*\\(function\\)[ \t\n.]+\\("
                  scilab-valid-variable-name
                  "\\)[ \t\n.]*"
            "\\(([^)]*)\\)")
       '(1 scilab-keyword-face append)
       '(2 scilab-function-name-face append)
       '(3 scilab-variable-name-face append))

 ;; Pathalogy: only function name
     (list (concat "^\\s-*\\(function\\)[ \t\n.]+\\("
                  scilab-valid-variable-name "\\)")
       '(1 scilab-keyword-face append)
       '(2 scilab-function-name-face append))

   '("\\<\\(for\\)\\s-+\\(\\sw+\\)\\s-*=\\s-*\
\\(\\([^\n,;//(]+\\|([^\n//)]+)\\)+\\)"
     (1 scilab-keyword-face)
     (2 scilab-variable-name-face append)
     (3 scilab-constant-face append))
   ;; Items after a select statements are cool
   '("\\<\\(case\\|select\\)\\s-+\\({[^}\n]+}\\|[^,//\n]+\\)"
     (1 scilab-keyword-face) (2 scilab-constant-face))
   ;; How about a few scilab constants such as pi, infinity, and sqrt(-1)?
   ;; The ^>> is in case I use this in an interactive mode someday
   '("\\<\\(%eps\\|%[a-z]_[a-z]\\|%[a-z]_[a-z]_[a-z]\\|%[a-z]\\|%[A-Z]\\|%pi\\|%inf\\|Inf\\|%Inf\\|NaN\\|%nan\\|Nan\\|%Nan\\|ans\\|%i\\|%n\\|%tab\\|\\^>>\\)\\>"
     (1 scilab-constant-face))
   ;; Define these as variables since this is about as close
   ;; as scilab gets to variables
;;;    (list (concat "\\<" scilab-indent-past-arg1-functions "\\s-*")
;;;      '("(\\s-*\\(\\w+\\)\\s-*\\(,\\|)\\)" nil nil
;;;        (1 scilab-variable-name-face)))
;;; I want to see fields of tlist  highlighted
  (list (concat scilab-valid-variable-name "\\.\\(" scilab-valid-variable-name "\\)")
'(1 scilab-tlist-field-face append))))
 "Expressions to highlight in Scilab mode.")
(if scilab-highlight-builtin
 (setq scilab-gaudy-font-lock-keywords
   (append
    scilab-gaudy-font-lock-keywords
    (list
     (list (scilab-make-regexp-from-builtin)
       '(1 scilab-builtin-face append))))))

(if scilab-highlight-macros
 (setq scilab-gaudy-font-lock-keywords
   (append
    scilab-gaudy-font-lock-keywords
    (list
     (list (scilab-make-regexp-from-libfunc)
       '(1 scilab-macros-face append))))))




(defvar scilab-really-gaudy-font-lock-keywords
 (append
  scilab-gaudy-font-lock-keywords
  (list
   ;; Since it's a math language, how bout dem symbols?
   '("\\([<>~]=?\\|\\.[*^']\\|=+\\|\\<xor\\>\\|[-!^&|*+\\/~:]\\)"
     1 scilab-type-face)
   ;; How about references in the HELP text.
   (list (concat "^" scilab-comment-line-s "\\s-*"
         "\\(\\([A-Z]+\\s-*=\\s-+\\|\\[[^]]+]\\s-*=\\s-+\\|\\)"
         "\\([A-Z][0-9A-Z]+\\)\\(([^)\n]+)\\| \\)\\)")
     '(1 scilab-constant-face prepend))
   (list (concat "^" scilab-comment-line-s "\\s-*"
         "SEE ALSO\\s-+")
     '("\\([A-Z][A-Z0-9]+\\)\\([,.]\\| and\\|$\\) *" nil nil
       (1 scilab-constant-face prepend)))
   (list (concat "//\\s-*"
         "\\(\\$Revision: 1.3 $]+\\$\\)")
     '(1 scilab-constant-face prepend))
   ;; continuation ellipsis.
   '("[^.]\\(\\.\\.\\.*\\)\\([^\n]*\\)" 1 scilab-keyword-face)
;      (2 scilab-comment-face))
   ;; How about debugging statements?
   ;;'("\\<\\(db\\sw+\\)\\>" 1 'bold)
   ;;(make-regexp '("dbstop" "dbclear" "dbcont" "dbdown" "dbmex"
   ;;           "dbstack" "dbstatus" "dbstep" "dbtype" "dbup" "dbquit"))
;;    '("\\<\\(db\\(c\\(lear\\|ont\\)\\|down\\|mex\\|quit\\|
;;st\\(a\\(ck\\|tus\\)\\|ep\\|op\\)\\|type\\|up\\)\\)\\>" (0 'bold)))
   '("\\<\\(\\(set\\|del\\|disp\\)bpt\\)\\>" (0 'bold)))

  (if scilab-handle-scicos
      ;; Scicos functions,  a scicos user has to edit this.
      (list (list (concat "\\<\\(\\([sg]et_param\\|sim\\([gs]et\\)?\\|"
              "\\(mld\\|ss\\)[A-Z]\\w+\\)\\|"
              "\\(new\\|open\\|close\\|save\\|find\\)_system\\|"
              "\\(add\\|delete\\|replace\\)_\\(block\\)\\|"
              "scicos\\|bd\\(root\\|close\\)"
              "\\)\\>")
          '(1 scilab-scicos-keyword-face)))
    nil))
 "Expressions to highlight in Scilab mode.")

(defvar scilab-shell-font-lock-keywords
 (list
  ;; How about Errors?
  '("\\(!--error\\)\\s-+\\([^\n]+\n\\)\\(.+\n\\)" (1 scilab-warning-face t)(2 scilab-warning-face t) (3 scilab-warning-face t))
  ;; and line numbers
  '("^\\(at line\\)\\s-+\\([0-9]+\\)" (1 scilab-warning-face) (2 scilab-warning-face))
  ;; Warnings
  '("\\(Warning:?\\|Warnings:?\\|WARNING:?\\|WARNINGS:?\\)"
    1 scilab-warning-face prepend)
  '("\\(Error:?\\|Errors:?\\|ERROR:?\\|ERRORS:?\\)"
    1 scilab-warning-face prepend)
  ;; User beep things
  '("\\(\\?\\?\\?[^\n]+\\)"
    1 scilab-warning-face)
  ;; Useful user commands, but not useful programming constructs
  (if (fboundp 'regexp-opt)
    (list (concat "\\<\\(" (regexp-opt scilab-user-keywords-list) "\\)\\>")
	  '(1 scilab-keyword-face)))
  ;; Various notices
  '("S C I L A B (R)" 0 'underline)
  '("SCILAB (R)" 0 'underline)
  '("S c i l a b" 0 scilab-function-name-face)
  '("All Rights Reserved" 0 'italic)
  '("\\((c)\\s-+Copyright[^\n]+\\)"
    1 scilab-comment-face t)
  '("\\(Copyright (C)\\)\\s-+\\([^\n]+\\)"
    (1 scilab-function-name-face t) (2 scilab-variable-name-face t))
  '("\\(Version\\)\\s-+\\([^\n]+\\)"
    (1 scilab-function-name-face t) (2 scilab-variable-name-face t))
  '("\\(scilab-\\)\\([1-9]\\.?[0-9]?\\.?[0-9]?\\)"
    (1 scilab-comment-face t) (2 scilab-variable-name-face t)))
 "Additional keywords used by Scilab when reporting errors in interactive\
mode and displays varios messages.")


;; hilit19 patterns
(defvar scilab-hilit19-patterns
 '(("\\(^\\|[^//]\\)\\(//[ \t].*\\|//\\)$" 2 comment)
   ("\\(^\\|[;,]\\)[ \t]*\\(\
function\\|global\\|for\\|while\\|if\\|elseif\\|else\\|end\\(function\\)?\
\\|return\\|select\\|case\\|then\\|quit\\|exit\\|stop\\|abort\\|resume\\|break\\|do\\)\\b" 2 keyword)))

(defvar scilab-imenu-generic-expression
 '((nil
    "^\\s-*function\\>[ \t\n.]*\\(\\(\\[[^]]*\\]\\|\\sw+\\)[ \t\n.]*\
=\[ \t\n.]*\\)?\\(%?[a-zA-Z0-9#_]+\\)"
    3))
 "Expressions which find function headings in Scilab sci files.")
(defvar  scilab-contline-regexp "\\(\\.\\.+[ \t.]*\n\\)"
 "Regexp used to perform continuation on code lines.
It may be .. ... .. ... etc \n.")

;; May be excess, but ...
(defvar  scilab-output-function-regexp "\\(\\(\\[[^]]*\\]\\|%?[a-zA-Z0-9#_]+\\)[ \t]*=[ \t]*\\)"
"Regexp used to perform output of function like []= or foo= or nothing")

(defvar scilab-function-head-regexp (concat
"^\\s-*function\\>[ \t\n.]*"
 scilab-output-function-regexp "?"
 "[ \t\n.]*"
 "\\(%?[a-zA-Z0-9#_]+\\)")
 "Regular Expression for function head  in  sci files.")

(defvar scilab-function-end-regexp  "^\\s-*endfunction\\>")

(provide 'mbscilab-font-lock)
