Konfigurering
=============

· Bruk standard fonter i lynx.cfg
· Ikke bruk lynx.lss
· (setq shell-file-name "C:/emacs-22.1/bin/cmdproxy.exe")
· (setq explicit-shell-file-name shell-file-name) ;default is "C:/emacs-22.1/bin/cmdproxy.exe"
  (Men lilypond krever muligens at man bruker (setq shell-file-name (concat *cygwin* "bin/bash.exe"))


