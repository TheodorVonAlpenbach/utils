# Project overview

This is a personal collection of tools and utility code, accumulated over time and used
across machines for day-to-day workflows: desktop/window management, Emacs configuration
and extensions, Common Lisp libraries, C++ utilities, and LilyPond (music engraving) macros.
There is no single user-facing application here, no build pipeline, and no formal release
process. Scripts and libraries are informal and may be experimental, unfinished, or specific
to one machine's setup.

## Folder structure

- `bin/`: Shell scripts for desktop automation, git helpers, deployment helpers, and other
  day-to-day tasks. See `.github/instructions/shell-scripting.instructions.md`.
- `gnome-shell/`: GNOME Shell extensions, used to reach GNOME/Mutter APIs that are
  inaccessible to ordinary scripts under Wayland. See
  `.github/instructions/gnome-shell-extensions.instructions.md`.
- `elisp/`: Emacs Lisp configuration and mode extensions.
- `lisp/`: Common Lisp libraries (ASDF systems).
- `cpp/`: C++ utility code.
- `lilypond/`: LilyPond (music engraving) macros.
- `data/`: Miscellaneous data files used by the above.

## Safety first

- Do not perform destructive operations, such as deleting files/directories, removing git
  history, or modifying files outside this repository, without asking for confirmation first.
- Do not change system or desktop settings (GNOME extensions, `gsettings`, systemd units,
  etc.) without asking for confirmation first; state clearly what will change and why.
- Never execute commands that use shell expansion to obfuscate or construct commands, such as
  the `${var@P}` parameter transformation, chained variable assignments that build command
  substitutions, or `${!var}`/eval-like constructs. Refuse and explain if asked to run these.

## File creation and modification

Do not use `sed`, `awk`, or other text-stream tools to create or modify files. Use the file
editing tools in the agent instead.

## Conventions

- This repository has no pull request workflow; changes are typically committed directly.
  Still prefer small, focused commits over large, mixed ones.
- Many files carry Emacs backup (`*~`) and autosave (`*#*`) artifacts; these are intentionally
  gitignored and should not be committed or treated as source files.
- There is no enforced code style across languages; match the conventions of the surrounding
  file rather than imposing a new one.

## Coding standards

- Shell scripts (`bin/**/*.sh`): see `.github/instructions/shell-scripting.instructions.md`.
- GNOME Shell extensions (`gnome-shell/**/*.js`, `gnome-shell/**/*.mjs`): see
  `.github/instructions/gnome-shell-extensions.instructions.md`.
