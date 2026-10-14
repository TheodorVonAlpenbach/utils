# Emacs Lisp brush-up plan

## Goal and scope

Make the code that is still useful easier to load, understand, test, and maintain.
Do not turn this personal toolbox into one large package or rewrite everything.
Treat each library or small application as a separate, bounded cleanup task.

This plan is based on repository inspection on 2026-10-14, including a read-only
Emacs 30.2 scan of all tracked `.el` files. No application code was loaded or
executed, and no existing tests were run. Findings are an initial inventory, not
a complete correctness review.

The tracked inventory contains 375 `.el` files, approximately 56,600 lines,
and 43 files with test-oriented names. These include third-party code, generated
data, and experiments; the amount worth actively maintaining should be smaller.
Backup/autosave files are not source and are outside the cleanup scope.

## Findings worth acting on

| Finding | Evidence | Proposed treatment |
| --- | --- | --- |
| Initialization and libraries are coupled | [`.emacs`](./.emacs) defines global Common Lisp aliases, assembles broad load paths, eagerly loads many features, and opens files. Libraries such as [`mb-lists.el`](./utils/mb-lists.el) use those aliases without declaring them. | Establish independent library loading before simplifying startup. Remove aliases only after migrating their callers. |
| Repeated definitions hide earlier implementations | The reader scan found 37 repeated top-level function/macro definitions. Examples include `nor` in [`mb-utils-div.el`](./utils/mb-utils-div.el) and `set-indent` in [`mb-indent.el`](./global/mb-indent.el). The duplicate `parse-csv-string` in [`mb-utils-io.el`](./utils/mb-utils-io.el) has since been consolidated. | Compare implementations and call sites, test the intended contract, and retain one implementation. Do not assume the earlier version is expendable. |
| Existing tests are a useful but inconsistent starting point | [`test-mb-utils-strings.el`](./utils/test-mb-utils-strings.el) declares its library dependency; [`test-mb-sequences.el`](./utils/test-mb-sequences.el) does not. [`test-mb-utils-file.el`](./utils/test-mb-utils-file.el) assumes a particular home-directory layout. | Make tests independent of a populated interactive session and the developer's machine. |
| Test discovery and execution need consolidation | [`mb-ert.el`](./mode-extensions/mb-ert.el) formerly excluded `mb-js-mode` filenames, repeated a runner definition, and ran only already-loaded tests. Discovery and the maintained manifest are now separate, though both [`ert-mb-utils-tree.el`](./utils/ert-mb-utils-tree.el) and [`test-mb-utils-tree.el`](./utils/test-mb-utils-tree.el) still need review. | Continue adding safe tests to the explicit manifest while keeping external-service/process tests opt-in. |
| Lexical binding needs a careful migration | Only 10 tracked files have a first-line `lexical-binding: t` header. [`mb-lists.el`](./utils/mb-lists.el) returns lambdas referencing local bindings, for example in `explicit<`. | Test returned functions after their creator returns. Migrate one module at a time; preserve intentional dynamic variables with `defvar` or `defcustom`. |
| Core helpers mix infrastructure with experiments | [`mb-utils-math.el`](./utils/mb-utils-math.el) includes experimental helpers after an initial `provide`. Load-time samples and Windows-only temporary paths have been removed from [`mb-utils-io.el`](./utils/mb-utils-io.el). | Separate demonstrations from reusable definitions and eliminate unintended load-time work. |
| Global setup contains conflicting or machine-specific bindings | [`global-map.el`](./global/global-map.el) binds F12 twice and includes a hard-coded document compilation command. [`mb-org.el`](./mode-extensions/mb-org.el) assumes Evil and key-chord functionality in its mode hook. | Preserve useful commands, make optional integrations explicit, and move machine-specific behavior out of shared setup. |
| Legacy API usage remains | [`php.el`](./c++/php.el) uses `defadvice`; [`qt-chess.el`](./c++/qt-chess.el) uses unprefixed `flet`. | If retained, replace with supported APIs after testing advice ordering and rebinding semantics. |
| Potential library-name collision | [`sql-mode.el`](./mode-extensions/sql-mode.el) contains personal extensions to built-in SQL mode. | Give the extension an unambiguous feature/file name and arrange setup after built-in SQL loads. |

### Files that fail the Emacs reader

The scan reads forms without evaluating them. These are reader failures, not
byte-compilation or test results:

| File | Location | Observed problem |
| --- | --- | --- |
| [`dict.el`](./dictionary/dict.el) | Line 1 | Stray escaping before the initial `require`; reader reports invalid syntax. |
| [`azure-REST.el`](./div/azure-REST.el) | Form beginning at line 29; error at line 61 | An unfinished implementation includes PHP-like text inside the Lisp source. |
| [`rotations.el`](./games/cube/rotations.el) | Line 68 | `#[...]` is read as a byte-code object, not an ordinary vector. |
| [`demo.el`](./music-analyzer/demo.el) | Line 192 | Unexpected closing parenthesis. |
| [`kommuner.el`](./quiz/kommuner.el) | Line 72 | Unexpected closing parenthesis. |

Fix these promptly if they are retained and used; otherwise classify them as
experiments or archive candidates. Do not spend substantial effort repairing
abandoned features merely to make a whole-tree scan green.

### Old or potentially obsolete code

Age is evidence for investigation, not proof that a feature is unused.

* **Strong retirement candidates:** Internet Explorer Favorites management in
  [`ie.el`](./browser/ie.el), which assumes `C:/Documents and Settings`; obsolete
  machine profiles among the [local initialization files](./); abandoned
  alternatives in [chess/old](./games/chess/old/); and unfinished scratch code.
  Confirm usage and references before archiving or deleting.
* **Platform-specific rather than automatically obsolete:** Notepad printing in
  [`mb-print.el`](./global/mb-print.el), Cygwin setup in [`.emacs`](./.emacs), and
  Windows temporary-file helpers in [`mb-utils-io.el`](./utils/mb-utils-io.el).
  Keep only the supported platforms, with explicit platform checks and portable
  temporary-file handling where appropriate.
* **Third-party maintenance decisions:** [download](./download/) contains copied
  libraries; [`scilab.el`](./external/scilab/scilab.el) has 2001/2009 origins and
  accounts for over 7,000 lines. Its last repository edit does not establish
  upstream freshness. Identify provenance, local patches, licenses, and current
  usage before choosing a maintained dependency, a documented vendor copy, or
  retirement. Keep this separate from first-party refactoring.
* **Historical documentation:** [`browser/README.txt`](./browser/README.txt)
  references Emacs 22.1, and the [Scilab notes](./external/scilab/readme.txt)
  describe a 2014 environment. Refresh retained instructions; label historical
  notes rather than treating them as current setup instructions.

## Implementation phases

### 0. Decide what to maintain

Classify modules as **active**, **occasional**, **experiment/archive**, or
**third-party/generated**. Record dependencies, external programs/data, supported
platforms, and whether loading a module changes global state or performs I/O.

Decisions needed before implementation:

* Which Emacs version is the minimum supported version?
* Are Windows/Cygwin and Scilab still required?
* Which tools, games, and project integrations are still used?

Start from the features loaded by [`.emacs`](./.emacs), but also inspect
interactive commands, hooks, local profiles, and manually loaded tools.
Static references alone cannot prove that personal interactive code is unused.

**Done when:** the first active subset is agreed, archive candidates are listed
without deleting them, and a few representative workflows are documented.

### 1. Establish a reproducible baseline

* Add a small batch ERT entry point using `emacs -Q --batch`, explicit repository
  load paths, and `ert-run-tests-batch-and-exit`.
* Initially list approved test files explicitly. Do not recursively load the
  entire tree: some modules and tests assume real databases, external files,
  programs, or an interactive configuration.
* Start with utility tests. Record load failures separately from assertion
  failures; make every test declare its subject library.
* Separate pure/unit tests from opt-in integration tests. For example,
  [`test-cram-db.el`](./games/cram/test-cram-db.el) expects a current database.
* Add a read-only syntax check and targeted byte-compilation for the active
  subset. Write compiled output to a disposable build location, not alongside
  source. Baseline existing warnings, then prohibit new ones in touched modules.
* Run each suite in a fresh process and repeat it to expose shared-state leaks.

**Done when:** one documented command discovers and runs the chosen tests without
the personal initialization file, reports a meaningful exit status, and lists
known exclusions explicitly.

### 2. Make the shared utility layer independently loadable

Work in small dependency groups, not one whole-directory patch.

1. Declare built-in and repository dependencies explicitly, including `cl-lib`
   and string helpers where needed. Resolve genuine dependency cycles by
   extracting only the small shared primitive that creates the cycle.
2. Characterize duplicate definitions, then consolidate them.
3. Move scratch expressions into tests or explicitly invoked demonstrations.
4. Make feature publication consistent, normally placing `provide` at the end.
5. Migrate old unprefixed Common Lisp calls and retain temporary compatibility
   aliases only until all affected callers are updated.
6. Add lexical binding module by module, reviewing closures, macros, and dynamic
   variables. Do not apply it mechanically across the directory.
7. Prefer built-in `seq`, string, buffer, and file helpers where contracts match.
   Keep custom helpers where they offer distinct semantics; avoid a mass rename.

**Test priorities:** empty/singleton inputs, boundary indices, mutation versus
copying, returned closures, macro single evaluation, Unicode strings, interval
endpoints, CSV quoting/empty fields/newlines, time zones and DST, and file paths.
Use temporary fixtures instead of real user files. Tests should preserve useful
existing contracts while making deliberate bug fixes explicit.

**Done when:** each migrated library loads in a clean process, its tests pass
without startup aliases, and byte-compilation introduces no new warnings.

### 3. Separate reusable code from startup and machine configuration

* Split [`.emacs`](./.emacs) into a small entry point and a few coherent setup
  modules, preserving load precedence and local overrides.
* Make load paths explicit instead of adding broadly discovered directories.
  Load machine variables before code that needs them, while retaining a clear
  distinction between early configuration and late overrides.
* Load optional modes and tools on demand using autoloads or deferred setup.
* Replace unintended load-time global changes with explicit setup functions,
  mode hooks, or minor modes. Make repeated setup idempotent.
* Review conflicting keys, hard-coded paths, old aliases, package setup, and
  outdated API usage. Do not redesign the user's keybindings as a side effect.

**Done when:** startup works with the default profile and a representative
supported machine profile; missing optional tools do not prevent unrelated work;
keybindings and representative editing workflows remain intact. Measure startup
time before and after, but do not optimize against an invented time target.

### 4. Brush up one application or integration at a time

Choose the most-used one first, for example [cram](./games/cram/),
[list-database](./list-database/), or a retained mode extension.

For each: document the entry points and data format, add characterization tests,
separate pure transformations from I/O, then refactor. Use temporary databases
and fake process/network boundaries for ordinary tests. Keep real-service tests
explicitly opt-in; never make a test run modify production data.

Treat [ADA](./projects/ada/), [music-analyzer](./music-analyzer/), and
[Scilab](./external/scilab/) as separate larger projects if retained. Each gets
its own bounded follow-up plan after its dependency and usage inventory.

**Done when:** the selected tool's important workflow is tested, errors remain
visible, and any data-format compatibility or migration requirements are covered.

### 5. Retire confirmed obsolete code and document the remainder

Archive or remove only approved unused code, adjusting references, hooks,
load paths, and documentation together. Obtain approval for destructive changes.
Document retained third-party sources and local modifications. Do not include
backup files or manually refactor generated tables.

Add concise usage/dependency notes for active modules and one top-level guide to
loading and testing. Keep historical experiments outside the active test/build
set without pretending they have been validated.

**Done when:** active entry points no longer reference retired modules and the
documentation describes the actual supported setup.

### 6. Keep the cleanup from regressing

Run syntax checks, the approved ERT subset, and targeted compilation before
committing changes. Add CI only if useful for this repository; a reliable local
command is sufficient initially. Test the declared minimum Emacs version as well
as the current version if more than one is supported.

Expand the maintained subset gradually. Prefer small commits with one behavior
change or mechanical migration at a time.

## Suggested first work packages

1. **Inventory and baseline:** classify startup dependencies and make a small
   utility ERT suite reproducible. No deletions or startup redesign.
2. **First utility migration:** select the smallest dependency group from the
   baseline, fix its loading dependencies, and add boundary/closure tests.
3. **I/O helpers:** consolidate CSV parsing and temporary-file behavior in
   [`mb-utils-io.el`](./utils/mb-utils-io.el), with characterization tests first.
4. **Test tooling:** consolidate discovery in [`mb-ert.el`](./mode-extensions/mb-ert.el)
   without losing source/test navigation or interactive test commands.
5. **Startup and keymaps:** tackle [`.emacs`](./.emacs) and
   [`global-map.el`](./global/global-map.el) after the utility baseline is stable.
6. **One active application**, followed by **approved retirement candidates**.

Phases 1-3 are dependent and should be sequential. Application cleanup can proceed
independently once its shared dependencies are stable; third-party provenance and
archive classification can be investigated alongside the baseline. Estimate each
work package after the baseline exposes its dependencies, rather than assigning
a speculative deadline to the entire tree.

## First work package baseline

The initial inventory and batch baseline were added on 2026-10-14. Run the
baseline from the repository root:

```sh
emacs -Q --batch -l elisp/tests/batch-tests.el
```

The command is independent of the current working directory after the entry
point has been found. It loads neither [`.emacs`](./.emacs) nor a machine-local
profile.

### Initial maintained test subset

The first subset is intentionally conservative:

* [`test-list-interval.el`](./test-list-interval.el): nine ERT tests for the
  closed-interval helpers, including construction and accessors.
* [`test-list-envelope.el`](./test-list-envelope.el): nine ERT tests for
  rectangle construction, validation, containment, overlap, intersection, and
  union.
* [`test-mb-utils-io.el`](./utils/test-mb-utils-io.el): ten ERT tests for text
  and binary file I/O, temporary-file cleanup, directory binding, CSV parsing,
  field iteration, and serialization.
* [`test-mb-ert.el`](./mode-extensions/test-mb-ert.el): nine ERT tests for test
  filename recognition, source/test name swapping, definition navigation, and
  ordered test discovery.
* [`test-mb-utils-file.el`](./utils/test-mb-utils-file.el): seven ERT tests for
  path construction, directory membership, readable-file lookup, filename
  transformations, parent directories, and recursive Scilab source copying.
* [`test-mb-utils-sets.el`](./utils/test-mb-utils-sets.el): three ERT tests for
  open/closed interval boundaries, one-dimensional spheres, and set equality.
* [`test-mb-utils-tree.el`](./utils/test-mb-utils-tree.el): six ERT tests for
  relation conversion, subtree filtering, leaf and member traversal, and
  independent tree copying.

[`list-interval.el`](./list-interval.el) and
[`mb-utils-sets.el`](./utils/mb-utils-sets.el) now declare their `cl-lib`
dependency, use lexical binding, and load independently of [`.emacs`](./.emacs).
Their test files require their subject libraries directly. The temporary
`first` and `second` aliases originally supplied by the batch runner have
therefore been removed.

[`list-envelope.el`](./list-envelope.el) is also independently loadable. Its
indentation property is declared directly instead of calling the startup-only
`cl-indent` helper, and its former dependencies on startup aliases have been
replaced with built-in operations.

### Startup inventory

[`.emacs`](./.emacs) currently establishes this sequence:

1. global editor settings and Common Lisp compatibility aliases;
2. selection of one of 16 tracked machine profiles, with
   [`.emacs-local-default`](./.emacs-local-default) as fallback;
3. broad load-path construction from repository subdirectories, local paths,
   installed packages, and selected nested projects;
4. eager loading of the features below;
5. loading the selected machine profile;
6. opening configured favorite files and loading additional packages such as
   `org-drill` and `flx-ido`.

| Eager feature | Current source | Initial classification |
| --- | --- | --- |
| `global-map` | [`global-map.el`](./global/global-map.el) | Active startup/keymap; global side effects |
| `mb-utils-io` | [`mb-utils-io.el`](./utils/mb-utils-io.el) | Active shared utility; startup-coupled |
| `mb-evil-map` | [`mb-evil-map.el`](./mode-extensions/mb-evil-map.el) | Active optional integration; global keymaps |
| `mb-things` | [`mb-things.el`](./utils/mb-things.el) | Active shared utility; broad dependencies |
| `radio-playlists` | [`radio-playlists.el`](./browser/radio-playlists.el) | Usage decision needed; external programs/network |
| `mb-lisp` | [`mb-lisp.el`](./mode-extensions/mb-lisp.el) | Active mode extension |
| `mb-python` | [`mb-python.el`](./mode-extensions/mb-python.el) | Active mode extension |
| `mb-octave` | [`mb-octave.el`](./mode-extensions/mb-octave.el) | Usage decision needed; external Octave process |
| `mb-ruby` | [`mb-ruby.el`](./mode-extensions/mb-ruby.el) | Usage decision needed |
| `mb-texinfo` | [`mb-texinfo.el`](./mode-extensions/mb-texinfo.el) | Usage decision needed |
| `list-db` | [`list-db.el`](./list-database/list-db.el) | Active shared application/library candidate |
| `mb-indent` | [`mb-indent.el`](./global/mb-indent.el) | Active startup compatibility layer; global symbol properties |
| `quiz-park` | [`quiz-park.el`](./quiz/quiz-park.el) | Usage decision needed; application |
| `mb-js-mode` | [`mb-js-mode.el`](./mode-extensions/mb-js-mode.el) | Active mode extension candidate |
| `quiz` | [`quiz.el`](./quiz/quiz.el) | Usage decision needed; application |
| `mb-ert` | [`mb-ert.el`](./mode-extensions/mb-ert.el) | Active developer tooling; refactor candidate |
| `mb-org` | [`mb-org.el`](./mode-extensions/mb-org.el) | Active mode extension; assumes optional Evil/key-chord setup |

The classifications above describe evidence from startup, not confirmed personal
usage. Static inspection cannot decide which optional integrations should remain
supported. The minimum Emacs version and the status of Windows/Cygwin and Scilab
also remain explicit decisions before retirement or compatibility work begins.

### Initial utility migrations

The closed-interval, envelope, set-helper, file, I/O, and tree modules are
independently loadable. Their validation comprises:

* a clean `emacs -Q` load of all six utility features and the ERT tooling;
* 53 passing ERT tests from the batch entry point;
* byte-compilation of the libraries, their tests, and the batch runner without
  warnings, with compiled output written outside the source tree.

The [`mb-utils-io.el`](./utils/mb-utils-io.el) migration consolidated its
duplicate CSV parser, removed load-time scratch expressions and startup helper
dependencies, and replaced Windows-only temporary paths. Character-separated
CSV parsing now retains every field, supports quoted separators and escaped
quotes, and reports unterminated quoted fields. The simple string-separator
behavior remains unchanged. The quote/escape state machine is isolated in a
single-field reader, leaving `parse-csv-line` as a short collecting loop.
Binary vector writing was also repaired and is covered by a range round-trip
test.

The maintained file list and load paths now live in
[`test-manifest.el`](./tests/test-manifest.el). Both the batch entry point and
the interactive `mb-ert-test-mb-elisp` command load that manifest. General test
discovery recognizes every `test-*.el` file; inclusion in the clean baseline is
a separate decision. For example,
[`test-mb-js-mode.el`](./mode-extensions/test-mb-js-mode.el) is correctly
recognized as a test file but remains outside the manifest because it invokes
an external JavaScript runtime.

[`mb-ert.el`](./mode-extensions/mb-ert.el) no longer relies on startup-provided
string, buffer, sequence, or definition-navigation helpers. Its duplicate
whole-project runner was consolidated, and the compatibility command
`elisp-test-all` delegates to the maintained runner.

[`mb-utils-file.el`](./utils/mb-utils-file.el) now uses built-in path and
recursive file APIs instead of startup aliases and the shell-oriented
`unix-find` helper. Directory membership checks respect path-component
boundaries, and recursive Scilab copying creates missing destination
directories and replaces older destination files. Its tests use disposable
directories rather than assuming that the home directory is under `/home`.

[`mb-utils-tree.el`](./utils/mb-utils-tree.el) now uses lexical binding and
declares its built-in dependencies. Its narrower redefinition of Emacs's
`copy-tree` has been removed, and startup-provided `flatten` and `aif` helpers
have been replaced with built-in operations. The maintained tests characterize
the existing subtree-filter result shapes and verify relation conversion and
copy independence. The older [`ert-mb-utils-tree.el`](./utils/ert-mb-utils-tree.el)
remains in place but outside the manifest pending a later duplicate-test
cleanup decision.
