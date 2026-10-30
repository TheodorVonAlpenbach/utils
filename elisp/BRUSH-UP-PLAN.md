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
| Legacy API usage remains | Archived [`php.el`](./archive/c++/php.el) uses `defadvice`; archived [`qt-chess.el`](./archive/c++/qt-chess.el) uses unprefixed `flet`. | Leave archived code unchanged unless it is restored to active use. |
| Potential library-name collision | Resolved: the former `sql-mode.el` personal extension is now [`mb-sql.el`](./mode-extensions/mb-sql.el), while Emacs's built-in SQL implementation remains `sql.el`. | Keep personal SQL commands namespaced and activate them lazily from `sql-mode-hook`. |

### Files that fail the Emacs reader

The scan reads forms without evaluating them. These are reader failures, not
byte-compilation or test results:

| File | Location | Observed problem |
| --- | --- | --- |
| [`dict.el`](./dictionary/dict.el) | Line 1 | Stray escaping before the initial `require`; reader reports invalid syntax. |
| [`azure-REST.el`](./div/azure-REST.el) | Form beginning at line 29; error at line 61 | An unfinished implementation includes PHP-like text inside the Lisp source. |
| [`rotations.el`](./games/cube/rotations.el) | Line 68 | `#[...]` is read as a byte-code object, not an ordinary vector. |
| [`demo.el`](./archive/music-analyzer/demo.el) | Line 192 | Unexpected closing parenthesis in archived code. |
| [`kommuner.el`](./quiz/kommuner.el) | Line 72 | Unexpected closing parenthesis. |

Fix these promptly if they are retained and used; otherwise classify them as
experiments or archive candidates. Do not spend substantial effort repairing
abandoned features merely to make a whole-tree scan green.

### Old or potentially obsolete code

Age is evidence for investigation, not proof that a feature is unused. Age may
also mean a utility now duplicates a standard function added in a later Emacs
release; those cases are tracked in the
[standard-library supersession register](#standard-library-supersession-register).

* **Archived code:** Internet Explorer Favorites management in
  [`ie.el`](./archive/browser/ie.el), which assumes `C:/Documents and Settings`,
  and the former browser, C++, and music-analyzer trees now live under
  [`archive/`](./archive/). Do not brush these up unless they return to active
  use. Other retirement candidates include obsolete
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
* **Historical documentation:** [`browser/README.txt`](./archive/browser/README.txt)
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

Treat [ADA](./projects/ada/) and [Scilab](./external/scilab/) as separate
larger projects if retained. The former music analyzer is now archived under
[`archive/music-analyzer/`](./archive/music-analyzer/). Each active project gets
its own bounded follow-up plan after its dependency and usage inventory.

**Done when:** the selected tool's important workflow is tested, errors remain
visible, and any data-format compatibility or migration requirements are covered.

### 5. Retire confirmed obsolete code and document the remainder

Archive or remove only approved unused code, adjusting references, hooks,
load paths, and documentation together. Obtain approval for destructive changes.
Document retained third-party sources and local modifications. Do not include
backup files or manually refactor generated tables.

Work through the
[standard-library supersession register](#standard-library-supersession-register)
here, honouring the recorded edge cases rather than substituting on name
similarity.

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

* [`test-backus-naur-mode.el`](./mode-extensions/test-backus-naur-mode.el):
  fifteen ERT tests for buffer-local mode setup, hooks, nonterminal indexing and
  links, comments, definition alignment, buffer-safe idle fontification,
  navigation-property refresh, isolated search overlays, static bindings, and
  completion and point-based definition jumps, plus parser, indentation, and
  definition-motion boundaries.
* [`test-mb-indent.el`](./global/test-mb-indent.el): four ERT tests for single
  and grouped indentation metadata, copied specifications, representative
  retained forms, upstream loop indentation, the global default, and actual
  Emacs Lisp indentation.
* [`test-global-map.el`](./global/test-global-map.el): four ERT tests for
  side-effect-free loading, explicit and repeated installation, namespaced
  prefix maps and Unicode insertion, retained bindings, removed machine
  shortcuts, and shell argument quoting.
* [`test-list-interval.el`](./prototypes/geometry/test-list-interval.el): nine ERT tests for the
  closed-interval helpers, including construction and accessors.
* [`test-list-envelope.el`](./prototypes/geometry/test-list-envelope.el): nine ERT tests for
  rectangle construction, validation, containment, overlap, intersection, and
  union.
* [`test-mb-utils-io.el`](./utils/test-mb-utils-io.el): ten ERT tests for text
  and binary file I/O, temporary-file cleanup, directory binding, CSV parsing,
  field iteration, and serialization.
* [`test-mb-utils-buffer.el`](./utils/test-mb-utils-buffer.el): fifteen ERT
  tests for buffer metadata, marks and regions, line navigation, editing,
  comments, word transformations, temporary files, save-as behavior, and the
  kill ring.
* [`test-mb-utils-regexp.el`](./utils/test-mb-utils-regexp.el): seven ERT
  tests for alternatives, integer alternatives, adjacent grouping, string
  factoring, unmatchable expressions, and exact finite-set complements.
* [`test-mb-utils-matrix.el`](./utils/test-mb-utils-matrix.el): eleven ERT
  tests for vector generation and arithmetic, geometric operations, matrix
  dimensions and products, norms, pseudoinverses, and permutations.
* [`test-mb-utils-physics.el`](./utils/test-mb-utils-physics.el): six ERT tests
  for physical constants, escape velocity, parsec and ordinary unit
  conversions, constant acceleration, and alcohol units.
* [`test-mb-utils-geometry.el`](./utils/test-mb-utils-geometry.el): seven ERT
  tests for one-dimensional objects, flexible and fixed-margin layouts,
  interspaces, affine transforms, extrema, and bounding boxes.
* [`test-mb-utils-search.el`](./utils/test-mb-utils-search.el): four ERT tests
  for searches in current, designated, and narrowed buffers, including point
  and match-data preservation.
* [`test-mb-utils-xml.el`](./utils/test-mb-utils-xml.el): seven ERT tests for
  attribute parsing and filtering, balanced nested-node extraction, directional
  limits, inner text, entity decoding, and temporary-buffer callbacks.
* [`test-mb-table.el`](./utils/test-mb-table.el): seven ERT tests for
  expression-aware row parsing, column widths, selected-region and paragraph
  alignment, explicit offsets, and malformed tables.
* [`test-mb-polynomials.el`](./utils/test-mb-polynomials.el): nine ERT tests
  for representation, arithmetic, long division, derivatives, evaluation,
  formatting, expansion, and polynomial fractions.
* [`test-mb-ert.el`](./mode-extensions/test-mb-ert.el): fourteen ERT tests for
  test filename recognition, source/test name swapping, definition navigation,
  ordered and region test discovery, selector normalization, keymaps, and
  invalid target reporting.
* [`test-mb-evil-map.el`](./mode-extensions/test-mb-evil-map.el): four ERT
  tests for clean loading without Evil, unavailable-package deferral, explicit
  dependency validation, and mocked prefix-map installation.
* [`test-mb-doc-view.el`](./mode-extensions/test-mb-doc-view.el): three ERT
  tests for standard page-command preservation, whole-document navigation
  order, and keybindings.
* [`test-mb-emacs-lisp.el`](./mode-extensions/test-mb-emacs-lisp.el): five ERT
  tests for optional keymaps, validated Common Lisp form rewriting,
  definition navigation, forced movement, and missing definitions.
* [`test-mb-json.el`](./mode-extensions/test-mb-json.el): five ERT tests for
  outer JSON-string decoding, bare timestamp repair, pretty printing, Mongo
  shell value conversion, and invalid input.
* [`test-mb-lilypond.el`](./mode-extensions/test-mb-lilypond.el): four ERT
  tests for quoted compilation commands, source tracking, successful PDF
  display, and narrow optional LilyPond command integration.
* [`test-mb-metafont.el`](./mode-extensions/test-mb-metafont.el): four ERT
  tests for compilation-output parsing, process status, GF-to-DVI conversion,
  and the compile-convert-display pipeline.
* [`test-mb-octave-insert.el`](./mode-extensions/test-mb-octave-insert.el):
  five ERT tests for insertion keymaps, function delegation, indented
  control-flow skeletons, explicit unsupported commands, and documentation
  template expansion.
* [`test-mb-octave-eval.el`](./mode-extensions/test-mb-octave-eval.el): six
  ERT tests for quoted source paths, expression parsing, process output,
  response timeouts, comment evaluation, and last-expression results.
* [`test-mb-octave-matlab.el`](./mode-extensions/test-mb-octave-matlab.el):
  eight ERT tests for chained assignments, default arguments, assertion and
  test-block conversion, condensed colon expressions, and explicit directory
  registration, plus integrated buffer transformations.
* [`test-mb-octave.el`](./mode-extensions/test-mb-octave.el): seven ERT tests
  for buffer-local setup, mode-buffer discovery, function parsing, quoted test
  commands, directory runners, and Texinfo conversion.
* [`test-octave-breakpoint.el`](./mode-extensions/test-octave-breakpoint.el):
  seven ERT tests for buffer markers, function locations, dbstop parsing and
  composition, buffer synchronization, temporary-breakpoint cleanup, and
  navigation.
* [`test-octave-debug-mode.el`](./mode-extensions/test-octave-debug-mode.el):
  seven ERT tests for independent keymaps, source markers, debugger protocol
  parsing, cross-buffer exit, session startup, display refresh, and command
  dispatch.
* [`test-mb-java-mode.el`](./mode-extensions/test-mb-java-mode.el): one ERT
  test for feature publication, Java buffer settings, idempotence, and global
  default isolation.
* [`test-mb-js-mode.el`](./mode-extensions/test-mb-js-mode.el): five ERT tests
  for JavaScript buffer settings, Node REPL hook registration and buffer
  lookup, mocked evaluation, and explicit missing-package errors.
* [`test-mb-org.el`](./mode-extensions/test-mb-org.el): five ERT tests for
  timestamp formatting, date/time/fact insertion, arrival/departure entries,
  keymaps, and hook registration.
* [`test-mb-python.el`](./mode-extensions/test-mb-python.el): three ERT tests
  for idempotent local setup, expression bounds, and nested Python defun bounds.
* [`test-mb-ruby.el`](./mode-extensions/test-mb-ruby.el): one ERT test for
  buffer-local Ruby settings, idempotence, and global-state isolation.
* [`test-mb-sql.el`](./mode-extensions/test-mb-sql.el): four ERT tests for
  ordinary and comment-prefixed line evaluation, empty input, and buffer-local
  key setup.
* [`test-mb-texinfo.el`](./mode-extensions/test-mb-texinfo.el): five ERT tests
  for command formatting and insertion, commented snippets, generated wrapper
  commands, keymaps, and hook registration.
* [`test-mb-utils-file.el`](./utils/test-mb-utils-file.el): seven ERT tests for
  path construction, directory membership, readable-file lookup, filename
  transformations, parent directories, and recursive Scilab source copying.
* [`test-mb-utils-sets.el`](./utils/test-mb-utils-sets.el): three ERT tests for
  open/closed interval boundaries, one-dimensional spheres, and set equality.
* [`test-mb-utils-tree.el`](./utils/test-mb-utils-tree.el): six ERT tests for
  relation conversion, subtree filtering, leaf and member traversal, and
  independent tree copying.
* [`test-mb-things.el`](./utils/test-mb-things.el): six ERT tests for generated
  thing recognition and movement, date/time/number increments, cyclic values,
  and coordinated weekday/date editing.
* [`test-mb-utils-economy.el`](./utils/test-mb-utils-economy.el): five ERT tests
  for growth, deposits, withdrawals, annualized profit, and basic financial
  helpers.
* [`test-mb-math-functions.el`](./utils/test-mb-math-functions.el): six ERT
  tests for degree and hyperbolic functions, finite and infinite series,
  special functions, polynomial roots, and numerical root solvers.
* [`test-mb-lists.el`](./utils/test-mb-lists.el): twenty-nine ERT tests for
  list generation, grouping, products, ordering, partitioning, repetition
  detection, rotation, padding, sorted structures, and list transformations.
* [`test-mb-sequences.el`](./utils/test-mb-sequences.el): twelve ERT tests for
  sequence type preservation, selection, splitting, drawing, positions,
  minima, projection, centering, insertion, and replacement.
* [`test-mb-utils-iso-time.el`](./utils/test-mb-utils-iso-time.el): six ERT
  tests for ISO date, time, timezone and date-time formatting, numbered regexp
  captures, localized calendar names, and supported time designators.

[`list-interval.el`](./prototypes/geometry/list-interval.el) and
[`mb-utils-sets.el`](./utils/mb-utils-sets.el) now declare their `cl-lib`
dependency, use lexical binding, and load independently of [`.emacs`](./.emacs).
Their test files require their subject libraries directly. The temporary
`first` and `second` aliases originally supplied by the batch runner have
therefore been removed.

[`list-envelope.el`](./prototypes/geometry/list-envelope.el) is also independently loadable. Its
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
| `global-map` | [`global-map.el`](./global/global-map.el) | Active startup keymap; independently loadable with explicit activation |
| `mb-utils-io` | [`mb-utils-io.el`](./utils/mb-utils-io.el) | Active shared utility; startup-coupled |
| `mb-evil-map` | [`mb-evil-map.el`](./mode-extensions/mb-evil-map.el) | Active optional integration; independently loadable with deferred global keymaps |
| `mb-things` | [`mb-things.el`](./utils/mb-things.el) | Active shared utility; independently loadable |
| `radio-playlists` | [`radio-playlists.el`](./archive/browser/radio-playlists.el) | Archived; external programs/network |
| `mb-lisp` | [`mb-lisp.el`](./mode-extensions/mb-lisp.el) | Active mode extension |
| `mb-python` | [`mb-python.el`](./mode-extensions/mb-python.el) | Active mode extension |
| `mb-octave` | [`mb-octave.el`](./mode-extensions/mb-octave.el) | Active mode extension; independently loadable with mocked process tests |
| `mb-ruby` | [`mb-ruby.el`](./mode-extensions/mb-ruby.el) | Active mode extension; independently loadable |
| `mb-texinfo` | [`mb-texinfo.el`](./mode-extensions/mb-texinfo.el) | Active mode extension; independently loadable |
| `list-db` | [`list-db.el`](./list-database/list-db.el) | Active shared application/library candidate |
| `mb-indent` | [`mb-indent.el`](./global/mb-indent.el) | Active indentation metadata; independently loadable with intentional global properties |
| `quiz-park` | [`quiz-park.el`](./quiz/quiz-park.el) | Usage decision needed; application |
| `mb-js-mode` | [`mb-js-mode.el`](./mode-extensions/mb-js-mode.el) | Active mode extension; optional Node.js evaluation |
| `quiz` | [`quiz.el`](./quiz/quiz.el) | Usage decision needed; application |
| `mb-ert` | [`mb-ert.el`](./mode-extensions/mb-ert.el) | Active developer tooling; independently loadable |
| `mb-org` | [`mb-org.el`](./mode-extensions/mb-org.el) | Active mode extension; independently loadable |

The classifications above describe evidence from startup, not confirmed personal
usage. Static inspection cannot decide which optional integrations should remain
supported. The minimum Emacs version and the status of Windows/Cygwin and Scilab
also remain explicit decisions before retirement or compatibility work begins.

### Initial utility migrations

The closed-interval, envelope, set-helper, file, I/O, tree, thing-at-point,
economy, additional-math, general-math, combinatorics, buffer, regexp, matrix,
physics, geometry, search, XML, anaphora, general, list, sequence, string,
time, ISO-time, and table modules are independently loadable. Their validation
comprises:

* a clean `emacs -Q` load of all forty-nine utility and mode-extension
  features plus the ERT tooling;
* 439 passing ERT tests from the batch entry point;
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
a separate decision. External-service and process boundaries remain mocked in
the maintained baseline.

[`mb-ert.el`](./mode-extensions/mb-ert.el) no longer relies on startup-provided
string, buffer, sequence, or definition-navigation helpers. Its duplicate
whole-project runner was consolidated, and the compatibility command
`elisp-test-all` delegates to the maintained runner. The remaining duplicate
symbol runner now aliases the canonical implementation, region testing is
implemented and exposed in the command map, selectors are deduplicated and
validated, and unvisited buffers, unreadable files, empty definitions, and
empty selections report explicit user errors. Fourteen tests cover discovery,
navigation, command dispatch, and failure boundaries.

[`mb-python.el`](./mode-extensions/mb-python.el) now loads independently with
lexical binding and the built-in Python library as its only dependency. Its
mode hook enables line numbers idempotently instead of toggling them each time.
Expression bounds preserve point and include complete function calls and
subscripts, while empty buffers signal an explicit error. Python-aware
navigation replaces the generic startup-only `bod*` and `eod*` helpers so
nested function bounds are selected correctly. Its three namespaced tests run
in the maintained baseline without starting a Python process.

[`mb-ruby.el`](./mode-extensions/mb-ruby.el) now loads independently with
lexical binding and the built-in Ruby mode as its only dependency. Its hook
uses buffer-local settings instead of leaking `evil-symbol-word-search` into
every buffer. The explicit underscore syntax-table mutation was removed
because modern `ruby-mode` already assigns the requested symbol syntax. Its
namespaced regression test also verifies repeated setup and global-state
isolation.

[`mb-indent.el`](./global/mb-indent.el) now loads independently with lexical
binding and explicit `cl-lib` and `cl-indent` dependencies. The duplicate
`set-indent` proposal, redundant `cl-indent` reload, dead scratch forms, and
private override advice were removed. In particular, the copied loop indenter
predated Emacs 30's corrected backquote handling and called obsolete private
functions; loop indentation now remains with upstream `cl-indent`. The retained
module declaratively registers personal macro specifications and intentionally
sets `common-lisp-indent-function` as the global Lisp indentation default.
Static `concat` and `format` metadata replaces mutation on every indentation
request. Four deterministic tests cover metadata and resulting indentation.

[`mb-evil-map.el`](./mode-extensions/mb-evil-map.el) now loads independently
with lexical binding when Evil is absent. Global Evil and key-chord mutations
are contained in `mb-evil-map-setup`; automatic activation first checks that
Evil is installed, while explicit setup reports missing integrations instead
of partially installing maps. Evaluation and clipboard prefix maps now have
namespaced variables with aliases for their former star names. Optional and
personal commands are declared without being eagerly loaded, and the existing
bindings remain installed on configured Evil systems. Four deterministic tests
mock the Evil/key-chord registration boundary.

[`global-map.el`](./global/global-map.el) now loads independently with lexical
binding and does not mutate global keys until `mb-global-map-setup` is called.
Startup invokes that setup explicitly after loading its feature list. Prefix
maps have namespaced variables with aliases for their former generic names,
Unicode insertion no longer depends on macro expansion from `mb-insert`, and
anonymous window, buffer, Lisp, and increment commands have named
implementations. The duplicate F12 binding and its course-specific TeX
filename were removed; no generic F12 replacement was selected. General
bindings, including the optional Lynx command, remain as symbolic bindings and
therefore do not eagerly load their implementations. `gfind` arguments are now
shell-quoted, and buffers without filenames no longer break extension
selection. Four tests install bindings into isolated keymaps.

[`mb-texinfo.el`](./mode-extensions/mb-texinfo.el) now loads independently
with lexical binding and built-in dependencies. The stale capitalized
`Texinfo-mode-hook` was replaced with the real `texinfo-mode-hook`; entering
Texinfo mode no longer requires Evil or key-chord, though their maps are still
configured when available. Startup-only region, string, process, line-motion,
and tag helpers were replaced with supported APIs. Personal wrapping commands
now use `mb-texinfo-` names rather than shadowing Emacs's standard
`texinfo-insert-@var`, `texinfo-insert-@code`, and related functions with
narrower signatures. Commented environment snippets now honor their
no-inner-space argument, process failures are surfaced, and the previously
dormant test file has been expanded to five namespaced tests in the maintained
baseline.

[`mb-js-mode.el`](./mode-extensions/mb-js-mode.el) now loads independently
with lexical binding and built-in editing dependencies. The optional
`nodejs-repl` package is loaded only when JavaScript evaluation is requested;
its absence now produces an explicit user error without preventing ordinary
JavaScript editing. Duplicate send implementations, manual process polling,
prompt scraping, and startup-only buffer/string helpers were replaced with the
package's synchronous execution API. The REPL setup function is now registered
on `nodejs-repl-mode-hook` rather than the mode symbol itself, JavaScript
settings are explicitly buffer-local and idempotent, and `js-eval-string`
returns its result even when also displaying it. Its five namespaced tests mock
the process boundary and do not start Node.js.

[`mb-org.el`](./mode-extensions/mb-org.el) now loads independently with lexical
binding and built-in dependencies. Its mode hook always restores graphical TAB
to `org-cycle`, while Evil/key-chord setup is applied only when both optional
facilities are available. Startup-only `iso-date`, `iso-time`, `now`,
`add-etime-time`, `bol`, `eol`, and `eob` dependencies were replaced with
built-in time formatting/arithmetic and direct buffer operations. Timestamp
insertion now has explicit point behavior, arrival entries normalize trailing
blank lines, and a supplied arrival time controls both the date and clock line
instead of potentially mixing dates around midnight. Its five namespaced tests
run without loading Evil or key-chord.

[`mb-java-mode.el`](./mode-extensions/mb-java-mode.el) now loads independently
with lexical binding and publishes its actual `mb-java-mode` feature. It
previously provided `mb-js-mode`, which made `(require 'mb-java-mode)` fail and
could falsely mark the JavaScript extension as loaded. The unsupported
`java-indent-level` assignment was replaced with CC Mode's buffer-local
`c-basic-offset`; line-number and electric-pair setup is now idempotent, and
all personal indentation settings remain local to Java buffers. Its namespaced
regression test also verifies that global defaults are unchanged.

[`mb-doc-view.el`](./mode-extensions/mb-doc-view.el) now loads independently
with lexical binding and explicit built-in dependencies. Its empty
zero-argument `doc-view-goto-page` definition was removed because it shadowed
Emacs's functional standard command, which requires a page argument. The two
retained commands compose standard first/last-page navigation with image
top/bottom movement, and their existing `C-M-<`/`C-M->` bindings are covered by
three namespaced regression tests.

[`mb-emacs-lisp.el`](./mode-extensions/mb-emacs-lisp.el) now loads
independently with lexical binding and the maintained ERT helper as its only
repository dependency. The unrelated ADA/MySQL map and Evil/key-chord binding
are optional instead of blocking ordinary Emacs Lisp editing. Startup-only
form, word, buffer, and definition helpers were replaced with syntax parsing
and built-in defun motion. `cl-ify-form` and `cl-ify-defun` now avoid duplicate
prefixes and reject heads that have no real `cl-lib` counterpart rather than
silently creating invalid names such as `cl-defvar`. `elisp-goto-defun`
preserves point when a definition is absent, quotes symbol names, and now
honors its documented force-to-beginning flag. Its five namespaced tests run
without the ADA or Evil integrations.

[`mb-json.el`](./mode-extensions/mb-json.el) now loads independently with
lexical binding and explicit built-in `cl-lib` and `json.el` dependencies.
JSON strings containing serialized JSON are decoded with the standard parser
rather than manual quote removal, and malformed decoded input is rejected
before formatting. The local repairs for bare UTC timestamps and Mongo shell
`ObjectId`/`ISODate` forms remain because built-in JSON formatting does not
accept those nonstandard inputs. Bare timestamps are quoted only in value
positions and existing JSON strings are left unchanged. Its five namespaced
tests cover decoding, timestamp repair, formatting, Mongo conversion, and
invalid input.

[`mb-lilypond.el`](./mode-extensions/mb-lilypond.el) now loads independently
without the optional LilyPond Emacs mode. Compilation filenames are shell
quoted and stored as buffer-local compilation state instead of being recovered
by parsing a hard-coded `*compilation*` buffer. The finish hook acts only on
successful LilyPond compilations with existing PDF output, refreshes an
existing PDF buffer without killing it, and remains deterministic under
concurrent compilation buffers. The copied upstream `LilyPond-command`
implementation was replaced with narrow advice installed only after
LilyPond mode loads; it changes the personal PDF `View` behavior and delegates
all other commands unchanged. Its four namespaced tests mock compilation and
display boundaries.

[`mb-metafont.el`](./mode-extensions/mb-metafont.el) now loads independently
with lexical binding and an explicit built-in `cl-lib` dependency. Compilation
starts from an empty output buffer, checks the `mf` process status, and rejects
missing or unrecognizable output instead of returning warning-shaped values or
parsing stale text. GF-to-DVI conversion runs in the source directory, checks
the converter status, and returns an absolute DVI filename. Buffer evaluation
requires a visited file, preserves the historical quiet intermediate
compilation, and refreshes an existing DVI buffer without killing it. Its four
namespaced tests mock both external programs and display operations.

[`mb-octave-insert.el`](./mode-extensions/mb-octave-insert.el) now loads
independently with lexical binding and the built-in Octave mode. Its if/else
and for-loop skeletons use ordinary indentation rather than Evil, track their
editing position through indentation changes, and no longer contain the stray
symbol that made for-loop insertion fail. Documentation templates are located
relative to the library, use built-in date formatting, and derive the Octave
function name from the visited `.m` filename instead of the removed
`octave-defun-name` helper. The unfinished case command now reports an explicit
user error. Its five namespaced tests do not start an Octave process.

[`mb-octave-eval.el`](./mode-extensions/mb-octave-eval.el) now loads
independently with lexical binding and explicit built-in dependencies. Source
commands quote filenames, require a visited file, and retain optional help
integration when the main Octave extension supplies it. Expression extraction
handles both ordinary code and commented test expressions without startup
buffer helpers. Inferior-process evaluation resets output per submitted line,
returns deterministic combined output, and has a configurable timeout instead
of waiting forever. Current-line evaluation rejects empty comments and retains
the debugger-facing last-expression variable through a compatibility alias.
The old prefix branches of `octave-eval-last-sexp` depended on the absent
`octave-eval-print-last-sexp`; they now report that unsupported request
explicitly instead of failing with an undefined-function error. Its six
namespaced tests mock the inferior Octave boundary.

[`mb-octave-matlab.el`](./mode-extensions/mb-octave-matlab.el) now loads
independently with lexical binding and built-in dependencies. Machine-specific
Octave/MATLAB roots are customizable, and corresponding directory registration
is an explicit operation rather than a load-time mutation of global swap
state. Startup-only string, list, buffer, region, and navigation aliases were
replaced throughout the converter, and its duplicate `o2m-all` definition was
removed. Condensed-colon conversion now rejects malformed input explicitly,
and Octave assertions inside `%!test` blocks are actually converted instead of
falling through as ordinary code. The dormant test file now has eight
namespaced tests in the maintained baseline; filesystem synchronization and
interactive documentation transfer remain outside those deterministic tests.

[`mb-octave.el`](./mode-extensions/mb-octave.el) now loads independently with
lexical binding and explicit built-in and repository dependencies. Evil,
key-chord, breakpoint, and debugger maps are installed only when their
facilities are available; loading Evil later retains the optional debugger
integration. Octave buffer setup is idempotent and buffer-local, including
syntax-table and keymap changes, and the stale
`texinfo-insert-@var*` binding now targets the maintained namespaced command.
Startup-only parsing, buffer, Info, paragraph, string, and Texinfo helpers were
replaced with supported APIs. Test paths and directory arguments are quoted,
machine-specific test-directory bindings were removed, `@seealso` references
preserve source order, and VC breakpoint checks no longer switch the selected
buffer. Its seven namespaced tests mock process boundaries and do not start
Octave.

[`octave-breakpoint.el`](./mode-extensions/octave-breakpoint.el) now loads
independently with lexical binding and explicit Octave dependencies.
Startup-only anaphoric, buffer-navigation, list, string, and directory helpers
were replaced with supported APIs, and the machine-specific source-directory
exception was removed. Breakpoint scopes now reject unsupported values,
malformed `dbstatus` responses fail explicitly, navigation reports empty
buffers instead of dividing by zero, and temporary breakpoints are removed
with `unwind-protect` even when the debug body fails. The corrected plural
`octave-update-all-dbstops` name retains an alias for its old singular spelling.
Its seven namespaced tests mock source and process boundaries and do not start
Octave.

[`octave-debug-mode.el`](./mode-extensions/octave-debug-mode.el) now loads
independently with lexical binding and no Evil dependency. Its minor-mode and
prefix maps use ordinary sparse keymaps, while the main Octave extension can
still install them into Evil when that optional package is present.
Startup-only anaphoric, buffer, navigation, and string helpers were removed;
the private `isdebugmode` Octave command was replaced with `dbstack`; and
`dbwhere` responses are parsed and validated explicitly. Debug source lookup
now uses xref instead of obsolete tags, marker state is buffer-local where
appropriate, leaving a session clears all Octave debug modes without sending a
spurious empty command, and rerunning an absent expression reports a user
error. Its seven tests mock all process and breakpoint boundaries and do not
start Octave.

The first staged migration of
[`backus-naur-mode.el`](./mode-extensions/backus-naur-mode.el) makes the legacy
major mode independently loadable with lexical binding and explicit `cl-lib`,
imenu, ring, and string dependencies. Mode state, indentation, hooks, and timer
cleanup are buffer-local; the real mode hook now runs; obsolete whole-buffer
fontification was replaced; and delayed refontification captures its originating
buffer rather than whichever buffer is current when the timer fires. Generic
buffer helper names and startup-only Common Lisp aliases were replaced, and
region indentation now calculates its alignment column correctly while tracking
an end marker as edits shift the buffer.

The second Backus-Naur checkpoint modernizes interactive nonterminal
navigation. Search overlays are now owned and removed by the mode instead of
deleting every overlay in the buffer, including overlays from unrelated
packages. Normal completion, keyboard quit, and mode re-entry all clean up only
those owned overlays. Refontification removes stale navigation properties,
flushes font-lock state before rebuilding it, and uses the correct
`font-lock-face` property when excluding optional references. Mouse navigation
uses supported event handling, and invalid search locations report user errors.

The third Backus-Naur checkpoint replaces the hand-built major-mode lifecycle
with `define-derived-mode`, a static mode keymap, buffer-local state variables,
and named commands for Home, End, completion, and point-based definition
navigation. Mode hooks now follow the standard derived-mode lifecycle, and the
completion command passes the required history symbol rather than the old
history value. Font-lock keyword construction is isolated from mode setup and
quotes the configured comment character. Eleven deterministic tests now cover
the first three BNF checkpoints.

The fourth Backus-Naur checkpoint removes the dead original indentation
implementation and tightens parser, motion, and alignment boundaries. Empty
nonterminal names are rejected; blank and comment lines retain their intended
layout; missing prior assignments report a user error; and explicit columns can
no longer delete part of a left nonterminal. Correctly spelled assignment
helpers retain aliases for their old misspellings. Definition motion now has
standard optional-count signatures and is installed through
`beginning-of-defun-function` and `end-of-defun-function`. Fifteen deterministic
tests now cover the complete retained BNF mode behavior.

The former `sql-mode.el` personal extension is now
[`mb-sql.el`](./mode-extensions/mb-sql.el), avoiding confusion with Emacs's
built-in `sql.el`. It loads independently, publishes a namespaced feature and
command, and no longer changes the global `split-width-threshold`. Startup
registers only an autoloaded mode-hook function, so the extension and built-in
SQL library load when SQL editing is first used. Leading SQL comment markers
are removed without truncating string literals or trailing comments, empty
expressions report a user error, and the command binding uses a per-buffer map
copy rather than mutating the shared `sql-mode-map`. Its four namespaced tests
mock the inferior SQL boundary.

[`mb-sh.el`](./mode-extensions/mb-sh.el) remains a usage-decision candidate.
Its original commit explicitly described it as unfinished, its formatter body
was never implemented, and there are no repository callers. Modernizing it
would require choosing new behavior rather than preserving existing behavior.

[`mb-tex.el`](./mode-extensions/mb-tex.el) also remains a usage-decision
candidate. It has no repository callers, requires an AUCTeX installation that
is absent from the clean environment, overrides the package's
`TeX-run-function`, and installs global `describe-mode` advice unrelated to
TeX. Its current AUCTeX usage and desired PDF-viewing behavior should be
confirmed before replacing that integration.

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
copy independence. The older duplicate `ert-mb-utils-tree.el` test file has been removed.

[`mb-sequences.el`](./utils/mb-sequences.el) now loads without the startup
environment and uses `cl-lib` directly. Its public helper names remain
available because they have many callers, while dependencies on generic
startup helpers have been replaced with local built-in operations. The
duplicate `draw-random` definition and load-time debug message were removed.
Rotation now uses its sequence argument, and minimum searches preserve their
input while reporting original positions for `:from-end`.

`deltas` previously delegated to `deltas-list` in
[`mb-lists.el`](./utils/mb-lists.el), which still fails to load outside the
startup environment. It is now expressed locally through the private
`mb-sequences--pairs` helper, which reproduces the flanking behaviour of
`pairs` and agrees with the original output for every input of three or more
elements. Two-element input previously returned `((nil nil) (nil nil))` and
now returns the surrounding half-difference. This small duplication remains deliberate because [`mb-lists.el`](./utils/mb-lists.el)
depends on the lower-level sequence module; making the sequence module call
back into `mb-lists` would create a cycle. The redundant list-only
`deltas-list` has been removed.

[`mb-lists.el`](./utils/mb-lists.el) now loads independently with lexical
binding and explicit `cl-lib` and sequence dependencies. Startup aliases and
the `cl-indent` helper have been replaced with supported APIs. The duplicate
`power-set-indices` and broken list-only `rotate` shadow were removed. Repairs
cover trailing chunks in `cut`, list padding, repetition keyword forwarding,
sorted insertion, and closure behavior. A small private list coercion helper
duplicates `listify` from `mb-utils-div`. It remains private rather than making
the lower-level list module depend on the broad general utility collection;
both copies should move to a small shared primitive module when that collection
is split further.

The widely used `aif`, `awhen`, `acond`, and `asetf` macros now live in the
small standalone [`mb-anaphora.el`](./utils/mb-anaphora.el) module rather than
the broad `mb-utils-div` collection. `mb-utils-div` requires the new feature,
so existing callers retain the same API without acquiring new dependencies.
Focused tests cover branch values, single evaluation, short-circuiting,
nested `it` bindings, and generalized assignment.

[`mb-utils-div.el`](./utils/mb-utils-div.el) now loads independently with
explicit Common Lisp, string, anaphora, and ISO-time dependencies instead of
pulling in the global indentation configuration. Startup aliases have been
replaced with supported APIs, including `cl-first` and `cl-rest` where the code
expresses positional list access. `car` and `cdr` remain for structural cons
operations. Its duplicate `nor` definition and embedded test
helpers were removed, and the old tests now run in the maintained clean
baseline. Repairs cover zero-position binding in `bind*`, lexical closures in
`listify`, numeric strings in `iintern`, generalized places in `push-unique`,
and function argument inspection under lexical binding.
Default handling in `pop-until` and removal of nil-valued properties in
`plist-pop` are also covered.

[`mb-utils-strings.el`](./utils/mb-utils-strings.el) now loads independently
with lexical binding and explicit list, sequence, math, set, and general
utility dependencies. Startup aliases and two load-time sample expressions
were removed, as was a shadowed `substring-intv` implementation. Repairs cover
its count variables, positional accessors, custom fill character, string
preservation, match starting position, replacement formatting, and error
messages. Loading the library no longer installs global quote-word keybindings;
those commands remain available for explicit configuration. Its 29 namespaced
tests now run in the maintained baseline.

[`mb-utils-time.el`](./utils/mb-utils-time.el) now loads independently with
lexical binding and explicit sequence, string, interval, list, ISO-time, and
built-in time dependencies. Removed load-time side effects include an obsolete
`cl-indent` call, a scratch formatting expression, and debug messages.
Decoded-time accessors now delegate to Emacs's standard accessors, encoded-time
comparison accepts every supported timestamp representation, and temporary
timezone changes are restored even when their body signals. Repairs cover
invalid-designator errors, four-part hours/minutes/seconds conversion, exact
rounding boundaries, and ISO weeks beginning in the following calendar year.
Its 42 namespaced tests now run in the maintained baseline.

[`mb-utils-math.el`](./utils/mb-utils-math.el) is now UTF-8 with lexical
binding and explicit list, sequence, anaphora, prime-data, general-utility, and
additional-math dependencies. Startup-only Common Lisp aliases and obsolete
`pi`/`lsh` uses were replaced, the premature `provide` was removed, and two
unfinished functions now signal explicit not-implemented errors. Repairs cover
integer rounding, free variables, cycle scoring, Haversine latitude handling,
annual interest, byte truncation/endianness/width, signed-byte boundaries,
four-part date decomposition, and configurable epoch years. The former
probabilistic test now uses deterministic weighted-selection boundaries. Its
20 namespaced tests run in the maintained baseline.

[`mb-combinatorics.el`](./utils/mb-combinatorics.el) now loads independently
with lexical binding and supported Common Lisp accessors. The obsolete
factorial size warning was removed because modern Emacs integers are
arbitrary-precision. Repairs cover negative derangements, out-of-range
binomial coefficients, distinctness in amicable pairs, UEFA side/opponent
selection, recursive draw generation, and Stirling-number documentation.
The UEFA recursion is characterized with a small deterministic fixture rather
than only the historical tournament data. Its 13 namespaced tests run in the
maintained baseline.

[`mb-utils-buffer.el`](./utils/mb-utils-buffer.el) now loads independently
with lexical binding and explicit dependencies. Loading it no longer changes
the global `M-t` or `C-x |` bindings. Repairs cover region marking, operations
on non-current buffers, paragraph and line positions, optional navigation
counts, line extraction around an explicit point, comment-region termination,
exception-safe temporary-file cleanup, and `save-as-1` using its argument.
The unsupported `commented-string-p` definition was removed because it
shadowed the maintained string utility. Its 15 namespaced tests run in the
maintained baseline.

[`mb-utils-regexp.el`](./utils/mb-utils-regexp.el) now loads independently
with lexical binding and only built-in dependencies. Raw-regexp alternatives
are grouped safely, an empty alternative set uses the standard unmatchable
regexp, and the grouping helpers no longer depend on removed `sort*`,
`remove*`, or startup accessor aliases. `mb-regexp-not-strings` now constructs
an anchored trie-complement regexp that rejects exactly the supplied finite
set, including empty strings, prefixes, duplicates, newlines, and regexp
metacharacters. Its seven namespaced tests run in the maintained baseline.

[`mb-utils-matrix.el`](./utils/mb-utils-matrix.el) now loads independently
with lexical binding and explicit dependencies. Vector and matrix operations
validate their dimensions instead of silently truncating through `cl-mapcar`.
Repairs cover seeded vector generation, vector-by-matrix orientation, the
missing square root in the Frobenius norm, zero-vector pseudoinverses,
unsupported general matrix pseudoinverses, obsolete `pi`, stable angle
clamping, and the previously non-working permutation row switch. Its eleven
namespaced tests run in the maintained baseline.

[`mb-utils-physics.el`](./utils/mb-utils-physics.el) now loads independently
with lexical binding and `cl-lib` as its only dependency. Its physical
constants were refreshed, escape velocity now honors the supplied body mass,
and meter-to-parsec now performs the conversion its name promises; the inverse
is available as `parsec-to-meter`. Constant-acceleration velocity consistently
uses the supplied acceleration, and unknown alcohol symbols signal an error
instead of recursing indefinitely. Its six namespaced tests run in the
maintained baseline.

[`mb-utils-geometry.el`](./utils/mb-utils-geometry.el) now loads independently
with lexical binding and `cl-lib` as its only dependency. Its one-dimensional
accessors no longer point at startup-only aliases, alignment honors optional
left and right margins, and interspace layouts preserve the full world length.
The unfinished affine transform and non-working interval mutation were replaced by
non-destructive transform and move operations. The duplicate broken `minmax`
definition was removed, restoring bounding boxes used by the municipality
quiz data. Its seven namespaced tests run in the maintained baseline.

[`mb-utils-search.el`](./utils/mb-utils-search.el) now loads independently
with lexical binding and no dependencies. Its sole public helper now searches
the designated buffer rather than silently using the current one, preserves
both point and match data, respects narrowing, and documents that it returns
the match end position rather than a boolean. Its four namespaced tests run in
the maintained baseline.

[`mb-utils-xml.el`](./utils/mb-utils-xml.el) now loads independently with
lexical binding and supported dependencies. Attribute parsing accepts quoted,
single-quoted, unquoted, and boolean attributes in any order. Raw-fragment
extraction now balances nested nodes of the same name and handles positive and
negative limits without capture-number assumptions. Inner-text extraction
discards child nodes, entity decoding delegates to the standard XML decoder,
and callback closures use lexical binding. Its seven namespaced tests run in
the maintained baseline.

[`mb-table.el`](./utils/mb-table.el) now loads independently with lexical
binding and built-in dependencies. Duplicate row-length implementations,
interactive-only line motion, and cursor-printing helper side effects were
removed. Rows are parsed as Emacs Lisp expressions, so compound expressions
containing spaces remain single cells; tables with malformed expressions or
uneven column counts signal explicit errors. Region, paragraph, and individual
line alignment share one deterministic formatter. Its seven namespaced tests
run in the maintained baseline.

[`mb-polynomials.el`](./utils/mb-polynomials.el) now loads independently with
lexical binding and `cl-lib` as its only dependency. Startup aliases and the
matrix dependency were removed; Horner evaluation now serves as the single
evaluation implementation. The duplicate `fp-value` and obsolete
`polynom-value-old` definitions were removed. Arithmetic now uses a consistent
zero-polynomial representation, long division terminates for zero remainders
and rejects zero divisors, polynomial predicates validate every coefficient,
and fraction evaluation no longer truncates integer quotients. Expansion,
float precision, negative-term formatting, and empty pole products were also
repaired. Emacs has no standard polynomial-arithmetic API that replaces this
module's retained functionality. Its nine namespaced tests run in the
maintained baseline.

[`mb-things.el`](./utils/mb-things.el) now loads independently with lexical
binding and built-in dependencies instead of pulling in the broad
`mb-utils-div` and `mb-utils-time` stacks. Its small date, clock, case, and
string operations are local and covered directly. Obsolete commented
templates and the non-working weekday/date helper were removed, the original
Latin-1 source was converted to UTF-8 without changing its Norwegian names,
and generated thing functions now compile without warnings.

[`mb-utils-economy.el`](./utils/mb-utils-economy.el) now loads independently
with lexical binding and `cl-lib` as its only dependency. A large anonymous
dataset that was sorted into the global variable `list` at load time has been
removed. The geometric-series calculation needed by the financial functions
is now private to the module. `withdraw-to-zero` includes all N withdrawals
rather than N-1; tests verify that using its returned balance in `growth`
reaches zero.

[`mb-math-functions.el`](./utils/mb-math-functions.el) now loads independently
with lexical binding and no startup helper dependencies. Degree-function macro
expansion uses symbols directly, while squaring and polynomial evaluation are
implemented without startup helpers. The migration corrected three defects: the
reversed sign of `tanh`, the finite geometric series for ratio -1, and endpoint
damping results in `falsi-method` that were computed but discarded. Numerical
characterization tests cover the affected functions.

[`mb-utils-iso-time.el`](./utils/mb-utils-iso-time.el) now uses lexical binding
and built-in Emacs time conversion rather than relying circularly on
`mb-utils-time` parsing and accessor functions. Its public constants and
numbered regexp capture groups are preserved. Timezones are now genuinely
optional in `iso-time-regexp`, and fractional seconds require a literal dot
followed by at least one digit.

## Standard-library supersession register

Many utilities here predate the Emacs releases that added equivalent built-ins:
`flatten-tree` and `proper-list-p` arrived in 27.1; `string-replace`,
`string-search`, `string-fill`, `string-pad`, `ensure-list`, `always`, and
`string-distance` in 28.1; `take`/`ntake` and `string-pixel-width` in 29.1.
A utility written before its standard counterpart is not automatically
redundant, so this register records candidates and, more importantly, the
**edge cases where the local version and the standard one genuinely differ**.

Append to this register whenever a migration turns up another candidate.
Retirement itself belongs to
[phase 5](#5-retire-confirmed-obsolete-code-and-document-the-remainder) and
needs the usual usage check across callers first.

### Retired unused compatibility modules

`function-signature.el` was removed after
confirming that it had no repository callers. Its reflection helpers are
covered by standard Emacs APIs: `help-function-arglist` returns argument lists,
`func-arity` returns minimum and maximum arity, and `interactive-form` returns
an interactive declaration or nil.

`split-sequence.el` was also removed after
confirming that it had no repository callers. It split generic sequences at
delimiter elements and supported start/end, count, reverse, predicate, and
second-return-value contracts. There is no single built-in replacement:
`split-string` covers ordinary string splitting, while the standard
`seq-split` chunks a sequence by length and is **not** equivalent. Reintroduce
the maintained GNU ELPA `split-sequence` package if that richer contract is
needed again rather than restoring the old local copy.

Three unreferenced mode-extension fragments were also retired after an explicit
usage decision:

* `mb-div-mode.el` contained only an unregistered PHP setup function, had no
  feature publication or callers, and was originally incomplete. Restore PHP
  settings through a normal `php-mode-hook` only if PHP editing becomes active
  again.
* `mb-key-chord.el` copied an old `key-chord` input-method implementation but
  published no feature and had no callers. Use the maintained `key-chord`
  package directly rather than reviving this private copy.
* `mb-geben.el` targeted an old Geben/PHP debugger installation and hard-coded
  obsolete Cygwin and Windows share paths. Reintroduce debugger integration as
  a new optional module with configurable path mapping if that workflow is
  needed again.

### Behaviourally equivalent for observed uses

These were compared against the standard function on the cases that matter and
matched. They are safe retirement candidates once callers are checked.

| Local | Standard | Note |
| --- | --- | --- |
| `string-to-character-list` | `string-to-list` | Identical. |
| `lines-to-string` | `string-join` with `"\n"` | Identical for string lists. |
| `empty-string-p` | `string-empty-p` | Both return nil for nil input. |
| `blanks` | `make-string`, or `string-pad` | Already a thin `make-string` wrapper. |
| `string-fill-paragraph` | `string-fill` | Standard takes an explicit width. |
| `string-trim*` and its side variants | `string-trim`, `string-trim-left`, `string-trim-right` | Standard also accepts trim regexps; the `:left`/`:right`/`:both` dispatch is the only extra. |
| `string-remove-props` | `substring-no-properties` | Already self-declared obsolete. Differs only in stripping destructively in place rather than copying. |
| `dtime-second` through `dtime-time-zone` | `decoded-time-second` through `decoded-time-zone` | The public local names now alias the standard decoded-time accessors. |
| `etime<` | `time-less-p` | The local wrapper now delegates to the standard predicate, which also supports integer and tick/frequency timestamps. |
| `diff-current-time-msec` | `time-subtract` plus `float-time` | The standard operations preserve timestamp representation independence; multiply the result by 1000 for milliseconds. |
| `parse-ms` | `seconds-to-time` | Divide milliseconds by 1000.0 before calling the standard function. |
| `unix-time` and `ms-unix-time` | `float-time` | The local versions additionally accept the broader `parse-time` designators. |
| `yearweek-number` and `week-number` | `format-time-string` with `%G` and `%V` | Results agree at tested ISO-year boundaries after repairing exact first-Monday handling. |
| `bytes-to-string` | `unibyte-string` | The local wrapper now delegates to the standard constructor and genuinely returns a unibyte string for bytes above 127. |
| `string-to-bytes` | `string-to-list` | The local wrapper now delegates directly to the standard function. |
| `int-to-hex` | `format` with `%x` | The helper only adds optional zero-padding. |
| `current-year` | `format-time-string` with `%Y`, or `decoded-time-year` | The local helper now uses the standard decoded-time accessor. |
| `reverse-lines` | `reverse-region` | The local name is only an alias for the standard operation. |
| `buffer-string*` | `buffer-substring-no-properties` from `point-min` to `point-max` | The local helper only adds an optional buffer argument. |
| `*empty-regexp*` | `regexp-unmatchable` | The local constant now uses the standard value; its former `"\\`\\`"` value incorrectly matched the empty string at the start of input. |
| `xml-decode-characters` | `xml-substitute-special` | The local wrapper now delegates to the standard XML entity and numeric-character-reference decoder; its obsolete locale-table argument is retained only for call compatibility. |

### Differ in edge cases — do not substitute blindly

These look replaceable but are not, and the difference is easy to miss.

| Local | Nearest standard | Divergence |
| --- | --- | --- |
| `listify` | `ensure-list` | **Interpreted lambdas**: `(lambda () nil)` is itself a list, so `ensure-list` returns it unwrapped while `listify` wraps it. **Vectors**: `listify` coerces element-wise to `(1 2)`; `ensure-list` wraps to `([1 2])`. Byte-compiled lambdas happen to agree. |
| `flatten` | `flatten-tree` | Agree on full flattening, including nil removal, but `flatten` takes a `LEVELS` argument for partial flattening that has no standard equivalent. |
| `substring*` | `substring` | Standard `substring` already supports negative indices, so that is no longer a reason to keep this. They diverge only out of range: `substring*` wraps via `mod` and silently returns a value where `substring` signals `args-out-of-range`. Decide whether that masking is wanted before retiring. |
| `blank-p` | `string-blank-p` | `blank-p` returns a boolean; `string-blank-p` returns a match position. Interchangeable in conditionals, not under `equal`. |
| `constantly` | `always` | `always` only covers the constantly-`t` case; `constantly` builds a closure returning any value. |
| `pure-string` | `substring-no-properties` | Despite its docstring, `pure-string` does not strip text properties: its string branch returns the argument unchanged. Its other branch unwraps a non-string sequence to its first element, which is unrelated to formatting. |
| `add-etime-date` | `decoded-time-add` | Both add calendar fields, but invalid dates differ: adding one month to 2019-01-31 normalizes to March 3 locally and clamps to February 28 in `decoded-time-add`. DST handling also needs comparison before substitution. |
| `now` | `current-time` | With no arguments both represent the current instant, but `now` round-trips through decoded time and discards subsecond precision. It also retains a deprecated keyword-adjustment interface. |
| `parse-time` | `date-to-time`, `seconds-to-time`, and `encode-time` | The local dispatcher accepts strings, symbols, numbers, decoded times, and encoded times; no single standard function has that combined contract. |
| `time` and `time-val` | `benchmark-run` and `benchmark-call` | Standard benchmarking reports elapsed/GC timing, while the local macros evaluate once, preserve the expression value, and report milliseconds. |
| `sum` and `product` | `seq-reduce` or `cl-reduce` | The local helpers retain richer keyword forwarding (`sum`) and key/initial-value defaults (`product`), so callers need checking before replacement. |
| `product-safe` and `product-safe-list` | ordinary integer multiplication | They detect fixnum overflow and switch to floats, but modern Emacs has arbitrary-precision integers. Keeping the old fallback can unnecessarily lose precision; retire only after checking callers that may expect floats. |
| `jstime` | `seconds-to-time` plus `decode-time` | The standard pair handles Unix timestamps directly. The local result instead uses zero-based month and day-within-month fields and supports a custom base year. |
| `leap-year-p` and `days-in-month` | `calendar-leap-year-p` and `calendar-last-day-of-month` | The local month argument is zero-based, whereas the calendar API uses months 1 through 12. |
| `regexp-or` | `regexp-opt` | `regexp-or` combines raw regexp fragments such as `"a+"`; `regexp-opt` treats its input as literal strings and quotes regexp operators. |
| `xml-extract-nodes` | `libxml-parse-html-region`, `libxml-parse-xml-region`, and DOM helpers | The standard parsers produce normalized node trees. Existing callers require original source fragments, regexp-valued attribute filters, and reverse extraction, so substitution would change their contract. |
| `mb-texinfo-insert-@var` and related wrappers | `texinfo-insert-@var`, `texinfo-insert-@code`, and related commands | The personal commands wrap the active region or symbol at point and leave point inside empty braces. Standard Texinfo commands retain their broader optional argument signatures and are no longer shadowed. |

### Internal consolidations

These are not replacements by standard library functions, but local APIs whose
overlapping responsibilities have been consolidated:

| Previous APIs | Consolidation | Note |
| --- | --- | --- |
| `all-factors`, `aliquot-sum`, and `divisor-function` | `all-factors` accepts an optional proper-divisor flag; `aliquot-sum` is `(sum (all-factors n t))`; `divisor-function` was retired | The only callers of `divisor-function` were the superseded `aliquot-sum` definition and examples beside it. |
| Duplicate `transpose` definitions in `mb-lists` and `mb-utils-matrix` | Retain the independently tested `mb-lists` implementation | Both definitions used `cl-mapcar` to transpose lists and had the same shortest-input behavior. |

### Still first-party

No standard equivalent found: `multiply-string` (string repetition),
`not-empty`, `concat*` and `andcat` (keyword-driven joining beyond
`string-join`), the `split-string-*` family, and the BSG, UUID, alphabet,
tab-formatting, literary-number, factorial, derangement, Fibonacci, binomial,
Catalan, and Stirling-number helpers.
