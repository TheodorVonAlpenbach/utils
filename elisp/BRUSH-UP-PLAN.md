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
| Potential library-name collision | [`sql-mode.el`](./mode-extensions/sql-mode.el) contains personal extensions to built-in SQL mode. | Give the extension an unambiguous feature/file name and arrange setup after built-in SQL loads. |

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
| `global-map` | [`global-map.el`](./global/global-map.el) | Active startup/keymap; global side effects |
| `mb-utils-io` | [`mb-utils-io.el`](./utils/mb-utils-io.el) | Active shared utility; startup-coupled |
| `mb-evil-map` | [`mb-evil-map.el`](./mode-extensions/mb-evil-map.el) | Active optional integration; global keymaps |
| `mb-things` | [`mb-things.el`](./utils/mb-things.el) | Active shared utility; independently loadable |
| `radio-playlists` | [`radio-playlists.el`](./archive/browser/radio-playlists.el) | Archived; external programs/network |
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

The closed-interval, envelope, set-helper, file, I/O, tree, thing-at-point,
economy, additional-math, general-math, combinatorics, buffer, regexp, matrix,
physics, geometry, search, XML, anaphora, general, list, sequence, string,
time, ISO-time, and table modules are independently loadable. Their validation
comprises:

* a clean `emacs -Q` load of all twenty-seven utility features and the ERT tooling;
* 322 passing ERT tests from the batch entry point;
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
