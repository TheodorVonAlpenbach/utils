---
description: "Use when writing or refactoring Emacs Lisp. Covers preferred destructuring and iteration forms."
applyTo: "elisp/**/*.el"
---

# Emacs Lisp style

- Do not use `pcase-let` or `pcase-let*` for destructuring. Use
  `cl-destructuring-bind` instead.
- Prefer `cl-loop` over `mapcar`, `cl-mapcar`, and similar mapping forms when
  the mapping would require an inline lambda.

Prefer:

```elisp
(cl-loop for relation in relations
         collect (mb-tree--relation-edge relation reverse-p))
```

over:

```elisp
(mapcar (lambda (relation)
          (mb-tree--relation-edge relation reverse-p))
        relations)
```

Direct function designators such as `(mapcar #'car values)` remain appropriate.
