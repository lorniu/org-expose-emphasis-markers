[![License: GPL-3.0](http://img.shields.io/:license-gpl3-blue.svg)](https://opensource.org/licenses/GPL-3.0)

Automatically show hidden emphasis markers at point in org mode.

This is useful for editing org file when `org-hide-emphasis-markers` is on.

In org mode, hide emphasis markers can make reading nice, but not good for editing.
Here provide a mode to let hidden markers auto expose when the cursor on, and auto
hide when cursor leave.

## Get Started

First, install this package, and load it when necessary:
```elisp
(require 'org-expose-emphasis-markers)
```

You should use this with `org-hide-emphasis-markers` set to `t`:
```elisp
(setq org-hide-emphasis-markers t)

;; turn on
(org-expose-emphasis-markers-mode t)

;; or put in the mode hook
(add-hook 'org-mode-hook (lambda () (org-expose-emphasis-markers-mode t)))
```

Config:

```elisp
;; You can set `org-expose-emphasis-markers-type' to 'sentence or 'paragraph to
;; change the auto expose scope.

(setq org-expose-emphasis-markers-type 'paragraph)
```

## Miscellaneous

Issues and PRs are welcome. Happy good day.
