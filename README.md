[![License: GPL-3.0](http://img.shields.io/:license-gpl3-blue.svg)](https://opensource.org/licenses/GPL-3.0)
[![MELPA](https://melpa.org/packages/org-expose-emphasis-markers-badge.svg)](https://melpa.org/#/org-expose-emphasis-markers)

Automatically show hidden emphasis markers at point in org mode.

This is useful for editing org file when `org-hide-emphasis-markers` is on.

In org mode, hide emphasis markers can make reading nice, but not good for editing.
Here provide a mode to let hidden markers auto expose when the cursor on, and auto
hide when cursor leave.

## Usage

Install this package from MELPA, and load it when necessary:
```elisp
(require 'org-expose-emphasis-markers)
```

Then config and turn on the mode:
```elisp
;; 1. make sure `org-hide-emphasis-markers' is true
(setq org-hide-emphasis-markers t)

;; 2. (optional) set the exposing scope, default value is 'item
(setq org-expose-emphasis-markers-type 'paragraph)

;; 3. turn on the mode
(add-hook 'org-mode-hook (lambda () (org-expose-emphasis-markers-mode t)))
```

Use command `org-expose-emphasis-markers` to make thing easier. For example, above can simply as:
```elisp
(add-hook 'org-mode-hook (lambda () (org-expose-emphasis-markers 'paragraph)))
```

The command can be invoked directly to set or change scope quickly:
```
M-x org-expose-emphasis-markers
```

## Advanced

New scope type can be created by specializing `org-expose-emphasis-markers-bounds`:
```elisp
(cl-defmethod org-expose-emphasis-markers-bounds ((_type (eql 'new-type-name)))
  ;; return a cons cell type, representing the scope bounds
  )
(setq org-expose-emphasis-markers-type 'new-type-name)
```

You can config to determine dynamically whether the mode should be inhibited.
By default, when current buffer is read only, the mode should not work.
Config with `org-expose-emphasis-markers-inhibit-determine-function` for other cases. For example:
```elisp
(setq org-expose-emphasis-markers-inhibit-determine-function
      (lambda () (or buffer-read-only (other-cases-mode-should-inhibit))))
```

## Change Log

v0.2:
* rename `org-expose-emphasis-markers-scope-type` to `org-expose-emphasis-markers`
* rename type `disabled` to `hide-all`, and add type `show-all`

## Miscellaneous

Issues and PRs are welcome. Happy good day.
