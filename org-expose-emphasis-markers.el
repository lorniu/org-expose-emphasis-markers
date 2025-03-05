;;; org-expose-emphasis-markers.el --- Automatically show hidden org emphasis markers -*- lexical-binding: t -*-

;; Copyright (C) 2025 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/org-expose-emphasis-markers
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Automatically show hidden emphasis markers at point in org mode.
;;
;; This is useful for editing org file when `org-hide-emphasis-markers' is on.
;;
;; In org mode, hide emphasis markers can make reading nice, but not good for editing.
;; Here provide a mode to let hidden markers auto expose when the cursor on, and auto
;; hide when cursor leave.
;;
;; Install this packge, then use it by turning on the mode:
;;
;;   (add-hook 'org-mode-hook (lambda () (org-expose-emphasis-markers-mode t)))
;;

;;; Code:

(require 'org)
(require 'cl-lib)

(defgroup org-expose-emphasis-markers nil
  "Automatically show hidden org emphasis markers at point."
  :group 'org
  :prefix 'org-expose-emphasis-markers)

(defcustom org-expose-emphasis-markers-type nil
  "The type represent scope to show hidden markers.

The value is a symbol like \\='paragraph, \\='sentence or nil. The
default value is nil, representing emphasis element around point."
  :type 'symbol)

(cl-defgeneric org-expose-emphasis-markers-bounds (_type)
  "Return the bounds of element at point according TYPE.
Bounds of emphasis elements around point is returned by default."
  (let ((bounds
         (or (org-find-text-property-region (point) 'org-emphasis)
             (let ((before (unless (bobp)
                             (org-find-text-property-region (- (point) 1) 'org-emphasis)))
                   (after (unless (eobp)
                            (org-find-text-property-region (+ (point) 1) 'org-emphasis))))
               (when (or before after)
                 (cons (if before (car before))
                       (if after (cdr after))))))))
    (when bounds
      (unless (car bounds) (setcar bounds (point)))
      (unless (cdr bounds) (setcdr bounds (point))))
    bounds))

(cl-defmethod org-expose-emphasis-markers-bounds ((_type (eql 'sentence)))
  "Return the bounds of sentence TYPE at point."
  (cons (line-beginning-position) (line-end-position)))

(cl-defmethod org-expose-emphasis-markers-bounds ((_type (eql 'paragraph)))
  "Return the bounds of paragraph TYPE at point."
  (save-excursion
    (backward-paragraph)
    (cons (point) (progn (forward-paragraph) (point)))))

(defun org-expose-emphasis-markers--expose-function ()
  "Auto expose hidden emphasis markers at/around point."
  (when org-hide-emphasis-markers
    (let ((bounds (org-expose-emphasis-markers-bounds org-expose-emphasis-markers-type)))
      ;; Previous emphasis element, reset
      (when (and org-expose-emphasis-markers--current-bounds
                 (not (equal org-expose-emphasis-markers--current-bounds bounds)))
        (let ((beg (car org-expose-emphasis-markers--current-bounds))
              (end (cdr org-expose-emphasis-markers--current-bounds)))
          (setq beg (save-excursion (goto-char beg) (line-beginning-position)))
          (setq end (save-excursion (goto-char end) (line-end-position)))
          (font-lock-flush beg end)
          (font-lock-ensure beg end)))
      ;; Current emphasis element, show the markers
      (when bounds
        (with-silent-modifications
          (remove-text-properties (car bounds) (cdr bounds) '(invisible t))))
      (setq org-expose-emphasis-markers--current-bounds bounds))))

(defvar-local org-expose-emphasis-markers--current-bounds nil)

;;;###autoload
(define-minor-mode org-expose-emphasis-markers-mode
  "Minor mode for auto expose hidden emphasis markers in org mode."
  :init-value nil
  (unless (eq major-mode 'org-mode)
    (user-error "Cannot turn on this mode outside org-mode buffers"))
  (if org-expose-emphasis-markers-mode
      (if org-hide-emphasis-markers
          (add-hook 'post-command-hook #'org-expose-emphasis-markers--expose-function -90 t)
        (user-error "This only works when `org-hide-emphasis-markers' is t"))
    (remove-hook 'post-command-hook #'org-expose-emphasis-markers--expose-function t))
  (font-lock-flush))

(provide 'org-expose-emphasis-markers)

;;; org-expose-emphasis-markers.el ends here
