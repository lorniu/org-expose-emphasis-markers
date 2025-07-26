;;; org-expose-emphasis-markers.el --- Automatically show hidden org emphasis markers -*- lexical-binding: t -*-

;; Copyright (C) 2025 lorniu <lorniu@gmail.com>

;; Author: lorniu <lorniu@gmail.com>
;; URL: https://github.com/lorniu/org-expose-emphasis-markers
;; License: GPL-3.0-or-later
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.21

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
;;   (add-hook 'org-mode-hook (lambda () (org-expose-emphasis-markers 'paragraph)))
;;

;;; Code:

(require 'org)
(require 'cl-lib)

(defgroup org-expose-emphasis-markers nil
  "Automatically show hidden org emphasis markers at point."
  :group 'org
  :prefix 'org-expose-emphasis-markers)

(defcustom org-expose-emphasis-markers-type 'item
  "The type represent scope to show hidden markers.

The value is a symbol like \\='item, \\='line or \\='paragraph.  Default
\\='item representing scope of emphasis element at point.  If set to other
value, the mode will not work, as same as turn it off."
  :type 'symbol)

(cl-defgeneric org-expose-emphasis-markers-bounds (_type)
  "Return the bounds of element at point according TYPE."
  nil)

(cl-defmethod org-expose-emphasis-markers-bounds ((_type (eql 'item)))
  "Return the bounds of current emphasis element TYPE at point."
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

(cl-defmethod org-expose-emphasis-markers-bounds ((_type (eql 'line)))
  "Return the bounds of line TYPE at point."
  (cons (line-beginning-position) (line-end-position)))

(cl-defmethod org-expose-emphasis-markers-bounds ((_type (eql 'paragraph)))
  "Return the bounds of paragraph TYPE at point."
  (unless (= (line-beginning-position) (line-end-position))
    (cons (save-excursion (backward-paragraph) (point))
          (save-excursion (forward-paragraph) (point)))))

(defvar org-expose-emphasis-markers-inhibit-determine-function
  #'org-expose-emphasis-markers-default-should-inhibit
  "Function to determine whether the mode should be inhibited.
This function will be called with no arguments.")

(defun org-expose-emphasis-markers-default-should-inhibit ()
  "Check whether the mode should be inhibited."
  buffer-read-only)

(defvar-local org-expose-emphasis-markers--current-bounds nil)

(defun org-expose-emphasis-markers--expose-function ()
  "Auto expose hidden emphasis markers at/around point."
  (let* ((inhibit (or (not org-hide-emphasis-markers)
                      (funcall org-expose-emphasis-markers-inhibit-determine-function)))
         (bounds (unless inhibit (org-expose-emphasis-markers-bounds org-expose-emphasis-markers-type))))
    ;; Previous emphasis element, reset
    (when (and org-expose-emphasis-markers--current-bounds
               (or inhibit (not (equal org-expose-emphasis-markers--current-bounds bounds))))
      (let ((beg (car org-expose-emphasis-markers--current-bounds))
            (end (cdr org-expose-emphasis-markers--current-bounds)))
        (setq beg (save-excursion (goto-char beg) (line-beginning-position)))
        (setq end (save-excursion (goto-char end) (line-end-position)))
        (font-lock-flush beg end)
        (font-lock-ensure beg end)))
    ;; Current emphasis element, show the markers
    (when bounds
      (cl-labels ((remove-invisible (beg end)
                    (with-silent-modifications
                      (when (> beg (point-min))
                        (remove-text-properties beg (1+ beg) '(invisible t)))
                      (when (< end (point-max))
                        (remove-text-properties (1- end) end '(invisible t))))))
        (if (memq org-expose-emphasis-markers-type '(nil item))
            (remove-invisible (car bounds) (cdr bounds))
          (save-excursion
            (save-restriction
              (narrow-to-region (car bounds) (cdr bounds))
              (goto-char (point-min))
              (while (not (eobp))
                (let ((p (get-text-property (point) 'org-emphasis))
                      (n (next-single-property-change (point) 'org-emphasis)))
                  (when (and p n) (remove-invisible (point) n))
                  (goto-char (or n (point-max))))))))))
    (setq org-expose-emphasis-markers--current-bounds bounds)))

;;;###autoload
(define-minor-mode org-expose-emphasis-markers-mode
  "Minor mode for auto expose hidden emphasis markers in org mode."
  :init-value nil
  (unless (derived-mode-p 'org-mode)
    (user-error "Cannot turn on this mode outside org-mode buffers"))
  (if org-expose-emphasis-markers-mode
      (if org-hide-emphasis-markers
          (add-hook 'post-command-hook #'org-expose-emphasis-markers--expose-function -90 t)
        (org-expose-emphasis-markers-mode -1)
        (message "Expose emphasis markers mode only works when `org-hide-emphasis-markers' is t"))
    (remove-hook 'post-command-hook #'org-expose-emphasis-markers--expose-function t))
  (font-lock-flush))

;;;###autoload
(defun org-expose-emphasis-markers (&optional scope)
  "Switch exposing SCOPE quickly."
  (interactive (let ((types (cl-loop ; collect the types from all the generic functions
                             for m in (cl--generic-method-table (cl--generic #'org-expose-emphasis-markers-bounds))
                             for n = (car (cl--generic-method-specializers m))
                             unless (eq n t) collect (substring (format "%s" (cadr n)) 1))))
                 (list (intern (completing-read "Expose scope: "
                                                `(,@types hide-all show-all) nil t nil nil
                                                (if org-hide-emphasis-markers
                                                    (if org-expose-emphasis-markers-mode
                                                        (format "%s" org-expose-emphasis-markers-type)
                                                      "hide-all")
                                                  "show-all")))))
               org-mode)
  (pcase scope
    ('hide-all (setq-local org-hide-emphasis-markers t)
               (org-expose-emphasis-markers-mode -1))
    ('show-all (setq-local org-hide-emphasis-markers nil)
               (org-expose-emphasis-markers-mode -1))
    (_ (setq-local org-hide-emphasis-markers t)
       (setq-local org-expose-emphasis-markers-type scope)
       (org-expose-emphasis-markers-mode 1)))
  (font-lock-flush)
  (when (called-interactively-p 'any)
    (message "Scope type of `org-expose-emphasis-markers-mode' changed to '%s" scope)))

(provide 'org-expose-emphasis-markers)

;;; org-expose-emphasis-markers.el ends here
