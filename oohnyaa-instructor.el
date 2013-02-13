;;; oohnyaa-instructor.el --- Enforce key-bind of Emacs.

;; Copyright (C) 2012 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; This program is heavily based on "drill-instructor.el" (v1.1.4) by k1LoW

;; Version: 1.0.1
;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/

;;; Commentary:

;; You can unset oohnyaa-instructor on some major-modes with code like
;;
;; (add-to-list 'oohnyaa-unset-major-mode-list '***-mode)

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 minor fix and refactorings

;;; Code:

;; * constants

(defconst oohnyaa-version "1.0.1")

;; prefered keys

(defconst oohnyaa-left-key "C-b")
(defconst oohnyaa-right-key "C-f")
(defconst oohnyaa-up-key "C-p")
(defconst oohnyaa-down-key "C-n")

(defconst oohnyaa-home-key "C-a")
(defconst oohnyaa-end-key "C-e")
(defconst oohnyaa-prior-key "M-v")
(defconst oohnyaa-next-key "C-v")

(defconst oohnyaa-delete-key "C-d")
(defconst oohnyaa-backspace-key "C-h")
(defconst oohnyaa-tab-key "C-i")
(defconst oohnyaa-return-key "C-m")

;; * mode variable

(defvar oohnyaa-instructor nil)

(defvar oohnyaa-unset-major-mode-list
  '(term-mode)
  "Ooh!Nyaa instructor unset list")

(defvar oohnyaa-key-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") (lambda()(interactive)(oohnyaa-alert oohnyaa-up-key)))
    (define-key map (kbd "<down>") (lambda()(interactive)(oohnyaa-alert oohnyaa-down-key)))
    (define-key map (kbd "<left>") (lambda()(interactive)(oohnyaa-alert oohnyaa-left-key)))
    (define-key map (kbd "<right>") (lambda()(interactive)(oohnyaa-alert oohnyaa-right-key)))

    (define-key map (kbd "<next>") (lambda()(interactive)(oohnyaa-alert oohnyaa-next-key)))
    (define-key map (kbd "<prior>") (lambda()(interactive)(oohnyaa-alert oohnyaa-prior-key)))
    (define-key map (kbd "<home>") (lambda()(interactive)(oohnyaa-alert oohnyaa-home-key)))

    (define-key map (kbd "<delete>") (lambda()(interactive)(oohnyaa-alert oohnyaa-delete-key)))
    (define-key map (kbd "<backspace>") (lambda()(interactive)(oohnyaa-alert oohnyaa-backspace-key)))
    (define-key map (kbd "<tab>") (lambda()(interactive)(oohnyaa-alert oohnyaa-tab-key)))
    (define-key map (kbd "<return>") (lambda()(interactive)(oohnyaa-alert oohnyaa-return-key)))
    map))

(add-to-list 'minor-mode-map-alist (cons 'oohnyaa-instructor oohnyaa-key-map))

(if (not (assq 'oohnyaa-instructor minor-mode-alist))
    (setq minor-mode-alist
          (cons '(oohnyaa-instructor " ・ω・")
                minor-mode-alist)) )

;; * oohnyaa instructor switch

(defun oohnyaa-instructor-switch ()
  "oohnyaa-instructor-switch"
  (if (memq major-mode oohnyaa-unset-major-mode-list)
      (setq oohnyaa-instructor nil)
    (setq oohnyaa-instructor t) ))

(defadvice switch-to-buffer (after oohnyaa-instructor-switch-to-buffer activate)
  "oohnyaa-instructor-switch-to-buffer"
  (oohnyaa-instructor-switch))

(defadvice kill-buffer (after oohnyaa-instructor-kill-buffer activate)
  "oohnyaa-instructor-kill-buffer"
  (oohnyaa-instructor-switch))

(defadvice other-window (after oohnyaa-instructor-other-window activate)
  "oohnyaa-instructor-other-window"
  (oohnyaa-instructor-switch))

(defadvice delete-window (after oohnyaa-instructor-delete-window activate)
  "oohnyaa-instructor-delete-window"
  (oohnyaa-instructor-switch))

;; * command

(defun oohnyaa-alert (keystring)
  (interactive)
  (message "(」・ω・)」 Why not") (sleep-for 0.5)
  (message (concat "(／・ω・)／  " keystring)) (sleep-for 0.5)
  (message "(」・ω・)」 Why not") (sleep-for 0.5)
  (message (concat "(／・ω・)／  " keystring)) (sleep-for 0.5)
  (message ""))

;; * activate

(setq oohnyaa-instructor t)

;; * provide

(provide 'oohnyaa-instructor)

;;; oohnyaa-instructor.el ends here
