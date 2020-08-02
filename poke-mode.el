;;; poke-mode.el --- Minor Emacs mode to show position in a buffer using a Pokemon -*- lexical-binding: t; -*-

;; Author: Ryan Miller <ryan@devopsmachine.com>
;; URL: https://github.com/RyanMillerC/poke-mode/
;; Version: 1.0.0
;; Keywords: pokemon, fun, mode-line, mouse

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; To activate, load and put `(poke-mode 1)' in your init file.

;; You can change pokemon with `(poke-set-pokemon "charmander")'.

;; You can click on the position bar (or the empty space) to scroll
;; your buffer!

;; You can customize the minimum window width below which poke-mode
;; will be disabled, so that more important information can be shown
;; in the modeline.

;;; Code:

(require 'cl-lib)

(defconst poke-directory (file-name-directory (or load-file-name buffer-file-name)))
(defconst poke-background-image (concat poke-directory "img/background.png"))
(defconst poke-modeline-help-string "Gotta catch 'em all!\nmouse-1: Scroll buffer position")
(defconst poke-size 3)

(defvar poke-active-pokemon nil)
(defvar poke-active-pokemon-type nil)
(defvar poke-old-car-mode-line-position nil)

;; Load required package for Pokemon type data
(load-file (concat poke-directory "poke-mode-types.el"))

(defun poke-refresh ()
  "Refresh poke-mode.
Intended to be called when customizations were changed, to reapply them immediately."
  (when (featurep 'poke-mode)
    (when (and (boundp 'poke-mode) poke-mode)
      (poke-mode -1)
      (poke-mode 1))))

(defun poke-set-pokemon (pokemon)
  "Set the active `POKEMON' for `poke-mode'."
  (interactive "sWhich Pokemon would you like to set? ")
  (if (car (assoc pokemon poke-pokemon-types))
      (progn
        (set-default 'poke-active-pokemon pokemon)
        (set-default 'poke-active-pokemon-type (cdr (assoc pokemon poke-pokemon-types)))
        (poke-refresh)
        (message "Pokemon set to %s" pokemon))
    (message "Couldn't find Pokemon \"%s\". See poke-mode repo for Pokemon names." pokemon)))

;; Set default Pokemon
(poke-set-pokemon "charmander")

(defgroup poke nil
  "Customization group for `poke-mode'."
  :group 'frames)

(defcustom poke-minimum-window-width 64
  "Minimum width of the window, below which poke-mode will not be displayed.
This is important because poke-mode will push out all informations from small windows."
  :type 'integer
  :set (lambda (sym val)
         (set-default sym val)
         (poke-refresh))
  :group 'poke)

(defcustom poke-bar-length 32
  "Length of Poke element bar in units.
Each unit is equal to an 8px image.
Minimum of 3 units are required for poke-mode."
  :type 'integer
  :set (lambda (sym val)
         (set-default sym val)
         (poke-refresh))
  :group 'poke)

(defun poke-get-pokemon-image ()
  "Get path to Pokemon PNG image."
  (concat poke-directory "img/pokemon/" poke-active-pokemon ".png"))

(defun poke-get-element-image ()
  "Get path to Pokemon PNG image."
  (concat poke-directory "img/elements/" poke-active-pokemon-type ".png"))

(defun poke-number-of-elements ()
  "Calculate number of elements."
  (round (/ (* (round (* 100
                        (/ (- (float (point))
                             (float (point-min)))
                          (float (point-max)))))
              (- poke-bar-length poke-size))
           100)))

(defun poke-scroll-buffer (percentage buffer)
  "Move point `BUFFER' to `PERCENTAGE' percent in the buffer."
  (interactive)
  (with-current-buffer buffer
    (goto-char (floor (* percentage (point-max))))))

(defun poke-add-scroll-handler (string percentage buffer)
  "Propertize `STRING' to scroll `BUFFER' to `PERCENTAGE' on click."
  (let ((percentage percentage)
                 (buffer buffer))
    (propertize string 'keymap `(keymap (mode-line keymap (down-mouse-1 . ,(lambda () (interactive) (poke-scroll-buffer percentage buffer))))))))

(defun poke-create ()
  "Return the Pokemon indicator to be inserted into mode line."
  (setq debug-on-error t)
  (if (< (window-width) poke-minimum-window-width)
      "" ; Disable for small windows
    (let* ((elements (poke-number-of-elements))
      (backgrounds (- poke-bar-length elements poke-size))
      (element-string "")
      (png-support (image-type-available-p 'png))
      (pokemon-string (propertize "|||" 'display (create-image (poke-get-pokemon-image) 'png nil :ascent 'center)))
      (background-string "")
      (buffer (current-buffer)))
      (dotimes (number elements)
        (setq element-string
          (concat element-string
            (poke-add-scroll-handler
              (if png-support
                  (propertize "|" 'display (create-image (poke-get-element-image) 'png nil :ascent 'center))
                "|")
              (/ (float number) poke-bar-length) buffer))))
        (dotimes (number backgrounds)
          (setq background-string
            (concat background-string
              (poke-add-scroll-handler
                (if png-support
                    (propertize "-" 'display (create-image poke-background-image 'png nil :ascent 'center))
                  "-")
                (/ (float (+ elements poke-size number)) poke-bar-length) buffer))))
      ;; Compute Poke Cat string.
      (propertize
        (concat
          pokemon-string
          element-string
          background-string)
        'help-echo poke-modeline-help-string))))

;;;###autoload
(define-minor-mode poke-mode
  "Use Pokemon to show buffer position in mode-line.
You can customize this minor mode, see option `poke-mode'.

Note: If you turn this mode on then you probably want to turn off
option `scroll-bar-mode'."
  :global t
  :group 'poke
  (cond (poke-mode
          (unless poke-old-car-mode-line-position
            (setq poke-old-car-mode-line-position (car mode-line-position)))
          (poke-create)
          (setcar mode-line-position '(:eval (list (poke-create)))))
        ((not poke-mode)
          (setcar mode-line-position poke-old-car-mode-line-position)
          (setq poke-old-car-mode-line-position nil))))


(provide 'poke-mode)

;;; poke-mode.el ends here
