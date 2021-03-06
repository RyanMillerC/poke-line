;;; poke-line.el --- Minor mode to show position in a buffer using a Pokemon -*- lexical-binding: t; -*-

;; Author: Ryan Miller <ryan@devopsmachine.com>
;; Package-Requires: ((emacs "24.3"))
;; Homepage: https://github.com/RyanMillerC/poke-line/
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

;; To activate, load and put `(poke-line-global-mode 1)' in your init file.

;; You can change pokemon with `(poke-line-set-pokemon "charmander")'
;; or customize `poke-line-pokemon'.

;; You can click on the position bar (or the empty space) to scroll
;; your buffer!

;; You can customize the minimum window width below which poke-line
;; will be disabled, so that more important information can be shown
;; in the modeline.

;;; Code:
(unless (image-type-available-p 'png)
  (user-error "PNG support not detected. poke-line requires png support"))

(require 'cl-lib)
(require 'poke-line-types)

(defconst poke-line-directory (file-name-directory (or load-file-name buffer-file-name)))
(defconst poke-line-background-image (concat poke-line-directory "img/background.png"))
(defconst poke-line-modeline-help-string "Gotta catch 'em all!\nmouse-1: Scroll buffer position")
(defconst poke-line-size 3)

(defun poke-line-refresh ()
  "Refresh poke-line.
Intended to be called when customizations were changed, to reapply them immediately."
  (force-mode-line-update))

(defgroup poke-line nil
  "Customization group for `poke-line'."
  :group 'frames)

(defcustom poke-line-pokemon "charmander"
  "Name of the chosen Pokemon."
  :type '(string :tag "name")
  :set (lambda (sym val)
         (set-default sym val)
         (poke-line-refresh))
  :options (mapcar 'car poke-line-pokemon-types)
  :group 'poke-line
  :safe t)

(defcustom poke-line-minimum-window-width 64
  "Minimum width of the window, below which poke-line will not be displayed.
This is important because poke-line will push out all informations from small windows."
  :type 'integer
  :set (lambda (sym val)
         (set-default sym val)
         (poke-line-refresh))
  :group 'poke-line)

(defcustom poke-line-bar-length 32
  "Length of Poke element bar in units.
Each unit is equal to an 8px image.
Minimum of 3 units are required for poke-line."
  :type 'integer
  :set (lambda (sym val)
         (set-default sym val)
         (poke-line-refresh))
  :group 'poke-line)

(defun poke-line-set-pokemon (name)
  "Choose active pokemon by NAME."
  (interactive
   (list (completing-read "Who do you choose? "
                          (mapcar #'car poke-line-pokemon-types) nil 'require-match)))
  (let* ((pokemon (assoc-string name poke-line-pokemon-types)))
    (unless pokemon (user-error "Couldn't find Pokemon \"%s\"" name))
    (setq poke-line-pokemon name)
    (poke-line-refresh)
    (message "%s, I choose you!" (upcase-initials name))))

(defun poke-line-set-random-pokemon ()
  "Choose a Pokemon at random."
  (interactive)
  (setq poke-line-pokemon
        (car (nth (random (length poke-line-pokemon-types))
                  poke-line-pokemon-types)))
  (poke-line-refresh))

(defun poke-line-get-pokemon-image ()
  "Get path to Pokemon PNG image."
  (concat poke-line-directory "img/pokemon/" poke-line-pokemon ".png"))

;; Avoid alist lookup of type each time the mode line renders
(defconst poke-line-element-by-name-table (make-hash-table :test 'equal))
(pcase-dolist (`(,name . ,type) poke-line-pokemon-types)
  (puthash name type poke-line-element-by-name-table))

(defun poke-line-get-element-image ()
  "Get path to Pokemon PNG image."
  (concat poke-line-directory "img/elements/"
          (gethash poke-line-pokemon poke-line-element-by-name-table) ".png"))

(defun poke-line-number-of-elements ()
  "Calculate number of elements."
  (round (/ (* (round (* 100
                         (/ (- (float (point))
                               (float (point-min)))
                            (float (point-max)))))
               (- poke-line-bar-length poke-line-size))
            100)))

(defun poke-line-scroll-buffer (percentage buffer)
  "Move point `BUFFER' to `PERCENTAGE' percent in the buffer."
  (interactive)
  (with-current-buffer buffer
    (goto-char (floor (* percentage (point-max))))))

(defun poke-line-add-scroll-handler (string percentage buffer)
  "Propertize `STRING' to scroll `BUFFER' to `PERCENTAGE' on click."
  (let ((percentage percentage)
        (buffer buffer))
    (propertize string 'keymap `(keymap (mode-line keymap (down-mouse-1 . ,(lambda () (interactive) (poke-line-scroll-buffer percentage buffer))))))))

(defun poke-line-image-string (str image-path)
  "Return STR propertized to display the PNG at IMAGE-PATH."
      (propertize str 'display
                  (create-image image-path 'png nil :ascent 'center :mask 'heuristic)))

(defun poke-line-create ()
  "Return the Pokemon indicator to be inserted into mode line."
  (if (< (window-width) poke-line-minimum-window-width)
      "" ;; Disable for small windows
    (let* ((elements (poke-line-number-of-elements))
           (backgrounds (- poke-line-bar-length elements poke-line-size))
           (element-string "")
           (background-string "")
           (buffer (current-buffer)))
      (dotimes (number elements)
        (setq element-string
              (concat element-string
                      (poke-line-add-scroll-handler
                       (poke-line-image-string "|" (poke-line-get-element-image))
                       (/ (float number) poke-line-bar-length) buffer))))
      (dotimes (number backgrounds)
        (setq background-string
              (concat background-string
                      (poke-line-add-scroll-handler
                       (poke-line-image-string "-" poke-line-background-image)
                       (/ (float (+ elements poke-line-size number)) poke-line-bar-length) buffer))))
      ;; Compute Poke Cat string.
      (propertize
       (concat
        (poke-line-image-string "|||" (poke-line-get-pokemon-image))
        element-string
        background-string
        " ")
       'help-echo (format "%s (%s)\n%s"
                          (capitalize poke-line-pokemon)
                          (gethash poke-line-pokemon poke-line-element-by-name-table)
                          poke-line-modeline-help-string)))))

;;;###autoload
(define-minor-mode poke-line-mode
  "Use Pokemon to show buffer position in mode-line.
You can customize this minor mode, see option `poke-line-mode'.

Note: If you turn this mode on then you probably want to turn off
option `scroll-bar-mode'."
  :global nil
  :group 'poke-line)

;;;###autoload
(define-globalized-minor-mode poke-line-global-mode poke-line-mode
  (lambda () (poke-line-mode 1))
  :global t
  :group 'poke-line)

;; Add a mode line entry that will only ever be displayed if the mode is active
(add-to-list 'mode-line-position '(poke-line-mode (:eval (list (poke-line-create)))))

(provide 'poke-line)

;;; poke-line.el ends here
