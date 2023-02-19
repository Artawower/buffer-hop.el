;;; buffer-hop.el --- buffer-hop                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/buffer-hop.el
;; Package-Requires: ((emacs "24.4"))
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
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
;;; Package for navigation between visited buffers with respect to
;;; their order of visitation.
;;

;;; Code:
(require 'cl-lib)
(require 'seq)

(defcustom buffer-hop-ignored-buffers-patterns '("^\\*" "^magit" "^COMMIT_EDITMSG")
  "List of regexps to match ignored buffers."
  :type '(repeat string)
  :group 'buffer-hop)

(defcustom buffer-hop-disabled-modes '(minibuffer-mode)
  "List of modes to ignore."
  :type '(repeat symbol)
  :group 'buffer-hop)

(defvar buffer-hop--get-ordered-persp-buffers nil
  "List of buffers in the order to open for active window.")

(defvar buffer-hop--locked-buffer nil
  "Previous visible buffer.")

(defvar buffer-hop--buffer-changed-hook nil
  "Hook to run when buffer is changed.")

(defun bh--store-new-buffer ()
  "Store the current buffer in the list of buffers to be saved."
  (unless (or (cl-some (lambda (regexp)
                         (or (string-match-p regexp (string-trim (buffer-name)))
                             (member major-mode buffer-hop-disabled-modes)))
                       buffer-hop-ignored-buffers-patterns)
              (string= (buffer-name (current-buffer)) buffer-hop--locked-buffer))

    (let* ((persp-name (bh--get-persp-name))
           (stored-persp (assoc persp-name buffer-hop--get-ordered-persp-buffers))
           (ordered-buffer-list (if stored-persp
                                    (cdr stored-persp)
                                  '()))
           (current-buffer-position (seq-position ordered-buffer-list
                                                  bh--locked-buffer))
           (current-buffer-position (when current-buffer-position
                                  (1+ current-buffer-position))))

      (if (and (length> ordered-buffer-list 0) current-buffer-position)
          (setq ordered-buffer-list (append (cl-subseq ordered-buffer-list 0 current-buffer-position)
                                            (list (buffer-name (current-buffer)))
                                            (cl-subseq ordered-buffer-list current-buffer-position)))
        (setq ordered-buffer-list (list (buffer-name (current-buffer)))))


      (if stored-persp
          (setcdr stored-persp ordered-buffer-list)
        (push (cons persp-name ordered-buffer-list) buffer-hop--get-ordered-persp-buffers))))



  (when (not (equal (current-buffer) buffer-hop--locked-buffer))
    (setq buffer-hop--locked-buffer (buffer-name (current-buffer)))))


(defun bh--get-persp-name ()
  "Get persp name of current opened buffer or return default."
  (if (bound-and-true-p persp-mode)
      (persp-name (get-current-persp))
    "default"))

(defun bh--change-buffer (buffer)
  "Change buffer to BUFFER."
  (setq buffer-hop--locked-buffer buffer)
  (switch-to-buffer buffer))

(defun bh--move-to-buffer (direction)
  "Move to the next buffer in the list of buffers to be saved.

DIRECTION is the direction to move, it can be `forward' or `backward'."
  (let* ((persp-name (bh--get-persp-name))
         (stored-persp (assoc persp-name buffer-hop--get-ordered-persp-buffers))
         (ordered-buffer-list (cdr-safe stored-persp))
         (navigation-list (if (equal direction 'forward)
                              ordered-buffer-list
                            (reverse ordered-buffer-list)))
         (before-current-buffer (member (buffer-name) navigation-list))
         (next-buffer (or (car-safe (cdr-safe before-current-buffer))
                          (cl-first navigation-list))))
    (when next-buffer
      (bh--change-buffer next-buffer))))

;;;###autoload
(defun bh-next ()
  "Navigate to the next visited buffer."
  (interactive)
  (if (bound-and-true-p buffer-hop-mode)
      (bh--move-to-buffer 'forward)
    (next-buffer)))

;;;###autoload
(defun bh-prev ()
  "Navigate to the previous visited buffer."
  (interactive)
  (if (bound-and-true-p buffer-hop-mode)
      (bh--move-to-buffer 'backward)
    (previous-buffer)))

;;;###autoload
(define-minor-mode buffer-hop-mode
  "Buffer hop mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When `buffer-hop-mode' is enabled, all buffer navigation will be stored"
  :init-value nil
  :global nil
  :lighter nil
  :group 'buffer-hop
  (unless (member buffer-hop--buffer-changed-hook window-buffer-change-functions)
    (add-to-list 'window-buffer-change-functions
                 (lambda (&rest _)
                   (run-hooks 'buffer-hop--buffer-changed-hook))))
  (if buffer-hop-mode
      (progn
        (bh--store-new-buffer)
        (add-hook 'buffer-hop--buffer-changed-hook #'bh--store-new-buffer))
    (setq buffer-hop--get-ordered-persp-buffers nil)
    (remove-hook 'buffer-hop--buffer-changed-hook #'bh--store-new-buffer)))

;;;###autoload
(define-globalized-minor-mode
  global-buffer-hop-mode
  buffer-hop-mode
  (lambda ()
    (unless buffer-hop-mode
      (buffer-hop-mode)))
  :group 'buffer-hop)




(provide 'buffer-hop)

;; Local Variables:
;; read-symbol-shorthands: (("bh-" . "buffer-hop-"))
;; End:

;;; buffer-hop.el ends here
