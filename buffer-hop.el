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

(defcustom buffer-hop-ignored-buffers-patterns '("^\\*" "^magit")
  "List of regexps to match ignored buffers."
  :type '(repeat string)
  :group 'buffer-hop)

(defcustom buffer-hop-disabled-modes '(minibuffer-mode)
  "List of modes to ignore."
  :type '(repeat symbol)
  :group 'buffer-hop)

(defvar bh--get-ordered-persp-buffers nil
  "List of buffers in the order to open for active window.")

(defun bh--store-new-buffer ()
  "Store the current buffer in the list of buffers to be saved."
  (unless (cl-some (lambda (regexp)
                     (or (string-match-p regexp (string-trim (buffer-name)))
                         (member major-mode buffer-hop-disabled-modes)))
                   buffer-hop-ignored-buffers-patterns)

    (let* ((persp-name (bh--get-persp-name))
           (stored-persp (assoc persp-name bh--get-ordered-persp-buffers))
           (ordered-buffer-list (if stored-persp
                                    (cdr stored-persp)
                                  '())))

      ;; (setq ordered-buffer-list (delete (buffer-name) ordered-buffer-list))
      (unless (member (buffer-name) ordered-buffer-list)
        (setq ordered-buffer-list (append ordered-buffer-list (list (buffer-name)))))

      (message "stored window: %s" stored-persp)
      (if stored-persp
          (setcdr stored-persp ordered-buffer-list)
        (push (cons persp-name ordered-buffer-list) bh--get-ordered-persp-buffers))


      ;; (unless (member buffer ordered-buffer-list)
      ;;   (setcdr stored-persp (cons buffer ordered-buffer-list)))
      (message "Stored buffer %s" bh--get-ordered-persp-buffers)
      )))

(defun bh--get-persp-name ()
  "Get persp name of current opened buffer or return default."
  (if (bound-and-true-p persp-mode)
      (persp-name (get-current-persp))
    "default"))

(defun bh--change-buffer (buffer)
  "Change buffer to BUFFER."
  (remove-hook 'buffer-list-update-hook #'bh--store-new-buffer)
  (switch-to-buffer buffer)
  (add-hook 'buffer-list-update-hook #'bh--store-new-buffer)
  (message "Switched to buffer %s" buffer))

(defun bh--move-to-buffer (direction)
  "Move to the next buffer in the list of buffers to be saved.

DIRECTION is the direction to move, it can be `forward' or `backward'."
  (let* ((persp-name (bh--get-persp-name))
         (stored-persp (assoc persp-name bh--get-ordered-persp-buffers))
         (ordered-buffer-list (cdr-safe stored-persp))
         (navigation-list (if (equal direction 'forward)
                                                          ordered-buffer-list
                                                        (reverse ordered-buffer-list)))
         (before-current-buffer (member (buffer-name) navigation-list))
         (next-buffer (or (car-safe (cdr-safe before-current-buffer))
                          (cl-first navigation-list))))

    (message "Before current buffer %s" before-current-buffer)
    (message "memb: %s" (member (buffer-name) navigation-list))
    (message "next buffer %s" next-buffer)

    (when next-buffer
      (bh--change-buffer next-buffer))))

;;;###autoload
(defun bh-next ()
  "Navigate to the next visited buffer."
  (interactive)
  (bh--move-to-buffer 'forward))

;;;###autoload
(defun bh-prev ()
  "Navigate to the previous visited buffer."
  (interactive)
  (bh--move-to-buffer 'backward))

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
  (if buffer-hop-mode
      (add-hook 'buffer-list-update-hook #'bh--store-new-buffer)
    (setq bh--get-ordered-persp-buffers nil)
    (remove-hook 'buffer-list-update-hook #'bh--store-new-buffer)))

(provide 'buffer-hop)

;; Local Variables:
;; read-symbol-shorthands: (("bh-" . "buffer-hop-"))
;; End:

;;; buffer-hop.el ends here
