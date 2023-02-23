;;; buffer-hop.el --- buffer-hop                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/artawower/buffer-hop.el
;; Package-Requires: ((emacs "28.1"))
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

(defcustom bh-ignored-buffers-patterns '("^\\*" "^magit" "^COMMIT_EDITMSG")
  "List of regexps to match ignored buffers."
  :type '(repeat string)
  :group 'buffer-hop)

(defcustom bh-disabled-modes '(minibuffer-mode dired-mode)
  "List of modes to ignore."
  :type '(repeat symbol)
  :group 'buffer-hop)

(defcustom bh-always-allowed-buffers-patterns '("\\*new\\*")
  "List of regexps to match always allowed buffers."
  :type '(repeat string)
  :group 'buffer-hop)

(defvar bh--get-ordered-persp-buffers nil
  "List of buffers in the order to open for active window.")

(defvar bh--persp-window-locked-buffers nil
  "Nested list of buffers locked for windows and persps.")

(setq bh--get-ordered-persp-buffers nil)

(defvar bh--buffer-changed-hook nil
  "Hook to run when buffer is changed.")

(defvar bh--previous-window-state nil
  "The state of the previous window, used to copy state when window splitted.")

(defun bh--allow-store-buffer-p (buffer-name)
  "Check if BUFFER-NAME should be stored."
  (or (not (cl-some (lambda (regexp)
                      (or (string-match-p regexp (string-trim buffer-name))
                          (member major-mode bh-disabled-modes)))
                    bh-ignored-buffers-patterns))
      (cl-some (lambda (regexp)
                 (string-match-p regexp (string-trim buffer-name)))
               bh-always-allowed-buffers-patterns)))

(defun bh--get-persp-name ()
  "Get persp name of current opened buffer or return default."
  (if (and (fboundp 'safe-persp-name) (fboundp 'get-current-persp))
      (safe-persp-name (get-current-persp))
    "default"))

(defun bh--get-window-id ()
  "Get window id of current opened buffer."
  ;; NOTE: I didn't find how to extract ID of the window correctly 🤷‍♂️
  ;; Spent fucking hour finding solution. If you read this and know how to do it,
  ;; please, let me know.
  (bh--extract-window-id (get-buffer-window (current-buffer))))

(defun bh--extract-window-id (window)
  "Return window id from provided WINDOW."
  (let ((window-name (format "%s" window)))
    (string-match "window \\([0-9]+\\)" window-name)
    (string-to-number (match-string 1 window-name))))

(defun bh--init-persp-window-store ()
  "Init storage for persp and windows if not exist."
  (let* ((persp-name (bh--get-persp-name))
         (window-id (bh--get-window-id))
         (stored-persps (assoc persp-name bh--get-ordered-persp-buffers))
         (stored-windows (when stored-persps (assoc window-id (cdr stored-persps)))))

    (unless stored-persps
      (push (cons persp-name nil) bh--get-ordered-persp-buffers)
      (setq stored-persps (assoc persp-name bh--get-ordered-persp-buffers)))

    (unless stored-windows
      (setf (cdr stored-persps)
            (push (cons window-id bh--previous-window-state) (cdr stored-persps))))
    (setq bh--previous-window-state nil)))

(defun bh--get-locked-buffer ()
  "Return current locked buffer for current persp and window."
  (let* ((persp-name (bh--get-persp-name))
         (window-id (bh--get-window-id))
         (stored-persp (assoc persp-name bh--persp-window-locked-buffers))
         (stored-window (when stored-persp (assoc window-id (cdr stored-persp)))))
    (cdr-safe stored-window)))

(defun bh--buffer-locked-p (buffer-name)
  "Check if BUFFER-NAME is locked."
  (when-let ((locked-buffer (bh--get-locked-buffer)))
    (string= buffer-name locked-buffer)))

(defun bh--lock-buffer (buffer-name)
  "Lock BUFFER-NAME for current window and persp."
  (let* ((persp-name (bh--get-persp-name))
         (window-id (bh--get-window-id))
         (stored-persp (assoc persp-name bh--persp-window-locked-buffers))
         (stored-window (when stored-persp (assoc window-id (cdr stored-persp)))))
    (if stored-window
        (setf (cdr stored-window) buffer-name)

      (unless stored-persp
        (push (cons persp-name nil) bh--persp-window-locked-buffers)
        (setq stored-persp (assoc persp-name bh--persp-window-locked-buffers)))

      (unless stored-window
        (setf (cdr stored-persp)
              (push (cons window-id buffer-name) (cdr stored-persp)))))))


(defun bh--store-new-buffer ()
  "Store the current buffer in the list of buffers to be saved."
  (bh--init-persp-window-store)
  (when (and (bh--allow-store-buffer-p (buffer-name))
             (not (bh--buffer-locked-p (buffer-name))))

    (let* ((persp-name (bh--get-persp-name))
           (stored-persp (assoc persp-name bh--get-ordered-persp-buffers))
           (stored-window (bh--get-window-id))
           (ordered-window-list (assoc stored-window (cdr stored-persp)))
           (ordered-buffer-list (cdr ordered-window-list))
           (current-buffer-position (seq-position ordered-buffer-list
                                                  (bh--get-locked-buffer)))
           (current-buffer-position (when current-buffer-position
                                      (1+ current-buffer-position))))

      (when (member (buffer-name (current-buffer)) ordered-buffer-list)
        (setq ordered-buffer-list (delete (buffer-name (current-buffer)) ordered-buffer-list)))

      (if (and (length> ordered-buffer-list 0)
               current-buffer-position)
          (progn
            (setq ordered-buffer-list (delete (buffer-name) ordered-buffer-list))
            (setq ordered-buffer-list (append (cl-subseq ordered-buffer-list 0 current-buffer-position)
                                              (list (buffer-name (current-buffer)))
                                              (cl-subseq ordered-buffer-list current-buffer-position))))
        (setq ordered-buffer-list (append ordered-buffer-list (list (buffer-name (current-buffer))))))

      (setcdr ordered-window-list ordered-buffer-list))))

(defun bh--store-new-window ()
  "Try to store the new appeared window and buffer attached to it."
  (let* ((window-id (bh--get-window-id))
         (persp-name (bh--get-persp-name))
         (stored-windows (assoc persp-name bh--get-ordered-persp-buffers))
         (stored-window (assoc window-id (cdr stored-windows))))

    (unless stored-window
      (bh--store-new-buffer))
    (setq bh--previous-window-state nil)))

(when (and (not (bh--buffer-locked-p (buffer-name (current-buffer))))
           (bh--allow-store-buffer-p (buffer-name)))
  (bh--lock-buffer (buffer-name (current-buffer))))

(defun bh--change-buffer (buffer)
  "Change buffer to BUFFER."
  (bh--lock-buffer buffer)
  (switch-to-buffer buffer))

(defun bh--move-to-buffer (direction &optional current-buffer-name)
  "Move to the next buffer in the list of buffers to be saved.

DIRECTION is the direction to move, it can be `forward' or `backward'.
CURRENT-BUFFER-NAME is optional arg for recursive search."
  (let* ((persp-name (bh--get-persp-name))
         (stored-persp (assoc persp-name bh--get-ordered-persp-buffers))
         (window-id (bh--get-window-id))
         (stored-windows (when stored-persp (assoc window-id stored-persp)))
         (ordered-buffer-list (cdr-safe stored-windows))
         (navigation-list (if (equal direction 'forward)
                              ordered-buffer-list
                            (reverse ordered-buffer-list)))
         (before-current-buffer (member (or current-buffer-name (buffer-name)) navigation-list))
         (next-buffer (or (car-safe (cdr-safe before-current-buffer))
                          (cl-first navigation-list))))


    (if (and next-buffer (get-buffer next-buffer))
        (bh--change-buffer next-buffer)
      (when next-buffer (bh--move-to-buffer direction next-buffer)))))

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

(defun bh-reset()
  "Reset buffer-hop."
  (interactive)
  (setq bh--get-ordered-persp-buffers nil))

(defun bh--get-list-of-visited-buffers ()
  "Get the list of visited buffers in current window and persp."
  (let* ((persp-name (bh--get-persp-name))
         (stored-persp (assoc persp-name bh--get-ordered-persp-buffers))
         (stored-window (when stored-persp (assoc (bh--get-window-id) (cdr stored-persp)))))
    (cdr-safe stored-window)))

(defun bh--store-window-state (&rest _)
  "Store current window state."
  (let* ((persp-name (bh--get-persp-name))
         (window-name (bh--get-window-id))
         (stored-persps (assoc persp-name bh--get-ordered-persp-buffers))
         (stored-window (assoc window-name (cdr stored-persps)))
         (stored-buffers (cdr stored-window)))
    (setq bh--previous-window-state stored-buffers)))

;;;###autoload
(defun bh-jump-to-recently-buffer ()
  "Hop to visited buffer."
  (interactive)
  (when-let ((choosed-buffer (completing-read "Swtich to buffer: " (bh--get-list-of-visited-buffers))))
    (bh--change-buffer choosed-buffer)))

(defun bh--restore-window-state-from-persp (&rest _)
  "Restore window state after persp switched.

NOTE: this solves problem when user switch current persp,
cause persp recreates windows every time."
  (let* ((window-ids (mapcar #'bh--extract-window-id (window-list)))
         (sorted-window-ids (sort window-ids #'<))
         (persp-name (bh--get-persp-name))
         (stored-persp (assoc persp-name bh--get-ordered-persp-buffers))
         (stored-windows (cdr stored-persp))
         (index 0)
         (new-stored-windows '()))

    (when stored-persp
      (dolist (stored-window stored-windows)
        (when (and (cdr stored-window) (nth index sorted-window-ids))
          (push (cons (nth index sorted-window-ids) (cdr stored-window)) new-stored-windows)
          (setq index (1+ index))))

      (setf (cdr stored-persp) new-stored-windows))))

(defun bh--init-hooks ()
  "Init hooks for buffer-hop."
  (when (boundp 'persp-activated-functions)
    (add-to-list 'persp-activated-functions 'bh--restore-window-state-from-persp))
  (add-hook 'bh--buffer-changed-hook #'bh--store-new-buffer)
  (add-hook 'window-configuration-change-hook #'bh--store-new-window)
  (add-hook 'window-state-change-hook #'bh--store-new-window)
  (advice-add 'split-window :before #'bh--store-window-state))

(defun bh--destroy-hooks ()
  "Destroy hooks for buffer-hop."
  (when (boundp 'persp-activated-functions)
    (setq persp-activated-functions (remove 'bh--restore-window-state-from-persp persp-activated-functions)))
  (remove-hook 'bh--buffer-changed-hook #'bh--store-new-buffer)
  (remove-hook 'window-configuration-change-hook #'bh--store-new-window)
  (remove-hook 'window-state-change-hook #'bh--store-new-window)
  (advice-remove 'split-window #'bh--store-window-state))

(defun bh--init-advices ()
  "Init advices for buffer-hop."
  (unless (member bh--buffer-changed-hook window-buffer-change-functions)
    (add-to-list 'window-buffer-change-functions
                 (lambda (&rest _)
                   (run-hooks 'bh--buffer-changed-hook)))))

;;;###autoload
(define-minor-mode buffer-hop-mode
  "Buffer hop mode.)

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When `buffer-hop-mode' is enabled, all buffer navigation will be stored"
  :init-value nil
  :global nil
  :lighter nil
  :group 'buffer-hop
  (bh--init-advices)
  (if buffer-hop-mode
      (progn
        (when (get-buffer-window (current-buffer))
          (bh--store-new-buffer))
        (bh--init-hooks))

    (setq bh--get-ordered-persp-buffers nil)
    (setq bh--persp-window-locked-buffers nil)
    (bh--destroy-hooks)))

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
