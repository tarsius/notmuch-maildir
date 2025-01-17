;;; notmuch-maildir.el --- Display maildirs as a tree  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2021 Free Software Foundation, Inc.
;; Copyright (C) 2021-2025 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.notmuch-maildir@jonas.bernoulli.dev>
;; Homepage: https://git.sr.ht/~tarsius/notmuch-maildir
;; Keywords: mail

;; Package-Version: 1.1.0
;; Package-Requires: ((emacs "26.1") (compat "30.0.0.0") (notmuch "0.38"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package displays maildirs hierarchically in Notmuch's hello
;; buffer.  Call `notmuch-maildir-inject-section' to enable this.

;;; Code:

(require 'compat)
(require 'notmuch)

(defvar notmuch-maildir-separator-regexp "[-.]")
(defvar notmuch-maildir-nosplit-regexp "[[@]")

(defvar notmuch-maildir-section-title "Maildirs")

(defun notmuch-hello-insert-maildirs ()
  "Insert the maildir tree section."
  (widget-insert notmuch-maildir-section-title ": ")
  (cond
   ((member notmuch-maildir-section-title notmuch-hello-hidden-sections)
    (widget-create 'push-button
		   :notify (lambda (&rest _ignore)
			     (setq notmuch-hello-hidden-sections
				   (delete notmuch-maildir-section-title
                                           notmuch-hello-hidden-sections))
			     (notmuch-hello-update))
		   "show")
    (widget-insert "\n"))
   (t
    (widget-create 'push-button
		   :notify (lambda (&rest _ignore)
			     (add-to-list 'notmuch-hello-hidden-sections
					  notmuch-maildir-section-title)
			     (notmuch-hello-update))
		   "hide")
    (widget-insert "\n\n")
    (pcase-dolist (`(,directory ,maildir) (notmuch-maildir--list-directories))
      (let* ((parts  (mapcan
                      (lambda (part)
                        (if (string-match-p notmuch-maildir-nosplit-regexp part)
                            (list part)
                          (split-string part notmuch-maildir-separator-regexp)))
                      (split-string maildir "[/\\]")))
             (depth  (1- (length parts)))
             (name   (car (last parts)))
             (string (concat (make-string (* 2 depth) ?\s) name)))
        (if (notmuch-maildir-p directory)
            (let* ((query  (format "folder:%s" maildir))
                   (unread (read (car (process-lines
                                       notmuch-command "count"
                                       (concat query " tag:unread")))))
                   (total  (read (car (process-lines
                                       notmuch-command "count"
                                       query))))
                   (widget-push-button-prefix "")
                   (widget-push-button-suffix ""))
              (widget-create 'push-button
                             :notify #'notmuch-hello-widget-search
                             :notmuch-search-terms query
                             string)
              (widget-insert (make-string (max 0 (- 30 (current-column))) ?\s))
              (widget-insert (propertize (format " [%s/%s]" unread total) 'face
                                         (if (zerop unread) 'default 'bold))))
          (widget-insert (propertize string 'face 'bold)))
        (widget-insert "\n"))))))

(defun notmuch-maildir-p (directory)
  (file-accessible-directory-p (expand-file-name "new" directory)))

(defun notmuch-maildir--mail-root ()
  (file-name-as-directory
   (car (or (process-lines notmuch-command "config" "get" "database.mail_root")
            (process-lines notmuch-command "config" "get" "database.path")))))

(defun notmuch-maildir--list-directories ()
  (let* ((directory (notmuch-maildir--mail-root))
         (offset (length directory)))
    (mapcar (lambda (dir) (list dir (substring dir offset)))
            (notmuch-maildir--list-directories-1 directory))))

(defun notmuch-maildir--list-directories-1 (directory)
  (mapcan (lambda (dir)
            (and (file-accessible-directory-p dir)
                 (if (notmuch-maildir-p dir)
                     (list dir)
                   (cons dir (notmuch-maildir--list-directories-1 dir)))))
          (directory-files directory t "^[^.]")))

;;;###autoload
(defun notmuch-maildir-inject-section ()
  "Inject `notmuch-hello-insert-maildirs' into `notmuch-hello-sections'."
  (unless (member 'notmuch-hello-insert-maildirs notmuch-hello-sections)
    (let ((cons (member 'notmuch-hello-insert-footer notmuch-hello-sections)))
      (setcdr cons (cons (car cons) (cdr cons)))
      (setcar cons 'notmuch-hello-insert-maildirs))))

;;; _
(provide 'notmuch-maildir)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; notmuch-maildir.el ends here
