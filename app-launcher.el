;;; app-launcher.el --- Launch applications from Emacs -*- lexical-binding: t -*-

;; Author: Sebastien Waegeneire
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/sebastienwae/app-launcher

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

;; app-launcher define the `app-launcher-run-app' command which uses
;; Emacs standard completion feature to select an application installed
;; on your machine and launch it.

;;; Acknowledgements:

;; This package uses code from the Counsel package by Oleh Krehel.
;; https://github.com/abo-abo/swiper

(require 'xdg)
(require 'cl-seq)

(defcustom app-launcher-apps-directories
  (mapcar (lambda (dir) (expand-file-name "applications" dir))
	  (cons (xdg-data-home)
		(xdg-data-dirs)))
  "Directories in which to search for applications (.desktop files)."
  :type '(repeat directory))

(defcustom app-launcher--annotation-function #'app-launcher--annotation-function-default
  "Define the function that genereate the annotation for each completion choices."
  :type 'function)

(defcustom app-launcher--action-function #'app-launcher--action-function-default
  "Define the function that is used to run the selected application."
  :type 'function)

(defvar app-launcher--cache nil
  "Cache of desktop files data.")

(defvar app-launcher--cache-timestamp nil
  "Time when we last updated the cached application list.")

(defvar app-launcher--cached-files nil
  "List of cached desktop files.")

(defun app-launcher-list-desktop-files ()
  "Return an alist of all Linux applications.
Each list entry is a pair of (desktop-name . desktop-file).
This function always returns its elements in a stable order."
  (let ((hash (make-hash-table :test #'equal))
	result)
    (dolist (dir app-launcher-apps-directories)
      (when (file-exists-p dir)
	(let ((dir (file-name-as-directory dir)))
	  (dolist (file (directory-files-recursively dir ".*\\.desktop$"))
	    (let ((id (subst-char-in-string ?/ ?- (file-relative-name file dir))))
	      (when (and (not (gethash id hash)) (file-readable-p file))
		(push (cons id file) result)
		(puthash id file hash)))))))
    result))

(defun app-launcher--is-installed (tryexec)
  "Check if TRYEXEC file is installed in the \"exec-path\"."
  (or (not tryexec)
      (locate-file tryexec exec-path nil #'file-executable-p)))

(defun app-launcher-parse-files (files)
  "Parse the .desktop FILES to return usable informations."
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (entry files hash)
      (when-let* ((file (cdr entry))
                  (parsed (xdg-desktop-read-file file))
                  (name (gethash "Name" parsed))
                  (exec (gethash "Exec" parsed))
                  ((string= (gethash "Type" parsed) "Application"))
                  ((app-launcher--is-installed (gethash "TryExec" parsed))))

        (puthash name
                 (list (cons 'file file)
                       (cons 'exec exec)
                       (cons 'path (gethash "Path" parsed))
                       (cons 'comment (gethash "Comment" parsed))
                       (cons 'visible
                             (not (string= "true" (or (gethash "Hidden" parsed)
                                                      (gethash "NoDisplay" parsed))))))
                 hash)))))

(defun app-launcher-list-apps ()
  "Return list of all Linux .desktop applications."
  (let* ((new-desktop-alist (app-launcher-list-desktop-files))
	 (new-files (mapcar 'cdr new-desktop-alist)))
    (unless (and (equal new-files app-launcher--cached-files)
		 (null (cl-find-if
			(lambda (file)
			  (time-less-p
			   app-launcher--cache-timestamp
			   (nth 5 (file-attributes file))))
			new-files)))
      (setq app-launcher--cache (app-launcher-parse-files new-desktop-alist))
      (setq app-launcher--cache-timestamp (current-time))
      (setq app-launcher--cached-files new-files)))
  app-launcher--cache)

(defun app-launcher--annotation-function-default (choice)
  "Default function to annotate the completion choices."
  (let ((str (cdr (assq 'comment (gethash choice app-launcher--cache)))))
    (when str (concat " - " (propertize str 'face 'completions-annotations)))))

(defun app-launcher--action-function-default (selected)
  "Default function used to run the selected application."
  (let* ((exec (cdr (assq 'exec (gethash selected app-launcher--cache))))
	 (command (let (result)
		    (dolist (chunk (split-string exec " ") result)
		      (unless (or (equal chunk "%U")
				  (equal chunk "%F")
				  (equal chunk "%u")
				  (equal chunk "%f"))
			(setq result (concat result chunk " "))))))
         (default-directory
          (or (cdr (assq 'path (gethash selected app-launcher--cache)))
              default-directory)))
    (call-process-shell-command command nil 0 nil)))

;;;###autoload
(defun app-launcher-run-app (&optional arg)
  "Launch an application installed on your machine.
When ARG is non-nil, ignore NoDisplay property in *.desktop files."
  (interactive)
  (let* ((candidates (app-launcher-list-apps))
	 (result (completing-read
		  "Run app: "
		  (lambda (str pred flag)
		    (if (eq flag 'metadata)
			'(metadata
			  (annotation-function . (lambda (choice)
						   (funcall
						    app-launcher--annotation-function
						    choice))))
		      (complete-with-action flag candidates str pred)))
		  (lambda (x y)
		    (if arg
			t
		      (cdr (assq 'visible y))))
		  t nil 'app-launcher nil nil)))
    (funcall app-launcher--action-function result)))

;; Provide the app-launcher feature
(provide 'app-launcher)

;;; app-launcher.el ends here
