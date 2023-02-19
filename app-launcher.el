;;; app-launcher.el --- Launch applications -*- lexical-binding: t -*-

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

;;; Code:

(require 'xdg)
(require 'cl-lib)

(defgroup app-launcher nil
  "Customizable options for the `app-launcher' package."
  :group 'tools
  :prefix "app-launcher")

(defcustom app-launcher-apps-directories
  (mapcar (lambda (dir) (expand-file-name "applications" dir))
	  (cons (xdg-data-home)
		(xdg-data-dirs)))
  "Directories in which to search for applications (.desktop files)."
  :type '(repeat directory))

(defcustom app-launcher--action-function #'app-launcher--action-function-default
  "Define the function that is used to run the selected application."
  :type 'function)

(defcustom app-launcher-icon-themes '("Papirus" "Adwaita" "hicolor")
  "Icon themes to use for app icons in priority order."
  :type '(choice
          (const :tag "None" nil)
          (repeat :tag "Path" string)))

(defvar app-launcher--cache nil
  "Cache of desktop files data.")

(defvar app-launcher--cache-timestamp nil
  "Time when we last updated the cached application list.")

(defvar app-launcher--cached-files nil
  "List of cached desktop files.")

(defconst app-launcher--icon-sizes '("scalable" "22x22" "24x24" "32x32" "36x36"
                                     "48x48" "64x64" "72x72" "96x96" "128x128"
                                     "192x192" "256x256" "512x512" "16x16")
  "An list of possible icon-size directory names, ordered by preference.")

(defvar app-launcher-icons-directories
  (mapcar (lambda (dir) (expand-file-name "icons" dir))
          (cons (xdg-data-home)
                (xdg-data-dirs)))
  "Directories to search for icon themes.")

(defvar app-launcher-icon-extensions '(".svg" ".png")
  "Icon extensions to use for application icons in priority order.")

(defun app-launcher--icon-path ()
  "Return a list of directories to search for icons, in priority order."
  (let (search-path)
    (dolist (theme app-launcher-icon-themes)
      (dolist (dir app-launcher-icons-directories)
        (when-let* ((themedir (expand-file-name theme dir))
                    (file-directory-p themedir))
          (dolist (size app-launcher--icon-sizes)
            (when-let* ((path (expand-file-name (concat size "/apps/") themedir))
                        (file-directory-p path))
              (push path search-path))))))
    (nreverse search-path)))

(defun app-launcher--get-icon (path icon)
  "Find the requested ICON in the requested PATH."
  (when-let* ((icon-file (locate-file icon path app-launcher-icon-extensions)))
    (create-image icon-file nil nil :ascent 'center
                  :scale 1.0
                  :height '(1.0 . ch))))

(defun app-launcher-list-desktop-files ()
  "Return an alist of all Linux applications.
Each list entry is a pair of (desktop-name . desktop-file).
This function always returns its elements in a stable order."
  (let ((hash (make-hash-table :test #'equal))
	result)
    (dolist (dir app-launcher-apps-directories)
      (when (file-exists-p dir)
	(let ((dir (file-name-as-directory dir)))
	  (dolist (file (directory-files-recursively dir ".*\\.desktop$" nil nil t))
	    (let ((id (subst-char-in-string ?/ ?- (file-relative-name file dir))))
	      (when (and (not (gethash id hash)) (file-readable-p file))
		(push (cons id file) result)
		(puthash id file hash)))))))
    result))

(defun app-launcher-parse-files (files)
  "Parse the .desktop FILES to return usable informations."
  (let ((hash (make-hash-table :test #'equal))
        (iconpath (app-launcher--icon-path)))
    (dolist (entry files hash)
      (let ((file (cdr entry)))
	(with-temp-buffer
	  (insert-file-contents file)
	  (goto-char (point-min))
	  (let ((start (re-search-forward "^\\[Desktop Entry\\] *$" nil t))
		(end (re-search-forward "^\\[" nil t))
		(visible t)
		name comment exec icon path)
	    (catch 'break
	      (unless start
		(message "Warning: File %s has no [Desktop Entry] group" file)
		(throw 'break nil))

	      (goto-char start)
	      (when (re-search-forward "^\\(Hidden\\|NoDisplay\\) *= *\\(1\\|true\\) *$" end t)
		(setq visible nil))
	      (setq name (match-string 1))

	      (goto-char start)
	      (unless (re-search-forward "^Type *= *Application *$" end t)
		(throw 'break nil))
	      (setq name (match-string 1))

	      (goto-char start)
	      (unless (re-search-forward "^Name *= *\\(.+\\)$" end t)
		(message "Warning: File %s has no Name" file)
		(throw 'break nil))
	      (setq name (match-string 1))

	      (goto-char start)
	      (when (re-search-forward "^Comment *= *\\(.+\\)$" end t)
		(setq comment (match-string 1)))

	      (goto-char start)
	      (when (re-search-forward "^Path *= *\\(.+\\)$" end t)
		(setq path (match-string 1)))

	      (goto-char start)
	      (unless (re-search-forward "^Exec *= *\\(.+\\)$" end t)
		;; Don't warn because this can technically be a valid desktop file.
		(throw 'break nil))
	      (setq exec (match-string 1))

	      (goto-char start)
	      (when (re-search-forward "^TryExec *= *\\(.+\\)$" end t)
		(let ((try-exec (match-string 1)))
		  (unless (locate-file try-exec exec-path nil #'file-executable-p)
		    (throw 'break nil))))

              (when iconpath
                (goto-char start)
                (setq icon (propertize
                            " " 'display
                            (or (and (re-search-forward "^Icon *= *\\(.+\\)$" end t)
                                     (app-launcher--get-icon iconpath (match-string 1)))
                                '(space :width height)))))

              (puthash name
                       `((name . ,name)
                         (file . ,file)
                         (exec . ,exec)
                         (path . ,path)
                         (icon . ,icon)
                         (comment . ,comment)
                         (visible . ,visible))
                       hash))))))))

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

(defun app-launcher--action-function-default (selected)
  "Default function used to run the SELECTED application."
  (let* ((exec (alist-get 'exec selected))
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

(defun app-launcher--affixate (align candidate)
  "Return the annotated CANDIDATE with the description aligned to ALIGN."
  (let-alist candidate
    (list .name
          (when .icon (concat .icon " "))
          (if .comment
              (concat (propertize " " 'display `(space :align-to ,align))
                      " "
                      (propertize .comment 'face 'completions-annotations))
            ""))))

(defun app-launcher--make-affixation-fn (table)
  "Return an affixation function for `app-launcher' completions."
  (let ((col 20))
    (lambda (completions)
      (setq col (max col (or (cl-loop for c in completions maximize (+ 10 (string-width c))) 0)))
      (mapcar (lambda (c) (app-launcher--affixate col (gethash c table))) completions))))

;;;###autoload
(defun app-launcher-run-app (&optional arg)
  "Launch an application installed on your machine.
When ARG is non-nil, ignore NoDisplay property in *.desktop files."
  (interactive)
  (let* ((candidates (app-launcher-list-apps))
         (table (completion-table-with-metadata
                 candidates
                 `((affixation-function . ,(app-launcher--make-affixation-fn candidates))
                   (category . app-launcher))))
         (result (completing-read
                  "Run app: "
                  table
                  (lambda (_ y) (if arg t (cdr (assq 'visible y))))
                  t nil 'app-launcher nil nil)))
    (funcall app-launcher--action-function (gethash result candidates))))

;; Provide the app-launcher feature
(provide 'app-launcher)

;;; app-launcher.el ends here
