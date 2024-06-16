;;; init.el --- jcreed's emacs setup, derived from Patrick Thomson's.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; This file loads use-package, org-mode, and compiles and executes readme.org
;;
;;; Code:

;; This seems to be required on emacs 26.1 to get package-refresh-contents
;; to not yield "bad request".
;; (taken from https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341)
(if (equal emacs-version "26.1")
 (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

; (require 'org-install)

(defvar init-dir
  (if load-file-name
       (file-name-directory (file-truename load-file-name))
    default-directory))

(defun tangle-config ()
  "Convert orgfile to elisp with a perl script because org-tangle
   is very slow. (>2000ms)"
  (interactive)
  (let ((source-file "~/.config/emacs/emacs-config.org")
		  (target-file "~/.config/emacs/emacs-config.el"))
	 (message (format "Running in directory %s..." init-dir))
	 (shell-command (format "%s <%s >%s"
				(expand-file-name "tangle.pl" init-dir)
				source-file
				target-file))
	 (message (format "Wrote file %s" target-file))
	 target-file))

(defun reload-config ()
  "Reload configuration"
  (interactive)
  (load-file (tangle-config)))

(setq max-lisp-eval-depth 2000)

(reload-config)

;;; init.el ends here
