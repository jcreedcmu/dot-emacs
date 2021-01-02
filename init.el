;;; init.el --- jcreed's emacs setup, derived from Patrick Thomson's.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; This file loads use-package, org-mode, and compiles and executes readme.org
;;
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'org-install)
(require 'ob-tangle)

(defun reload-config ()
  "Reload the literate config config."
  (interactive)
  (let ((vc-follow-symlinks t))
      (org-babel-load-file "~/.config/emacs/emacs-config.org")))

(setq max-lisp-eval-depth 2000)

(reload-config)

;;; init.el ends here
