(setq load-path (cons (expand-file-name "~/.site-lisp/") load-path))

(defmacro ifat (loc &rest body) (if (equal location loc) (cons 'progn body) nil))

(load "location.el")

(ifat chef
      (setq mac-command-modifier 'meta))

(defface jcreed-header-face nil "Jcreed Header Face")
(defface jcreed-minor-header-face nil "Jcreed Minor Header Face")
(defface jcreed-question-face nil "Jcreed Question Face")
(defface jcreed-answer-face nil "Jcreed Answer Face")
(defface jcreed-bad-face nil "Jcreed Bad Face")
(defface jcreed-shell-face nil "Jcreed Shell Face")
(defface jcreed-command-face nil "Jcreed Command Face")
(defface jcreed-person-face nil "Jcreed Person Face")
(defface jcreed-task-face nil "Jcreed Task Face")
(defface jcreed-paste-face nil "Jcreed Paste Face")
(defface jcreed-path-face nil "Jcreed Path Face")
(defface jcreed-paper-face nil "Jcreed Paper Face")

(setq solarized-base03    "#002b36")
(setq solarized-base02    "#073642")
(setq solarized-base01    "#586e75")
(setq solarized-base00    "#657b83")
(setq solarized-base0     "#839496")
(setq solarized-base1     "#93a1a1")
(setq solarized-base2     "#eee8d5")
(setq solarized-base3     "#fdf6e3")
(setq solarized-yellow    "#b58900")
(setq solarized-orange    "#cb6b16")
(setq solarized-red       "#dc322f")
(setq solarized-magenta   "#d33682")
(setq solarized-violet    "#6c71c4")
(setq solarized-blue      "#268bd2")
(setq solarized-cyan      "#2aa198")
(setq solarized-green     "#859900")

(blink-cursor-mode 0)

(add-to-list (quote auto-mode-alist) (quote ("\\.tex\\'" . latex-mode)))

(setq frame-title-format  '("Emacs [%b]"))

(defun jcreed-set-frame-title (x)
  (interactive "s")
  (setq frame-title-format (concat "Emacs [%b] --- " x )))

(defun Buffer-menu-other-window ()
 "Select this line's buffer in other window, leaving buffer menu visible?"
 (interactive)

 (let* ((w (selected-window))
	(pop-up-windows nil)
	same-window-buffer-names
	same-window-regexps)
   (pop-to-buffer (Buffer-menu-buffer t) t nil)
   (select-window w)
   ))


(autoload 'python-mode "python-mode" "Python editing mode." t)
(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(autoload 'clojure-mode "clojure-mode" "Clojure editing mode." t)

;;;***

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(allout-command-prefix "")
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-scroll-output (quote first-error))
 '(css-indent-offset 2 t)
 '(current-language-environment "English")
 '(dired-bind-jump t)
 '(face-font-selection-order (quote (:slant :height :weight :width)))
 '(global-font-lock-mode t nil (font-lock))
; '(idris-interpreter-path "/home/jcreed/Idris/.cabal-sandbox/bin/idris")
 '(inhibit-startup-screen t)
 '(load-home-init-file t t)
 '(mouse-yank-at-point t)
 '(safe-local-variable-values (quote ((erlang-indent-level . 4) (css-indent-offset . 2))))
 '(sclang-eval-line-forward nil)
 '(search-whitespace-regexp nil)
 '(sentence-end-double-space nil)
 '(show-paren-mode t nil (paren))
 '(show-trailing-whitespace t)
 '(sml-indent-level 2 t)
 '(tab-always-indent t)
 '(tab-width 3)
 '(transient-mark-mode t)
 '(typescript-indent-level 2)
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-extra-keywords (quote (("javascript" "type" "declare" "global")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 88) (background light)) (:foreground "#073642" :background "#fdf6e3"))))
 '(font-lock-comment-face ((t (:foreground "#93a1a1"))))
 '(font-lock-constant-face ((t (:foreground "#0070ff"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "#dc322f"))))
 '(font-lock-function-name-face ((nil (:foreground "#268bd2"))))
 '(font-lock-keyword-face ((nil (:foreground "#6c71c4" :weight bold))))
 '(font-lock-string-face ((nil (:foreground "#2aa198"))))
 '(font-lock-type-face ((nil (:foreground "#859900" :weight bold))))
 '(font-lock-variable-name-face ((nil (:foreground "#d33682"))))
 '(fuzz-font-lock-annot-face ((((background light)) (:foreground "gray40" :weight bold))))
 '(highlight ((t (:background "#ff0"))))
 '(italic ((((supports :underline t)) (:slant italic))))
 '(jcreed-answer-face ((((class color) (min-colors 88) (background light)) (:foreground "#268bd2"))) t)
 '(jcreed-bad-face ((((class color) (min-colors 88) (background light)) (:foreground "yellow" :background "#dc322f"))) t)
 '(jcreed-command-face ((((class color) (min-colors 88) (background light)) (:foreground "gray20" :weight bold))) t)
 '(jcreed-header-face ((((class color) (min-colors 88) (background light)) (:background "#586e75" :foreground "#fdf6e3"))) t)
 '(jcreed-minor-header-face ((((class color) (min-colors 88) (background light)) (:background "#8ac" :foreground "#fdf6e3"))) t)
 '(jcreed-paste-face ((t (:foreground "#268bd2" :weight bold))) t)
 '(jcreed-path-face ((t (:foreground "#d33682" :weight bold))) t)
 '(jcreed-person-face ((t (:foreground "#6c71c4" :weight bold))) t)
 '(jcreed-question-face ((((class color) (min-colors 88) (background light)) (:foreground "#dc322f"))) t)
 '(jcreed-shell-face ((((class color) (min-colors 88) (background light)) (:foreground "#586e75" :background "#eee8d5"))) t)
 '(jcreed-task-face ((t (:foreground "#2aa198" :weight bold))) t)
 '(jcreed-paper-face ((((class color) (min-colors 88) (background light)) (:background "#77cc77" :foreground "black"))) t)
 '(link ((t (:foreground "#007" :background "#eef"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "black"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "RoyalBlue3"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#2aa198"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#d33682"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#6c71c4"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "gray40"))))
 '(region ((t (:background "#aff"))))
 '(tex-verbatim ((t (:background "gray90"))))
 '(trailing-whitespace ((t (:background "#ffbfbf"))))
 '(twelf-font-decl-face ((t (:inherit nil :stipple nil :background "white" :foreground "blue" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "cbp" :family "Codon"))) t)
 '(twelf-font-fvar-face ((t (:stipple nil :background "white" :foreground "Blue1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 116 :width normal :foundry "cbp" :family "codon"))) t)
 '(xx-font-lock-constructor-face ((t (:foreground "purple3" :weight bold)))))

(ifat baez
		(setq twelf-root "~/tmp/twelf/")
		(load (concat twelf-root "emacs/twelf-init.el"))
)

(ifat baez
      (custom-theme-set-faces
       'user
       '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "cbp" :family "codon"))))
       '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "Firebrick" :slant italic))))
       '(font-lock-constant-face ((t (:foreground "cyan3"))))
       '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "firebrick"))))
       '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "DarkGreen"))))
       '(fuzz-font-lock-annot-face ((((background light)) (:foreground "gray40" :weight bold))))
       '(italic ((((supports :underline t)) (:slant italic :family "codon"))))
       '(rainbow-delimiters-depth-1-face ((t (:foreground "black"))))
       '(rainbow-delimiters-depth-2-face ((t (:foreground "RoyalBlue3"))))
       '(rainbow-delimiters-depth-3-face ((t (:foreground "#2aa198"))))
       '(rainbow-delimiters-depth-4-face ((t (:foreground "#d33682"))))
       '(rainbow-delimiters-depth-5-face ((t (:foreground "#6c71c4"))))
       '(rainbow-delimiters-depth-6-face ((t (:foreground "gray40"))))
       '(region ((t (:background "#aff"))))
       '(tex-verbatim ((t (:background "gray90"))))
       '(trailing-whitespace ((t (:background "#ffbfbf"))))
       '(twelf-font-decl-face ((t (:stipple nil :background "white" :foreground "green4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 96 :width normal :foundry "cbp" :family "Codon"))) t)
       '(twelf-font-fvar-face ((t (:stipple nil :background "white" :foreground "Blue1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant italic :weight normal :height 116 :width normal :family "cbp-codon"))) t)
       '(xx-font-lock-constructor-face ((t (:foreground "purple3" :weight bold))))))

(ifat chef (custom-theme-set-faces
            'user
            '(default ((((class color) (min-colors 88) (background light)) (:foreground "#073642" :background "#fdf6e3"))))
            '(font-lock-comment-face ((nil (:foreground "#93a1a1"))))
            '(font-lock-constant-face ((nil (:foreground "#dc322f"))))
            '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "#dc322f"))))
            '(font-lock-function-name-face ((nil (:foreground "#268bd2"))))
            '(font-lock-keyword-face ((nil (:foreground "#6c71c4" :weight bold))))
            '(font-lock-string-face ((nil (:foreground "#2aa198"))))
            '(font-lock-type-face ((nil (:foreground "#859900" :weight bold))))
            '(font-lock-variable-name-face ((nil (:foreground "#d33682"))))
            '(highlight ((t (:background "#fff"))))
            '(italic ((((supports :underline t)) (:slant italic))))))

; from http://ck.kennt-wayne.de/2013/may/emacs:-jump-to-matching-paren-beginning-of-block
(defun goto-matching-ruby-block ()
  (interactive)
  (cond
   ;; are we at an end keyword?
   ((equal (current-word) "end")
    (ruby-beginning-of-block)) ; not quite right for do not at beginning of line

   ;; or are we at a keyword itself?
   ((string-match (current-word) "\\(else\\|for\\|while\\|until\\|if\\|class\\|module\\|case\\|unless\\|def\\|begin\\|do\\)")
    (ruby-end-of-block)
    (forward-word))))

(defun jcreed-tcons (x) (cons x x))
(setq jcreed-completion (mapcar 'jcreed-tcons '("lemma" "corollary" "theorem" "conjecture" "proposition" "question" "definition" "remark" "postulate" "prooftree" "easyrule")))
(setq jcreed-proof (mapcar (lambda (x) (cons x 1)) '("lemma" "corollary" "theorem")))
(setq jcreed-math (mapcar (lambda (x) (cons x 1)) '("prooftree")))

(defun jcreed-insert-easy-template ()
  "Inserts a copy of my easyrule template"
  (interactive)
  (insert-string "$$\n\\erule\n{}\n{")
  (let ((pm (point-marker)))
    (insert-string "}\n$$")
    (goto-char pm)))

(defun jcreed-insert-other ()
  "Inserts a proposition/lemma/corollary/theorem template."
  (interactive)
  (let ((env (completing-read "Environment: " jcreed-completion '(lambda (x) t) t)))
    (if (equal env "easyrule") (jcreed-insert-easy-template)
      (if (assoc env jcreed-math)
	  (insert-string "$$\n"))
      (insert-string (concat "\\begin{" env "}\n"))
      (if (assoc env jcreed-math)
	  (insert-string "\\[\n\\justifies\n\\]\n\\justifies\n"))
      (let ((pm (point-marker)))
	(insert-string (concat "\n\\end{" env "}\n"))
	(if (assoc env jcreed-proof)
	    (insert-string "\n\\begin{proof}\n\n\\cqed\n\\end{proof}\n"))
	(if (assoc env jcreed-math)
	    (insert-string "$$\n"))
	(goto-char pm)))
    (recenter)))

(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (define-key emacs-lisp-mode-map "\C-o" 'lisp-complete-symbol)
	     ))

(add-hook 'lisp-interaction-mode-hook
	  '(lambda ()
	     (define-key lisp-interaction-mode-map
	       "\C-o"
	       'lisp-complete-symbol)))

(add-hook 'latex-mode-hook
                  '(lambda ()
                         (define-key tex-mode-map
                           "\C-cz"
                           'jcreed-insert-other)))

(define-key global-map "\M-=" 'backward-up-list)

(define-key global-map "\M-," 'pop-tag-mark)
(define-key global-map "\M-." 'jcreed-find-tag)
(define-key global-map "\M-\C-g" 'jcreed-deactivate-mark)

(defun jcreed-deactivate-mark () (interactive) (deactivate-mark))

(defun jcreed-find-tag (b e)
  (interactive "r")
  (if mark-active (progn
;		    (deactivate-mark)
		    (find-tag (buffer-substring-no-properties b e)))
    (find-tag (find-tag-default))))

(setq tex-dvi-view-command "xdvi.bin")

(setq tex-dvi-view-args '("-s" "5" "-geometry" "1024x600+0+600"))

(defun jcreed-tex-bibtex-file ()
  "Run BibTeX on the current buffer's file."
  (interactive)
  (if (tex-shell-running)
      (tex-kill-job)
    (tex-start-shell))
  (let* (shell-dirtrack-verbose
         (source-file (tex-main-file))
	 (x (message (expand-file-name source-file)))
         (tex-out-file
          (tex-append (file-name-nondirectory source-file) ""))
         (file-dir (file-name-directory (expand-file-name source-file))))
    (tex-send-command tex-shell-cd-command file-dir)
    (tex-send-command tex-bibtex-command tex-out-file))
  (tex-display-shell))

(defun jcreed-tex-view ()
 "Preview the last `.dvi' file made by running TeX under Emacs.
This means, made using \\[tex-region], \\[tex-buffer] or \\[tex-file].
The variable `tex-dvi-view-command' specifies the shell command for preview."
  (interactive)
  (let ((view-file-name-dvi (tex-append tex-print-file ".dvi"))
	test-name)
    (if (and (not (equal (current-buffer) tex-last-buffer-texed))
	     (file-newer-than-file-p
	      (setq test-name (tex-append (buffer-file-name) ".dvi"))
	      view-file-name-dvi))
	(setq view-file-name-dvi test-name))
    (if (not (file-exists-p view-file-name-dvi))
        (error "No appropriate `.dvi' file could be found")
      (progn
;       (debug)
	(apply 'start-process (append '("xdvi" "*xdvi*") (cons tex-dvi-view-command nil)
		        tex-dvi-view-args (cons view-file-name-dvi nil)))))))

(defvar jcreed-tex-main-buffer nil
"Set jcreed-tex-main-buffer to be something to always tex that rather than the current buffer")

(defun jcreed-set-main-buffer ()
  (interactive) (setq jcreed-tex-main-buffer (current-buffer)))

(defun jcreed-clear-main-buffer ()
  (interactive) (setq jcreed-tex-main-buffer nil))

(defun jcreed-tex-file ()
  (interactive)
  (when jcreed-tex-main-buffer
    (set-buffer jcreed-tex-main-buffer))
  (tex-file)
  (jcreed-tex-signal))

(defun jcreed-tex-signal ()
  (interactive)
  (save-excursion
    (let* ((xdvi-proc (get-process "xdvi")))
      (when xdvi-proc
	(let* ((tex-proc (tex-shell-proc))
	       (buf (process-buffer tex-proc))
	       (string
		(concat "kill -USR1 " (number-to-string (process-id xdvi-proc)))))
	  ;; Switch to buffer before checking for subproc output in it.
	  (set-buffer buf)
	  (goto-char (process-mark tex-proc))
	  (insert string)
	  (comint-send-input))))))

(add-hook 'latex-mode-hook
	  '(lambda ()
	     (define-key tex-mode-map "\C-c\C-v" 'jcreed-tex-view)
	     (define-key tex-mode-map "\C-c\C-d" 'jcreed-tex-bibtex-file)
	     (define-key tex-mode-map "\C-c\C-f" 'jcreed-tex-file)
	     (define-key tex-mode-map "\C-cf" 'jcreed-tex-signal)))


(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                    interpreter-mode-alist))


(setq file-coding-system-alist
(cons '(".*\\.eo" . iso-8859-3) file-coding-system-alist))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(define-key global-map "\C-z" 'call-last-kbd-macro)
(define-key global-map "\M-g" 'goto-line) ; how do people live without this?
(define-key global-map [(control tab)] 'other-window)

(add-hook 'sml-mode-hook
	  '(lambda ()
	     (setq sml-compile-command "CM.make \"sources.cm\"")
	     (setq sml-compile-commands-alist '(("CM.make \"sources.cm\"" . "sources.cm")))))


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun jcreed-save-whitespace ()
  (interactive)
  (remove-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq write-file-functions nil)
  (setq require-final-newline nil))

(defun jcreed-no-save-whitespace ()
  (interactive)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq require-final-newline t))

(defun jcreed-postprocess-path (path)
  (cond ((string-match "/Users/jreed/tiros-server/\\(.*\\)" path)
         (concat "tiros//" (match-string 1 path)))
        ((string-match "/Users/jreed/.cabal/share/x86_64-osx-ghc-7.10.3/Agda-2.6.0/lib/\\(.*\\)" path)
         (concat "agdalib//" (match-string 1 path)))
        ((string-match "/Users/jreed/.cabal/sandboxes/agda-build/agda/\\(.*\\)" path)
         (concat "agda//" (match-string 1 path)))
        (t
         path)))

(defun jcreed-copy-path ()
  "copy buffer's full path to kill ring, but with some
    postprocessing that works well with
    jcreed-open-file-at-point"
  (interactive)
  (when buffer-file-name
    (kill-new (jcreed-postprocess-path (file-truename buffer-file-name)))))
(define-key global-map "\M-p" 'jcreed-copy-path)

(defun nano-data ()
  (set-buffer (find-file-noselect "wordcount-history"))
  (goto-char (point-max))
  (let* ((tm (current-time))
	 (str1 (int-to-string (car tm)))
	 (str2 (int-to-string (cadr tm)))
	 (shellcmd (concat "wc -w 2005-*.tex | tail -1 | perl -lane 'print ((" str1 " * 65536 +  " str2 ") . \" $F[0]\" )' ")))
    (insert (shell-command-to-string shellcmd)))
    (basic-save-buffer))

(define-minor-mode nanowri-mode
  "just an after-save-hook hack for now"
  nil
  " NaNoWriMo"
  nil
  (if nanowri-mode
    (add-hook 'after-save-hook 'nano-data nil t)
    (remove-hook 'after-save-hook 'nano-data)))

(defun sd-mousewheel-scroll-up (event)
  "Scroll window under mouse up by two lines."
  (interactive "e")
  (let ((current-window (selected-window)))
    (unwind-protect
        (progn
          (select-window (posn-window (event-start event)))
          (scroll-up 2))
      (select-window current-window))))

(defun sd-mousewheel-scroll-down (event)
  "Scroll window under mouse down by two lines."
  (interactive "e")
  (let ((current-window (selected-window)))
    (unwind-protect
        (progn
          (select-window (posn-window (event-start event)))
          (scroll-down 2))
      (select-window current-window))))

(global-set-key (kbd "<mouse-5>") 'sd-mousewheel-scroll-up)
(global-set-key (kbd "<mouse-4>") 'sd-mousewheel-scroll-down)

(defun match-paren (arg)
  "Go to the matching paren if on a paren."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
        ((looking-back "\\s\)" (1- (point-marker))) (backward-list 1))
        ((eq major-mode 'ruby-mode) (goto-matching-ruby-block))))

(global-set-key "\M-)" 'match-paren)

(menu-bar-mode -1)
(when (boundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (and (boundp 'tool-bar-mode) (functionp 'tool-bar-mode)) (tool-bar-mode -1))

(setq visible-bell t)
(defun my-bell-function ()
  (unless (memq this-command
		'(isearch-abort abort-recursive-edit exit-minibuffer
				keyboard-quit mwheel-scroll down up next-line previous-line
				backward-char forward-char))
    (ding)))

(setq ring-bell-function 'my-bell-function)

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;(require 'browse-kill-ring)
;(browse-kill-ring-default-keybindings)

(set-time-zone-rule "EST")

; (load "/home/jcreed/.site-lisp/paraphrase-mode.el")
; (add-to-list 'auto-mode-alist '("\\.pp$" . latex-paraphrase-mode))


(setq line-move-visual nil)

;(setq-default indent-tabs-mode nil)


(autoload 'paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

(defun paredit () (interactive) (enable-paredit-mode))

(defun jcreed-kill-sexp-tail ()
  (interactive)
  (let ((begin (point))
        (end 0))
    (save-excursion
      (paredit-forward-up)
      (backward-char)
      (setq end (point)))
    (kill-region begin end)))

(add-hook 'paredit-mode-hook
	  '(lambda ()
	     (define-key paredit-mode-map (kbd "M-)") 'match-paren)
	     (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
             (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
             (define-key paredit-mode-map (kbd "M-r") 'revert-buffer)
             (define-key paredit-mode-map (kbd "M-R") 'paredit-raise-sexp)
             (define-key paredit-mode-map (kbd "M-k") 'jcreed-kill-sexp-tail)))

(add-hook 'comint-mode-hook
 	  '(lambda ()
 	     (define-key comint-mode-map
 	       [mouse-2]
 	       'mouse-yank-primary)))

; (setq mouse-yank-at-point t)

(autoload 'rainbow-mode "rainbow-mode" "Colorizes stuff." t)
(autoload 'forth-mode "gforth" "Colorizes stuff." t)

;(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
;(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
;(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

(add-to-list 'auto-mode-alist '("\\.se$" . emacs-lisp-mode))
(add-hook 'emacs-lisp-mode-hook '(lambda () (paredit-mode)))
(add-hook 'clojure-mode-hook '(lambda () (paredit-mode)))
(define-key global-map "\C-x;" 'comment-region)
(define-key global-map (kbd "C-S-k") 'kill-sexp)
(define-key global-map (kbd "C-k") 'kill-line)

(setq term-term-name "vt100")

(autoload 'rust-mode "rust-mode" "Start rust-mode" t)
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))


(defun eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its value"
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(ifat chef
      (require 'whitespace)
      (setq whitespace-style '(face empty tabs lines-tail trailing))
      (setq-default indent-tabs-mode nil))

(setq c-basic-offset 2)

(ifat chef
      (remove-hook 'find-file-hooks 'vc-find-file-hook) ; perf win
      (setq vc-handled-backends nil)

;      (add-to-list 'load-path "/home/jcreed/.site-lisp/expand-region.el")
;      (require 'expand-region)
;      (global-set-key (kbd "C-=") 'er/expand-region)
      )


(defun jcreed-inc (start end)
  (interactive "r")
  (let ((n (string-to-number (buffer-substring start end))))
    (delete-region start end)
    (insert (number-to-string (+ n 1)))))

(defun jcreed-date ()
  (interactive)
  (insert (format-time-string "=== %Y.%m.%d\n\n")))



(add-to-list 'load-path "/home/jcreed/.site-lisp/sml-mode-4.0")
(autoload 'sml-mode "sml-mode" "Sml editing mode." t)

(add-to-list 'load-path "/home/jcreed/.site-lisp/lua-mode")
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


(remove-hook 'find-file-hooks 'vc-find-file-hook)

(defun jcreed-qna-q ()
   (interactive)
   (insert "Q: \nA: ???\n")
   (backward-char 8))

(defun jcreed-qna-a ()
   (interactive)
   (insert "Q: \nA: "))

(define-key global-map "\C-c=" 'jcreed-date)
(ifat chef
      (define-key global-map "\C-cc" 'hs-toggle-hiding)
      (define-key global-map "\C-cH" 'hs-hide-all)
      (define-key global-map "\C-cS" 'hs-show-all))

(define-key global-map "\C-cq" '(lambda () (interactive) (jcreed-qna-q)))
(define-key global-map "\C-ca" '(lambda () (interactive) (jcreed-qna-a)))
(define-key global-map "\C-c/" 'jcreed-browse-thing-at-point)
(define-key global-map "\C-c\C-f" 'jcreed-open-file-at-point)
(define-key global-map "\M-," 'pop-tag-mark)
;(define-key global-map "\C-cg" 'tbgs)
;(define-key global-map "\C-c\C-c" 'jcreed-class-to-path)

(add-hook 'cperl-mode-hook
          (lambda ()
            (define-key cperl-mode-map "\t" 'indent-for-tab-command)))

(setq display-time-day-and-date t
      display-time-default-load-average nil
      display-time-format "%A %b %e %k:%M")

(display-time)

(ifat baez
 (setq browse-url-browser-function 'browse-url-generic
       browse-url-generic-program "google-chrome"))

(defun jcreed-browse-thing-at-point (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (cond ((equal face 'jcreed-person-face)
           (browse-url (concat "redacted" (thing-at-point 'word))))
          ((equal face 'jcreed-diff-face)
           (browse-url (concat "redacted" (thing-at-point 'word))))
          ((equal face 'jcreed-task-face)
           (browse-url (concat "redacted" (task-at-point))))
          ((equal face 'jcreed-paste-face)
           (browse-url (concat "redacted" (thing-at-point 'word))))
          ((equal face 'jcreed-paper-face)
           (browse-url (cadr (assoc (thing-at-point 'word) notes-data))))
          ((equal face 'jcreed-path-face)
           (let ((thing (thing-at-point 'filename)))
             (when (string-match "\\(.*\\)//\\(.*\\)" thing)
               (let ((repo (match-string 1 thing))
                     (path (match-string 2 thing)))
                 (cond
                  ((equal repo "occ")
                   (let ((lib-string
                          (replace-regexp-in-string "\\([^/]+/\\).*\\'" "\\1blob/master/" path nil nil 1)))
                     (browse-url (concat "http://github.com/chef/" lib-string))
                     ))
                  ((equal repo "agdac")
                   (browse-url (concat "https://github.com/agda/agda/commit/" path)))
                  ((equal repo "agda")
                   (browse-url (concat "https://github.com/agda/agda/blob/master/" path)))
                  ((equal repo "gh")
                   (browse-url (concat "http://github.com/" path)))
                  )))))
          (t (browse-url-at-point)))))

(defun task-at-point ()
  (let ((word (thing-at-point 'word)))
    (if (string-match "\\([0-9]+\\)" word)
        (match-string 1 word)
      "")))

(defun jcreed-find-file-other-window (filename)
  (let ((value (find-file-noselect filename))
        (pop-up-windows t))
    (pop-to-buffer value '(display-buffer-use-some-window
                           . ((inhibit-same-window . t))))))

(defun jcreed-open-file-at-point (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (cond ((equal face 'jcreed-path-face)
           (let ((thing (thing-at-point 'filename)))
             (when (string-match "\\(.*\\)//\\(.*\\)" thing)
               (let ((repo (match-string 1 thing))
                     (path (match-string 2 thing)))
                 (message (concat path " - " repo))
                 (cond
                  ((equal repo "tiros")
                   (jcreed-find-file-other-window (concat "/Users/jreed/tiros-server/" path)))
                  ((equal repo "occ")
                   (jcreed-find-file-other-window (concat "/Users/jreed/occ/" path)))
                  ((equal repo "agda")
                   (jcreed-find-file-other-window (concat "/Users/jreed/.cabal/sandboxes/agda-build/agda/" path)))
                  ((equal repo "agdalib")
                   (jcreed-find-file-other-window (concat "/Users/jreed/.cabal/share/x86_64-osx-ghc-7.10.3/Agda-2.6.0/lib/" path)))
                  ((equal repo "home")
                   (jcreed-find-file-other-window (concat "/Users/jreed/" path)))
                  )))))
          (t (jcreed-browse-thing-at-point)))))

(defun jcreed-thing-at-point (pos)
  (interactive "d")
  (message (thing-at-point 'filename)))

(defun plaintext (b e)
  (interactive "r")
  (set-text-properties b e nil))

; (setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))

(define-key global-map "\M-i" '(lambda () (interactive) (switch-to-buffer "IDEAS")))
(define-key global-map "\C-c\M-%" 'query-replace-regexp)
(define-key global-map "\M-r" 'revert-buffer)

(setenv "NODE_NO_READLINE" "1")
(ifat chef
      (setenv "PATH" (concat (getenv "PATH") ":/Users/jcreed/Library/Haskell/bin:/usr/local/bin:/Users/jcreed/bin")))

(define-derived-mode notes-mode fundamental-mode
  (setq font-lock-defaults '(notes-mode-highlights))
  (setq-local notes-data nil)
  (notes-reload-data)
  (define-key notes-mode-map "\C-c\C-r" 'notes-reload-data)
  (setq mode-name "Notes"))

(setq auto-mode-alist (cons '("/\\(IDEAS\\|NOTES\\|TODO\\|JOURNAL\\)$" . notes-mode) auto-mode-alist))

(defun notes-reload-data ()
  (interactive)
  (let ((data-file "DATA.el"))
    (when (file-exists-p data-file)
        (setq notes-data
              (with-temp-buffer
                (with-current-buffer (find-file-noselect "DATA.el")
                  (goto-char (point-min))
                  (read (current-buffer)))))
        (message "Loaded notes data."))))


(defun jcreed-find-paper-name (lim)
  (catch 'jcreed-find-paper-name-ret
    (while t
      (let* ((succ (re-search-forward "\\[\\([a-zA-Z0-9]+?\\)\\]" lim t))
             (_ (when (not succ) (throw 'jcreed-find-paper-name-ret nil)))
             (data (match-data))
             (good (assoc (match-string 1) notes-data)))
        (when good
          (set-match-data data)
          (throw 'jcreed-find-paper-name-ret t))))))


(setq notes-mode-highlights
		'((jcreed-find-paper-name . 'jcreed-paper-face)
        ("^=== .*\n" . 'jcreed-header-face)
		  ("^---\n" . 'jcreed-minor-header-face)
		  ("^#\\(?:\\w\\|-\\)+" . 'font-lock-type-face)
		  ("\\s-#\\w+" . 'font-lock-type-face)
		  ("^Q:" . 'jcreed-question-face)
		  ("^TODO:" . 'jcreed-question-face)
		  ("^DONE:" . 'jcreed-answer-face)
		  ("^A:" . 'jcreed-answer-face)
		  ("^\\$ .*" . 'jcreed-shell-face)
		  ("^\\$\\( +[-a-z./]+ *\\)"  1 'jcreed-command-face t)
		  ("<<<\n" . 'jcreed-shell-face)
		  (">>>\n" . 'jcreed-shell-face)
		  ("\\([a-z]+\\)@[^a-z]" 1 'jcreed-person-face t)
		  ("https?://[^[:space:]\n]+" . 'link)
		  ("\\bD[0-9]+\\b" . 'jcreed-diff-face)
		  ("\\bT[0-9]+\\b" . 'jcreed-task-face)
		  ("\\bP[0-9]+\\b" . 'jcreed-paste-face)
		  ("\\b[a-z]+//\\(?:\\w\\|[-_/.]\\)+" . 'jcreed-path-face)
		  ("\\?\\?\\?" . 'jcreed-bad-face)))

; XXX split off into separate file
(defun journal-hook ()
  (when (and (stringp buffer-file-name)
             (equal "journal.txt" (file-name-nondirectory buffer-file-name)))
    (setq font-lock-keywords
          '((";\\(Checking\\);" 1 'jcreed-question-face t)
				(";\\(ChaseChecking\\);" 1 'jcreed-question-face t)
				(";\\(Capone\\);" 1 'jcreed-answer-face t)
				(";\\(Ccard\\);" 1 'jcreed-person-face t)
				(";\\(.*401k\\);" 1 'jcreed-shell-face t)
				("^\\([0-9-]+\\);;\\(\$?[0-9.]+\\)" 2 'jcreed-command-face t)
				(";\\(PayPal\\);" 1 'jcreed-shell-face t)
				("\\?" . 'jcreed-bad-face)))))

(add-hook 'find-file-hook 'journal-hook)

;;; url encode and decode regions

(defun func-region (start end func)
  "run a function over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))
(defun hex-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))
(defun unhex-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))


(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))

;;; make backup files in a single place, not polluting various directories

(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)


(ifat chef
      ;; XXX should change this if I ever work on code that actually cares
      ;; about tiny screens again.
      (setq highlight-80+-columns 80))




(setq mode-line-position (assq-delete-all 'wc-mode mode-line-position))
(setq mode-line-position
      (append
       mode-line-position
       '((wc-mode
	  (6 (:eval (if (use-region-p)
			(format " [ %d words ]"
				(count-words-region (point) (mark)))
		      (format " [ %d words ]"
			      (count-words-region (point-min) (point-max))))))
	  nil))))


(define-minor-mode wc-mode
  "Toggle word-count mode.
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

When enabled, the total number of characters, words, and lines is
displayed in the mode-line.")


(defun jcreed-recolor-fast ()
  (interactive)
  (kill-all-local-variables)
  (global-font-lock-mode-enable-in-buffers)
  (run-hooks 'find-file-hook))

(defun jcreed-recolor ()
  (interactive)

  (global-font-lock-mode-cmhh)

 (setq mode-name "Fundamental")
 (setq major-mode 'fundamental-mode)
;  (pp change-major-mode-hook)
;				  (run-hooks 'change-mode-major-hook)

  ;(font-lock-change-mode)
  (global-font-lock-mode-cmhh)
  (global-font-lock-mode-enable-in-buffers)
  (run-hooks 'find-file-hook)
  (font-lock-fontify-buffer))

;(global-set-key (kbd "M-r") 'jcreed-recolor)

(setq verilog-auto-newline nil)
(setq verilog-auto-indent-on-newline nil)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(require 'package)
(package-initialize)

(ifat chef
      (add-to-list 'auto-mode-alist '("\\.js" . js-mode))
      (add-to-list 'auto-mode-alist '("\\.erl" . erlang-mode)))


(setq exec-path (append exec-path '("/usr/local/bin")))

(add-hook 'after-init-hook
          (lambda ()
            (setq rainbow-delimiters-max-face-count 4)
            ;(require 'button-lock)
            ;(global-button-lock-mode 1)
            ;; (button-lock-register-global-button
            ;;  "https?://[^[:space:]\n]+"
            ;;  'browse-url-at-mouse
            ;;  :face 'link :face-policy 'prepend)
            ))

(setq paragraph-start "[A-Z]+:\\|\f\\|[ \t]*$")
(setq paragraph-separate "\\$\\|[a-z]+//\\|https?:\\|[A-Z]+:$\\|: \\|<<<$\\|>>>$\\|[ \t\f]*$")
(setq sentence-end-double-space nil)

(define-key global-map "\M-q" 'jcreed-fill-paragraph)
(defun jcreed-fill-paragraph ()
  (interactive)
  (let ((case-fold-search nil))
    (fill-paragraph)))

(defun jcreed-sort-buffers-by-file ()
  (interactive)
  (Buffer-menu-sort 6))

(add-hook 'Buffer-menu-mode-hook
          (lambda ()
;            (jcreed-sort-buffers-by-file)
            (define-key Buffer-menu-mode-map (kbd "M-f") 'jcreed-sort-buffers-by-file)))


(ifat chef
      (define-key global-map (kbd "M-`") 'other-frame))

(require 'dired)
(global-set-key (kbd "C-x C-j") #'dired-jump)

(autoload 'coffee-mode "coffee-mode" "Coffeescript editing mode." t)

;; web-mode config

(ifat baez (require 'web-mode))

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
   ad-do-it))

(setq web-mode-content-types-alist
		'(("jsx" . "\\.js[x]?\\'")
		  ("jsx" . "\\.ts[x]?\\'")))
;;;;;;;;;;;


(defun jcreed-setup-indent (n)
  ;; web development
  (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset n)    ; web-mode, css in html file
  (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq css-indent-offset n)
  (setq sml-indent-level n)
  (setq js-indent-level n))

(jcreed-setup-indent 2)

(ifat chef
      (add-hook 'before-save-hook #'gofmt-before-save))

(defun jcreed-uncamel (b e)
  (interactive "r")
  (replace-regexp "\\([A-Z]\\)" " \\1" nil b e)
  ;; This is not correct; should be a larger region because of the
  ;; spaces inserted
  (downcase-region b e)
  (goto-char b)
  (delete-char 1))

(global-set-key [(control shift tab)] (lambda () (interactive) (other-window -1)))

(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
            (catch 'non-ascii
              (while (not (eobp))
                (or (eq (char-charset (following-char))
                        'ascii)
                    (throw 'non-ascii (point)))
                (forward-char 1)))))
    (if point
        (goto-char point)
        (message "No non-ascii characters."))))

(ifat chef
      (setq twelf-root "/Applications/Twelf/")
      (load (concat twelf-root "emacs/twelf-init.el")))

(setq default-process-coding-system '(utf-8 . utf-8))
(define-key global-map (kbd "RET") 'electric-newline-and-maybe-indent)


(add-to-list (quote auto-mode-alist) (quote ("\\.scala\\'" . scala-mode)))

(ifat baez
      (require 'unicode-fonts)
      (unicode-fonts-setup))


(add-hook 'agda2-mode-hook
          (lambda ()
            (jcreed-add-agda-keys)
            (define-key agda2-mode-map "\M-," 'agda2-go-back)))


(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
									 py-indent-offset 2
                            tab-width 2))))

(defun jcreed-thingy ()

  (agda-input-setup))

(setq jcreed-add-agda-keys-called nil)
(defun jcreed-add-agda-keys ()
  (when (not jcreed-add-agda-keys-called)
    (require 'agda-input)
    (with-temp-buffer
      (activate-input-method "Agda") ;; the input method has to be triggered for `quail-package-alist' to be non-nil
      (let ((quail-current-package (assoc "Agda" quail-package-alist)))
        (quail-define-rules ((append . t))
                            ("\\esh" ?ʃ)
                            ("\\prov" ?⊢)
                            ("\\adj" ?⊣)
                            ("\\prequiv" ["⊣⊢"]))))
    (setq jcreed-add-agda-keys-called t)))

(ifat chef

      (setq agda-path "/Users/jreed/.cabal/sandboxes/agda-build/agda/dist/build/")
      (load-file (let ((coding-system-for-read 'utf-8))
                   (shell-command-to-string (concat agda-path "agda-mode/agda-mode locate"))))

      ;; (setq agda2-include-dirs '("."  "/Users/jreed/.agda/HoTT-Agda/core"))
      (setq agda2-program-name (concat agda-path "Agda/agda"))
      )

(ifat baez
      (setq agda2-program-name "/home/jcreed/Idris/.cabal-sandbox/bin/agda")
      (load-file (let ((coding-system-for-read 'utf-8))
                   ;; (shell-command-to-string "/home/jcreed/.cabal/sandbox/.cabal-sandbox/bin/agda-mode locate")
                   (shell-command-to-string "/home/jcreed/Idris/.cabal-sandbox/bin/agda-mode locate")
                   )))

(ifat chef
      (add-hook 'notes-mode-hook
                (lambda ()
                  (jcreed-add-agda-keys)
                  (set-input-method "Agda")))
      (add-hook 'latex-mode-hook
                '(lambda ()
                   (setq tex-command "/usr/local/texlive/2017/bin/x86_64-darwin/pdflatex"))))
