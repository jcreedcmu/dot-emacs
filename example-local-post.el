(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(sml-mode . ("millet-ls" "--stdio"))))

(require 'company)
(require 'sml-mode)
(define-key sml-mode-map (kbd "M-;") 'company-complete)

(setq auto-mode-alist (cons '("\\.fun$" . sml-mode) auto-mode-alist))



(setq load-path
      (cons "/home/jcreed/pgit/twelf/emacs" load-path))

;; Autoload libraries when Twelf-related major modes are started.
(autoload 'twelf-mode "twelf" "Major mode for editing Twelf source." t)
(autoload 'twelf-server "twelf" "Run an inferior Twelf server." t)
(autoload 'twelf-sml "twelf" "Run an inferior Twelf-SML process." t)

;; Switch buffers to Twelf mode based on filename extension,
;; which is one of .elf, .quy, .thm, or .cfg.
(setq auto-mode-alist
		(cons '("\\.elf$" . twelf-mode)
				(cons '("\\.quy$" . twelf-mode)
						(cons '("\\.thm$" . twelf-mode)
								(cons '("\\.cfg$" . twelf-mode)
										auto-mode-alist)))))

(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
            (lambda ()
              (when (locate-dominating-file "." ".clang-format")
                (clang-format-buffer))
              ;; Continue to save.
              nil)
            nil
            ;; Buffer local hook.
            t))

;; Run this for each mode you want to use the hook.
(add-hook 'c-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))

;;; old local-post.el is what follows:

(setq jcreed-browse-target-file-alist
		'(("CONWORLD" . "/home/jcreed/self/CONWORLD")
		  ("IDEAS" . "/home/jcreed/self/IDEAS")
		  ("NOTES" . "/home/jcreed/self/NOTES")
		  ("STORYMETA" . "/home/jcreed/self/STORYMETA")))

(defun jcreed-open-repo-path (repo path prefix-arg)
  (message (concat path " - " repo))
  (cond
	((equal repo "home")
	 (jcreed-find-file-prefix (concat "/home/jcreed/" path) prefix-arg))))

(define-key global-map (kbd "M-l") (lambda () (interactive) (kill-new (minibuffer-contents-no-properties))))


(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))

(setq-default indent-tabs-mode nil)

(defun jcreed-markdown-mode-hook ()
  (define-key markdown-mode-map (kbd "<M-right>") 'right-word)
  (define-key markdown-mode-map (kbd "<M-left>") 'left-word))
(add-hook 'markdown-mode-hook #'jcreed-markdown-mode-hook)

(define-key notes-mode-map (kbd "M-l") 'downcase-word)


(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))



(when nil
  (setq load-path (cons "/home/jcreed/pgit/lean4-mode" load-path))
  (require 'lean4-mode))


(defun jcreed-pov-mode-hook ()
  (define-key pov-mode-map (kbd "<tab>") 'indent-region)
  (define-key pov-mode-map (kbd "C-c C-c") (lambda () (interactive) (compile "make"))))
(add-hook 'pov-mode-hook #'jcreed-pov-mode-hook)

(defun jcreed-c++-mode-hook ()
  (define-key c++-mode-map (kbd "C-c C-c") (lambda () (interactive) (compile "make"))))
(add-hook 'c++-mode-hook #'jcreed-c++-mode-hook)

; (setq revert-without-query '("a.png"))

;; (define-key tide-mode-map (kbd "C-c C-c")
;;   (lambda () (interactive)
;;     (compile "cd .. && make svg")
;;     (with-current-buffer (find-file-noselect "/tmp/a.png")
;;       (revert-buffer t t t))))

;; don't make a bunch of lockfiles in the same directory, which would
;; confuse servers that check for changes in that directory and make them
;; do extra work.
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "/tmp/\\1" t)))

;; go to previous window
(define-key global-map (kbd "C-x O") (lambda () (interactive) (other-window -1)))

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(global-set-key (kbd "C-c o") 'switch-to-minibuffer)


; (use-package eglot)

(defvar fictional-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for fictional-mode.")


(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(fictional-mode . ("/home/jcreed/proj/twelf-lsp/twelf-lsp"))))

;; Define the mode
(define-derived-mode fictional-mode prog-mode "Fictional"
  "A major mode for testing purposes."
  )
(add-to-list 'auto-mode-alist '("\\.fiction\\'" . fictional-mode))



(defun jcreed-fictional-mode-hook ()
  (interactive)
  (eglot)
  (define-key fictional-mode-map (kbd "M-.") 'xref-find-definitions))

(add-hook 'fictional-mode-hook 'jcreed-fictional-mode-hook)


  (use-package rustic
    :ensure
    :bind (:map rustic-mode-map
                ("M-j" . lsp-ui-imenu)
                ("M-?" . lsp-find-references)
                ("C-c C-c l" . flycheck-list-errors)
                ("C-c C-c a" . lsp-execute-code-action)
                ("C-c C-c r" . lsp-rename)
                ("C-c C-c q" . lsp-workspace-restart)
                ("C-c C-c Q" . lsp-workspace-shutdown)
                ("C-c C-c s" . lsp-rust-analyzer-status))
    :config)

(add-hook 'rustic-mode-hook 'jcreed-rustic-mode-hook)
(defun jcreed-rustic-mode-hook ()
  ;; is this going to conflict with my other config? who knows
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-eldoc-render-all t)
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (define-key rustic-mode-map (kbd "M-.") 'lsp-find-definition)
  (define-key rustic-mode-map (kbd "M-;") 'company-complete)
  (define-key rustic-mode-map (kbd "C-c C-s") 'lsp-rename)
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t)
  ;; less flashiness
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil)

    ;; comment to disable rustfmt on save
;    (setq rustic-format-on-save t)
    )
