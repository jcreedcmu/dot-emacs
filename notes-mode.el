;; Notes mode

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
(defface jcreed-path2-face nil "Jcreed Path2 Face")
(defface jcreed-paper-face nil "Jcreed Paper Face")
(defface jcreed-meta-face nil "Jcreed Meta Face")

(set-face-attribute 'jcreed-answer-face nil  :foreground "#268bd2")
(set-face-attribute 'jcreed-bad-face nil  :foreground "yellow" :background "#dc322f")
(set-face-attribute 'jcreed-command-face nil  :foreground "gray20" :weight 'bold)
(set-face-attribute 'jcreed-header-face nil  :background "#586e75" :foreground "#fdf6e3")
(set-face-attribute 'jcreed-minor-header-face nil  :background "#8ac" :foreground "#fdf6e3")
(set-face-attribute 'jcreed-paper-face nil  :background "#77cc77" :foreground "black")
(set-face-attribute 'jcreed-paste-face nil  :foreground "#268bd2" :weight 'bold)
(set-face-attribute 'jcreed-path-face nil  :foreground "#d33682" :weight 'bold)
(set-face-attribute 'jcreed-path2-face nil  :foreground "#d33682" :weight 'bold)
(set-face-attribute 'jcreed-person-face nil  :foreground "#6c71c4" :weight 'bold)
(set-face-attribute 'jcreed-question-face nil  :foreground "#dc322f")
(set-face-attribute 'jcreed-shell-face nil  :foreground "#586e75" :background "#eee8d5")
(set-face-attribute 'jcreed-task-face nil  :foreground "#2aa198" :weight 'bold)
(set-face-attribute 'jcreed-meta-face nil  :background "#99cc55" :foreground "#337733")

(define-derived-mode notes-mode fundamental-mode
  (setq font-lock-defaults '(notes-mode-highlights))
  (setq-local notes-data nil)
  (notes-reload-data)
  (define-key notes-mode-map "\C-c\C-r" 'notes-reload-data)
  (setq mode-name "Notes"))

(setq auto-mode-alist (cons '("/\\(IDEAS\\|NOTES\\|TODO\\|JOURNAL\\|RECIPE\\)$" . notes-mode) auto-mode-alist))

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
		  ("`.*?`" . 'jcreed-shell-face)
		  ("\\([a-z]+\\)@[^a-z]" 1 'jcreed-person-face t)
		  ("https?://[^[:space:]\n]+" . 'link)
		  ("\\bD[0-9]+\\b" . 'jcreed-diff-face)
		  ("\\bT[0-9]+\\b" . 'jcreed-task-face)
		  ("\\bP[0-9]+\\b" . 'jcreed-paste-face)
		  ("\\b[a-z]+//\\(?:\\w\\|[-_/.]\\)+" . 'jcreed-path-face)
		  ("\\b[a-z]+:\\[\\(?:[^]]\\)+\\]" . 'jcreed-path2-face)
		  ("\\?\\?\\?" . 'jcreed-bad-face)))

(setq auto-mode-alist (cons '("/\\(journal.txt\\)$" . journal-mode) auto-mode-alist))

;; A convenient keybinding to open my notes file.

(defun jcreed-open-ideas-file (prefix-arg)
  "Open notes file, or if prefix arg is given, open emacs config"
  (interactive "p")
  (cond
	((eq prefix-arg 16)
	 (switch-to-buffer (find-file-noselect jcreed-local-post-file-name)))
	((eq prefix-arg 4)
	 (switch-to-buffer (find-file-noselect jcreed-emacs-config-file-name)))
	(t
	 (switch-to-buffer (find-file-noselect jcreed-ideas-file-name)))))

(define-key global-map (kbd "M-i") #'jcreed-open-ideas-file)

;; Here are some functions I use to get the current path of a buffer I'm
;; visiting, subject to some postprocessing. Right now I just redefine
;; =jcreed-postprocess-path= in a local =.el= if I need to, but it would
;; be nicer to define some proper data that describe the required
;; transformations.

;; A typical use of =jcreed-postprocess-path= is just to "compress" a
;; path from a known directory into a repo path like
;; =repo:[some/path/within/that/repo]=. (Or, well, it appears here that I was
;; using an old style repo path like =repo//some/path/within/that/repo=)

(defun jcreed-postprocess-path (path)
  (cond ((string-match "/Users/jreed/.cabal/share/x86_64-osx-ghc-7.10.3/Agda-2.6.0/lib/\\(.*\\)" path)
			(concat "agdalib//" (match-string 1 path)))
		  ((string-match "/Users/jreed/.cabal/sandboxes/agda-build/agda/\\(.*\\)" path)
			(concat "agda//" (match-string 1 path)))
		  (t
			path)))

(defun jcreed-yank-postprocessed ()
  (interactive)
  (insert (jcreed-postprocess-path (current-kill 0))))
(define-key global-map (kbd "C-S-y") #'jcreed-yank-postprocessed)

(defun jcreed-copy-path (prefix-arg)
  "copy buffer's full path to kill ring, but with some
    postprocessing that works well with
    jcreed-open-file-at-point"
  (interactive "p")
  (when buffer-file-name
    (let* ((path (file-truename buffer-file-name))
           (ppath (jcreed-postprocess-path path))
           (lpath (jcreed-postprocess-path (format "%s:%d" path (line-number-at-pos))))
           (idpath (concat lpath " " (thing-at-point 'symbol))))
      (cond
       ((eq prefix-arg 0) (kill-new path))
       ((eq prefix-arg 1) (kill-new ppath))
       ((eq prefix-arg 4) (kill-new lpath))
       ((eq prefix-arg 16) (kill-new idpath))
       ((eq prefix-arg 9)
        (with-current-buffer (find-buffer-visiting jcreed-ideas-file-name)
          (insert (concat idpath "\n"))
          (cl-dolist (window (get-buffer-window-list nil nil t))
            (set-window-point window (point)))))))))

(define-key global-map "\M-p" 'jcreed-copy-path)

;; Questions and Answers

(defun jcreed-qna-q ()
  (interactive)
  (insert "Q: \nA: ???\n")
  (backward-char 8))

(defun jcreed-qna-a ()
  (interactive)
  (insert "Q: \nA: "))

;; Keybindings

;; Define keybindings for notes mode.
;; XXX These are =global-map= but should be =notes-mode-map=.

(define-key global-map "\C-c=" 'jcreed-date)
(ifat chef
		(define-key global-map "\C-cc" 'hs-toggle-hiding)
		(define-key global-map "\C-cH" 'hs-hide-all)
		(define-key global-map "\C-cS" 'hs-show-all))

(define-key global-map "\C-cq" '(lambda () (interactive) (jcreed-qna-q)))
(define-key global-map "\C-ca" '(lambda () (interactive) (jcreed-qna-a)))
(define-key global-map "\C-c/" 'jcreed-browse-thing-at-point)
(define-key global-map "\C-c\C-f" 'jcreed-open-file-at-point)

;; XXX this is not really notes related, should be elsewhere?
(define-key global-map "\M-," 'pop-tag-mark)

;;(define-key global-map "\C-cg" 'tbgs)
;;(define-key global-map "\C-c\C-c" 'jcreed-class-to-path)

;; Navigation

(defun jcreed-browse-repo-path (repo path)
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
	))

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
					  (jcreed-browse-repo-path repo path)))))
			 (t (browse-url-at-point)))))

(defun jcreed-open-repo-path (repo path)
  (message (concat path " - " repo))
  (cond
	((equal repo "agda")
	 (jcreed-find-file-other-window (concat "/Users/jreed/.cabal/sandboxes/agda-build/agda/" path)))
	((equal repo "agdalib")
	 (jcreed-find-file-other-window (concat "/Users/jreed/.cabal/share/x86_64-osx-ghc-7.10.3/Agda-2.6.0/lib/" path)))
	((equal repo "home")
	 (jcreed-find-file-other-window (concat "/Users/jreed/" path)))
	))

(defun task-at-point ()
  (let ((word (thing-at-point 'word)))
	 (if (string-match "\\([0-9]+\\)" word)
		  (match-string 1 word)
		"")))

(defun jcreed-open-file-at-point (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
						(get-char-property (point) 'face))))
	 (cond ((equal face 'jcreed-path-face)
			  (let ((thing (thing-at-point 'filename)))
				 (when (string-match "\\(.*\\)//\\(.*\\)" thing)
					(let ((repo (match-string 1 thing))
							(path (match-string 2 thing)))
					  (jcreed-open-repo-path repo path)))))
			 ((equal face 'jcreed-path2-face)
			  (let ((thing (face-bounded-thing-at-point (point))))
				 (when (string-match "\\(.*\\):\\[\\(.*\\)\\]" thing)
					(let ((repo (match-string 1 thing))
							(path (match-string 2 thing)))
					  (jcreed-open-repo-path repo path)))))
			 (t (jcreed-browse-thing-at-point)))))

(defun face-bounded-thing-at-point (pos)
  (message "hi")
  (buffer-substring-no-properties
	(or (previous-single-property-change pos 'face) (point-min))
	(or (next-single-property-change pos 'face) (point-max))))

(defun jcreed-thing-at-point (pos)
  (interactive "d")
  (message (thing-at-point 'filename)))

;; Defining paragraphs
;; Useful for delimiting =fill-paragraph=.

(setq paragraph-start "[A-Z]+:\\|\f\\|[ \t]*$")
(setq paragraph-separate "\\$\\|[a-z]+//\\|https?:\\|[A-Z]+:$\\|: \\|<<<$\\|>>>$\\|[ \t\f]*$")
(setq sentence-end-double-space nil)

(provide 'notes-mode)
