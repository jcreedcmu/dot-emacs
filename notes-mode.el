;; Notes mode
(require 'xref) ;; needed for xref-push-marker-stack
(require 'timezone) ;; needed for timezone-last-day-of-month

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
(defface jcreed-link-face nil "Jcreed Link Face")
(defface jcreed-strikethrough-face nil "Jcreed Strikethrough Face")

(set-face-attribute 'jcreed-answer-face nil  :foreground "#268bd2")
(set-face-attribute 'jcreed-bad-face nil  :foreground "yellow" :background "#dc322f")
(set-face-attribute 'jcreed-command-face nil  :foreground "gray20" :weight 'bold)
(set-face-attribute 'jcreed-header-face nil  :background "#586e75" :foreground "#fdf6e3" :extend t)
(set-face-attribute 'jcreed-minor-header-face nil  :background "#8ac" :foreground "#fdf6e3" :extend t)
(set-face-attribute 'jcreed-paper-face nil  :background "#77cc77" :foreground "black")
(set-face-attribute 'jcreed-paste-face nil  :foreground "#268bd2" :weight 'bold)
(set-face-attribute 'jcreed-path-face nil  :foreground "#d33682" :weight 'bold)
(set-face-attribute 'jcreed-path2-face nil  :foreground "#d33682" :weight 'bold)
(set-face-attribute 'jcreed-person-face nil  :foreground "#6c71c4" :weight 'bold)
(set-face-attribute 'jcreed-question-face nil  :foreground "#dc322f")
(set-face-attribute 'jcreed-shell-face nil  :foreground "#586e75" :background "#eee8d5" :extend t)
(set-face-attribute 'jcreed-task-face nil  :foreground "#2aa198" :weight 'bold)
(set-face-attribute 'jcreed-meta-face nil  :background "#99cc55" :foreground "#337733")
(set-face-attribute 'jcreed-link-face nil  :foreground "#26d" :background nil :weight 'bold)
(set-face-attribute 'jcreed-strikethrough-face nil  :strike-through t)

(when jcreed-dark-mode
  (set-face-attribute 'jcreed-shell-face nil  :foreground "#fdf6e3" :background "#001b26")
  (set-face-attribute 'jcreed-header-face nil  :foreground "#fdf6e3" :background "#004656")
  (set-face-attribute 'jcreed-minor-header-face nil  :foreground "#fdf6e3" :background "#204646")
  (set-face-attribute 'jcreed-command-face nil  :foreground "#586e75" :weight 'bold))

(defvar notes-show-metadata nil
  "Non-nil means show entry metadata")

(define-derived-mode notes-mode fundamental-mode
  (setq font-lock-defaults '(notes-mode-highlights t))
  (setq font-lock-extra-managed-props '(invisible display))
  (setq buffer-invisibility-spec '((jcreed-meta) t))
  (make-local-variable 'notes-show-metadata)
  (setq notes-show-metadata nil)
  (setq-local notes-data nil)
  (notes-reload-data)
  (define-key notes-mode-map (kbd "C-c C-r") 'notes-reload-data)
  (define-key notes-mode-map (kbd "C-c C-l") 'jcreed-make-link)
  (define-key notes-mode-map (kbd "C-c ;") 'notes-toggle-metadata)
  (define-key notes-mode-map (kbd "C-c =") 'jcreed-insert-date)
  (define-key notes-mode-map (kbd "C-c -") 'jcreed-insert-minor-separator)
  (define-key notes-mode-map (kbd "C-c C-m") 'jcreed-insert-meta)
  (define-key notes-mode-map (kbd "M-.") 'jcreed-browse-thing-at-point)
  (define-key notes-mode-map (kbd "C-c q") '(lambda () (interactive) (jcreed-qna-q)))
  (define-key notes-mode-map (kbd "C-c a") '(lambda () (interactive) (jcreed-qna-a)))
  (define-key notes-mode-map (kbd "C-c /") 'jcreed-browse-thing-at-point)
  (define-key notes-mode-map (kbd "C-c C-f") 'jcreed-open-file-at-point)
  (setq mode-name "Notes"))

(define-key global-map (kbd "M-p") 'jcreed-copy-path)

(defun notes-toggle-metadata ()
  "Toggle the visibility of entry metadata"
  (interactive)
  (if notes-show-metadata
		(progn (add-to-invisibility-spec '(jcreed-meta))
				 (setq notes-show-metadata nil))
	 (progn (org-remove-from-invisibility-spec '(jcreed-meta))
			  (setq notes-show-metadata t)))
  (font-lock-refresh-defaults))

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
 ;; ("\\(\\[\\[\\)\\(.*?\\)\\(\\]\\[\\)\\(.*?\\)\\(\\]\\]\\)"
 ;; 			(1 '(face jcreed-link-face invisible jcreed-meta))
 ;; 			(2 '(face jcreed-link-face invisible jcreed-meta))
 ;; 			(3 '(face jcreed-link-face invisible jcreed-meta))
 ;; 			(4 '(face jcreed-link-face))
 ;; 			(5 '(face jcreed-link-face invisible jcreed-meta)))

(setq notes-mode-highlights
		'((jcreed-find-paper-name . 'jcreed-paper-face)
		  ("\\(^=== .*\\)\\(META: .*\\)\\(\n\\)"
			(1 'jcreed-header-face)
			(2 '(face jcreed-meta-face invisible jcreed-meta))
			(3 'jcreed-header-face))
		  ("^\\(=== .*\n\\)" . (1 'jcreed-header-face))
		  ("\\(^---.*\\)\\(META: .*\\)\\(\n\\)"
			(1 'jcreed-minor-header-face)
			(2 '(face jcreed-meta-face invisible jcreed-meta))
			(3 'jcreed-minor-header-face))
		  ("\\(link:\\[\\)\\(.*?\\)\\(\\]\\[\\)\\(.*?\\)\\(\\]\\)"
			(1 '(face jcreed-link-face invisible jcreed-meta))
			(2 '(face jcreed-link-face invisible jcreed-meta))
			(3 '(face jcreed-link-face invisible jcreed-meta))
			(4 '(face jcreed-link-face))
			(5 '(face jcreed-link-face invisible jcreed-meta)))
		  ("\\(marker\\)\\(:\\[:id [^[:space:]]*?\\]\\)"
			(1 '(face jcreed-link-face display "■"))
			(2 '(face jcreed-link-face invisible jcreed-meta)))
		  ("\\(smage:\\[\\([^[:space:]]*?\\)\\]\\)"
         (0 (jcreed-render-image 0.3)))
		  ("\\(image:\\[\\([^[:space:]]*?\\)\\]\\)"
         (0 (jcreed-render-image 1)))
		  ("^---\n" . 'jcreed-shell-face)
		  ("^-~-.*\n" . 'jcreed-shell-face)
		  ("^#\\(?:\\w\\|-\\)+" . 'font-lock-type-face)
		  ("^@\\(?:\\w\\|-\\)+" . 'font-lock-type-face)
		  ("\\s-#\\(?:\\w\\|-\\)+" . 'font-lock-type-face)
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
		  ("~\\b.*?\\b~" . 'jcreed-strikethrough-face)
		  ("\\?\\?\\?" . 'jcreed-bad-face)))

;; from image:[...] matcher above
(defun jcreed-render-image (scale) (jcreed-render-named-image scale (match-string-no-properties 2)))

(defun jcreed-render-named-image (scale text)
  (put-text-property
   (match-beginning 1) (match-end 1)
   'display `(image :type png :file ,text :scale ,scale :transform-smoothing t)))

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

(ifat chef
		(define-key global-map "\C-cc" 'hs-toggle-hiding)
		(define-key global-map "\C-cH" 'hs-hide-all)
		(define-key global-map "\C-cS" 'hs-show-all))

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

(defun jcreed-browse-uuid (uuid)
  (goto-char (point-min))
  (search-forward (concat ":id " uuid))
  (beginning-of-line))

(defvar jcreed-browse-target-file-alist nil)

(defun jcreed-browse-target (target)
  (xref-push-marker-stack)
  (cond
	((string-match "^http" target)
	 (browse-url target))
	((string-match "\\(.*?\\)/\\(.*\\)" target)
	 (let* ((fileid (match-string 1 target))
			 (uuid (match-string 2 target))
			 (file (assoc fileid jcreed-browse-target-file-alist)))
		(cond
		 (file (switch-to-buffer (find-file-noselect (cdr file)))))
		(jcreed-browse-uuid uuid)))
	(t (jcreed-browse-uuid target))))

;; Can locally redefine this if desired
(defun jcreed-browse-thing-at-point-fallback ()
  (browse-url-at-point))

(defun jcreed-browse-thing-at-point (pos prefix-arg)
  (interactive "d\np")
  (let ((face (or (get-char-property (point) 'read-face-name)
						(get-char-property (point) 'face))))
	 (cond ((equal face 'jcreed-paper-face)
			  (browse-url (cadr (assoc (thing-at-point 'word) notes-data))))
			 ((equal face 'jcreed-path-face)
			  (let ((thing (thing-at-point 'filename)))
				 (when (string-match "\\(.*\\)//\\(.*\\)" thing)
					(let ((repo (match-string 1 thing))
							(path (match-string 2 thing)))
					  (jcreed-browse-repo-path repo path)))))
			 ((equal face 'jcreed-path2-face)
			  (let ((thing (face-bounded-thing-at-point (point))))
				 (when (string-match "\\(.*\\):\\[\\(.*\\)\\]" thing)
					(let ((repo (match-string 1 thing))
							(path (match-string 2 thing)))
					  (jcreed-browse-repo-path repo path)))))
			 ((equal face 'jcreed-link-face)
			  (let ((target (save-excursion
									(forward-char 5) ;; length of "link:"
						  (let* ((regexp "link:\\[\\(.*?\\)\\]\\[\\(.*?\\)\\]")
									(b (re-search-backward "link:"))
									(e (re-search-forward regexp))
									(str (buffer-substring-no-properties b e)))
							 (when  (string-match regexp str)
								(match-string 1 str))))))
				 (jcreed-browse-target target)))
			 (t (jcreed-browse-thing-at-point-fallback)))))

(defun jcreed-find-file-prefix (file prefix-arg)
  (cond ((eq prefix-arg 1) (jcreed-find-file-other-window file))
		  (t (find-file file))))

(defun jcreed-open-repo-path (repo path prefix-arg)
  (message (concat path " - " repo))
  (cond
	((equal repo "agda")
	 (jcreed-find-file-prefix (concat "/Users/jreed/.cabal/sandboxes/agda-build/agda/" path) prefix-arg))
	((equal repo "agdalib")
	 (jcreed-find-file-prefix (concat "/Users/jreed/.cabal/share/x86_64-osx-ghc-7.10.3/Agda-2.6.0/lib/" path) prefix-arg))
	((equal repo "home")
	 (jcreed-find-file-prefix (concat "/Users/jreed/" path) prefix-arg))))

(defun task-at-point ()
  (let ((word (thing-at-point 'word)))
	 (if (string-match "\\([0-9]+\\)" word)
		  (match-string 1 word)
		"")))

(defun jcreed-open-file-at-point (pos prefix-arg)
  (interactive "d\np")
  (let ((face (or (get-char-property (point) 'read-face-name)
						(get-char-property (point) 'face))))
	 (cond ((equal face 'jcreed-path-face)
			  (let ((thing (thing-at-point 'filename)))
				 (when (string-match "\\(.*\\)//\\(.*\\)" thing)
					(let ((repo (match-string 1 thing))
							(path (match-string 2 thing)))
					  (jcreed-open-repo-path repo path prefix-arg)))))
			 ((equal face 'jcreed-path2-face)
			  (let ((thing (face-bounded-thing-at-point (point))))
				 (when (string-match "\\(.*\\):\\[\\(.*\\)\\]" thing)
					(let ((repo (match-string 1 thing))
							(path (match-string 2 thing)))
					  (jcreed-open-repo-path repo path prefix-arg)))))
			 (t (jcreed-browse-thing-at-point pos prefix-arg)))))

(defun face-bounded-thing-at-point (pos)
  (buffer-substring-no-properties
	(or (previous-single-property-change pos 'face) (point-min))
	(or (next-single-property-change pos 'face) (point-max))))

(defun jcreed-thing-at-point (pos)
  (interactive "d")
  (message (thing-at-point 'filename)))

(defun jcreed-recolor-notes ()
  (interactive)
  (dolist ($buf (buffer-list (current-buffer)))
	 (with-current-buffer $buf
		(when (eq major-mode 'notes-mode)
		  (font-lock-refresh-defaults)))))

;; (defun jcreed-uuid ()
;;   (replace-regexp-in-string "\n$" ""  (shell-command-to-string "python -c 'import uuid; print(uuid.uuid4())'")))

(defun jcreed-uuid ()
  (replace-regexp-in-string "\n$" ""  (shell-command-to-string "uuidgen")))

(defun jcreed-insert-date ()
  (interactive)
  (insert (concat (format-time-string "=== %Y.%m.%d") (format " META: %s\n" `(:id ,(jcreed-uuid))))))

(defun jcreed-insert-minor-separator ()
  (interactive)
  (insert (concat (format-time-string "---") (format " META: %s\n" `(:id ,(jcreed-uuid))))))

(defun jcreed-make-marker ()
  (let ((id (jcreed-uuid)))
	 (insert (format "marker:[:id %s]" id))
	 (kill-new (format "link:[%s][link]" (jcreed-target-in-current-buffer id)))))

(defun jcreed-insert-meta (prefix-arg)
  (interactive "p")
  (cond
	((eq prefix-arg 4) (jcreed-make-marker))
	(t (insert (format " META: %s" `(:id ,(jcreed-uuid)))))))

;; https://www.reddit.com/r/emacs/comments/3ryby6/elisp_equivalente_of_refindall/cwsgbqq/
(defun string-find-all (regexp str &optional start-pos)
  (cl-loop for match-pos = (string-match regexp str start-pos)
           while match-pos
           collect (match-string 1 str)
           do (setf start-pos (1+ match-pos))))

(defun jcreed-attrs-of-text (txt)
 (mapcan (lambda (x)
			  (when (string-match "@\\([a-z]+\\): \\(.*\\)" x)
				 (list (intern (concat ":" (match-string 1 x))) (match-string 2 x))))
			(string-find-all "^\\(@[a-z]+: .*\\)$" txt)))

(defun jcreed-target-in-current-buffer (id)
  (let ((filename (car (rassoc buffer-file-name jcreed-browse-target-file-alist))))
	 (when (not filename) (error (format "don't know this file: %s" buffer-file-name)))
	 (format "%s/%s" filename id)))

(defun jcreed-current-header-line ()
  "Returns the text of the current header line, not containing the newline, either the current line if the point
is on it, or the nearest header line above the point."
  (save-excursion
    (end-of-line)
    (let* ((b (search-backward "===" nil t))
           (_ (end-of-line))
           (e (point))
           (txt (buffer-substring-no-properties b e)))
      txt)))

(defun jcreed-current-header-date ()
  "Returns the datestring (e.g. \"2022.10.01\") of the current header line."
  (let ((line (jcreed-current-header-line)))
    (when (string-match "^=== \\([0-9]+\\.[0-9]+\\.[0-9]+\\) " line) (match-string 1 line))))

(defun jcreed-make-link ()
  (interactive)
  (save-excursion
    ;; probably should reuse some code here
	 (let* ((b (re-search-backward "^---.*META\\|^===.*META" nil t))
			  (e (or (search-forward "\n\n" nil t) (buffer-end 1)))
			  (txt (buffer-substring-no-properties b e))
			  (attrs (jcreed-attrs-of-text txt)))
		(setq foobar attrs)
		(when (string-match "META: \\(.*\\)\n" txt)
		  (let* ((meta (read (match-string 1 txt)))
					(link-text (or (plist-get attrs :title) "link")))
			 (kill-new (format "link:[%s][%s]"
									 (jcreed-target-in-current-buffer (plist-get meta :id))
									 link-text)))))))

(defun jcreed-datestring-to-time (datestring)
  "takes a string like 2020.10.23 and returns an emacs time"
  (date-to-time (concat (replace-regexp-in-string "\\." "-" datestring) "T12:00:00")))

(defun jcreed-format-date (time)
  "Returns a string that represents the date of TIME in YYYY.MM.DD format."
    (format-time-string "%Y.%m.%d" time))

(defun jcreed-day-offset (offset &optional datestring)
  "Returns a string that represents the date that is OFFSET days after DATESTRING.

DATESTRING and the return value are in the format \"2020.10.23\".
If DATESTRING is nil, the current date is used."
  (let ((base-time (if datestring (jcreed-datestring-to-time datestring) (current-time))))
    (jcreed-format-date (time-add base-time (days-to-time offset)))))

(defun jcreed-first-day-of-month (time)
  "takes in a time, and returns a time that is during the first day of that month"
  (let* ((dect (decode-time time)))
    (setf (nth 3 dect) 1)
    (apply 'encode-time dect)))

(defun jcreed-last-day-of-month (time)
"takes in a time, and returns a time that is during the last day of that month"
  (let* ((dect (decode-time time))
         (month (nth 4 dect))
         (year (nth 5 dect)))
    (setf (nth 3 dect) (timezone-last-day-of-month month year))
    (apply 'encode-time dect)))

(defun jcreed-next-month (time)
"takes in a time, and returns a time that is during the next month"
  (let* ((dect (decode-time time)))
    (setf (nth 4 dect) (+ (nth 4 dect) 1))
    (apply 'encode-time dect)))


(defun jcreed-reticulate-week-template ()
  "returns reticulate-week template for current date"
  (let* ((today (jcreed-current-header-date))
         (back7 (jcreed-day-offset -7 today))
         (fore7 (jcreed-day-offset 7 today)))
    (format "@tag: reticulate-week

Q: What happened last week? (%s - %s)
A: ???

Q: How do I feel about this?
A: ???

Q: How do I see next week going? (%s - %s)
A: ???
" back7 today today fore7)))

(defun jcreed-reticulate-month-template ()
  "returns reticulate-month template for current date"
  (let* ((today (jcreed-datestring-to-time (jcreed-current-header-date)))
         (next-month (jcreed-next-month today)))
   (format "@tag: reticulate-month

Q: What happened last month? (%s - %s)
A: ???

Q: How do I feel about this?
A: ???

Q: How do I see next month going? (%s - %s)
A: ???
"
           (jcreed-format-date (jcreed-first-day-of-month today))
           (jcreed-format-date (jcreed-last-day-of-month today))
           (jcreed-format-date (jcreed-first-day-of-month next-month))
           (jcreed-format-date (jcreed-last-day-of-month next-month)))))

(defun reticulate-week ()
  (interactive)
  (insert (jcreed-reticulate-week-template)))
(defun reticulate-month ()
  (interactive)
  (insert (jcreed-reticulate-month-template)))

;; Defining paragraphs
;; Useful for delimiting =fill-paragraph=.

(setq paragraph-start "[A-Z]+:\\|\f\\|[ \t]*$")
(setq paragraph-separate "\\$\\|[a-z]+//\\|https?:\\|[A-Z]+:$\\|: \\|<<<$\\|>>>$\\|[ \t\f]*$")
(setq sentence-end-double-space nil)

(provide 'notes-mode)
