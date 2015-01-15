
(setq auto-mode-alist
      (cons '("\\.cf\\'" . java-mode)
	    auto-mode-alist))

(defvar c-default-style
  '((c-mode . "stroustrup")
    (c++-mode . "stroustrup")
    (java-mode . "java")))

(add-hook 'java-mode-hook
	  (lambda ()
	    (set-variable 'c-basic-offset 4)))

(add-hook 'c-mode-hook
	  (lambda ()
	    (set-variable 'show-trailing-whitespace t)))

(add-hook 'c++-mode-hook
	  (lambda ()
	    (set-variable 'show-trailing-whitespace t)))

(add-hook 'c-mode-common-hook
          (lambda ()
             (c-set-offset 'case-label '2)
	     (c-set-offset 'statement-case-intro '2)))

; Belt and suspenders on this one

(eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
(remove-hook 'find-file-hooks 'vc-find-file-hook)

; Source grep

; TODO: must include the js/public directory also
; TODO: would be helpful for files to be sorted by basename first, extension last
; TODO: should exclude misc benchmarking directories, notably octane (many false hits)
; TODO: should exclude build directories
; TODO: probably useful to have a 'cgrep' variant that excludes all js code
; TODO: a variant 'dgrep' should take an identifier and try to find candidates
;       for its definition.  This would have to be heuristic, and a number of
;       the heuristics would be to reject candidates.
; TODO: should maintain a separate window for each search term (*grep foo*, *grep bar*)
;       to simplify recursive searches
; TODO: when the buffer is *grep* we should really not fall back to the default,
;       but should look in the first line to see if there's a directory there
;       that matches our criteria.

(defvar *sgrep-default-dir* "/home/lth/moz/mozilla-inbound/js/src")
(defvar *sgrep-files* "*.h *.c *.cpp *.js")
(defvar *cgrep-files* "*.h *.c *.cpp")

(defun sgrep (pattern)
  "Recursive grep across *sgrep-files* within *sgrep-dir*."
  (interactive 
   (progn
     (grep-compute-defaults)		; A hack - forces grep to be loaded
     (list (let* ((def (current-word))
		  (prompt (if (null def)
			      "Find: "
			    (concat "Find (default " def "): "))))
	     (read-string prompt nil nil def)))))
  (rgrep pattern *sgrep-files* (compute-sgrep-dir (buffer-file-name))))

(defun cgrep (pattern)
  "Recursive grep across *cgrep-files* within *sgrep-dir*."
  (interactive 
   (progn
     (grep-compute-defaults)		; A hack - forces grep to be loaded
     (list (let* ((def (current-word))
		  (prompt (if (null def)
			      "Find: "
			    (concat "Find (default " def "): "))))
	     (read-string prompt nil nil def)))))
  (rgrep pattern *cgrep-files* (compute-sgrep-dir (buffer-file-name))))

(defun compute-sgrep-dir (fn)
  (if (not fn)
      *sgrep-default-dir*
    (let ((dir (file-name-directory fn)))
      (if (not dir)
	  *sgrep-default-dir*
	(setq dir (directory-file-name dir))
	(while (and dir
		    (let ((base (file-name-nondirectory dir)))
		      (and base 
			   (not (string-match-p "^mozilla-[a-z]+" base)))))
	  (if (not (string-match-p "mozilla-[a-z]+" dir))
	      (setq dir nil)
	    (let ((ndir (file-name-directory dir)))
	      (setq dir (and ndir (directory-file-name ndir))))))
	(if dir
	    (concat (file-name-as-directory dir) "js/src")
	  *sgrep-default-dir*)))))
