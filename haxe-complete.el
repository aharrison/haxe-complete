(defun get-completes-from-haxe (hxml-file file pos)
  (let* ((completion-buffer (get-buffer-create 
			     "*haxe-completions*"))
         (cmd (concat "cd " 
		      (file-name-directory hxml-file)  
		      "; haxe " 
		      hxml-file " --display " file "@" 
		      (number-to-string (+ pos 1)))))
    (shell-command cmd completion-buffer completion-buffer)
    (let ((clist (xml-parse-region 1 (buffer-size completion-buffer) completion-buffer))
	  (completes nil))
      (dolist (s (cddar clist))
	(when (listp s)
	  (let* ((item (cdr s))
		 (name (cdaar item))
		 (type (car (cddadr item)))
		 (desc (cdddr item)))
	    (setf completes (cons (concat "." name) completes)))))
      completes)))


(defun file-find-upwards (buffer file-name)
  ;; Chase links in the source file and search in the dir where it  points.
  (setq dir-name (or (and (buffer-file-name buffer)
                          (file-name-directory (file-chase-links
                                                (buffer-file-name buffer))))
                     default-directory))
  ;; Chase links before visiting the file.  This makes it easier to
  ;; use a single file for several related directories.
  (setq dir-name (file-chase-links dir-name))
  (setq dir-name (expand-file-name dir-name))
  ;; Move up in the dir hierarchy till we find a change log file.
  (let ((file1 (concat dir-name file-name))
        parent-dir)
    (while (and (not (file-exists-p file1))
                (progn (setq parent-dir
                             (file-name-directory
                              (directory-file-name
                               (file-name-directory file1))))
                       ;; Give up if we are already at the root dir.
                       (not (string= (file-name-directory file1)
                                     parent-dir))))
      ;; Move up to the parent dir and try again.
      (setq file1 (expand-file-name file-name parent-dir)))
    ;; If we found the file in a parent dir, use that.  Otherwise,
    ;; return nil
    (if (or (get-file-buffer file1) (file-exists-p file1))
        file1
      nil)))


(defun icandidate-list ()
  (interactive)
  (get-candidate-list (current-buffer) (point)))

(defun get-candidate-list (buffer pos)
  (get-completes-from-haxe 
   (file-find-upwards buffer "compile.hxml") 
   (buffer-file-name buffer) 
   pos))


(defun haxe-company-backend (command &optional arg &rest ignored)
  (case command
    ('prefix (get-prefix))
    ('candidates (candidate-list-helper arg))
    ('no-cache t)
    ('sorted t)
    ('post-completion (delete-window (get-buffer-create "*haxe-completions*")))
    ('meta (format "This value is named %s" arg))))

(defun candidate-list-helper (arg)
  (when (buffer-modified-p)
    (basic-save-buffer))
  (get-candidate-list (current-buffer) (- (position-bytes (point)) (length args))))

(defun get-prefix ()
  (when  (looking-back "\\.\\w*") "."))

(provide 'haxe-company-backend)
(require 'haxe-company-backend)

(provide 'haxe-complete)

(require 'flymake)

(defun flymake-haxe-init ()
    (list "haxe" 
	  (list (concat (file-find-upwards (current-buffer) "compile.hxml")))
	  (replace-in-string "compile.hxml" "" (file-find-updates (current-buffer) "compile.hxml"))

(setq flymake-err-line-patterns
      (list '("^\\(.*?\\):\\([0-9]*?\\): characters \\(.*?\\) : \\(.*?\\)$" 
	      1 2 nil 4 nil 4)))

(add-to-list 'flymake-allowed-file-name-masks '("\\.hx\\'" flymake-haxe-init))

(add-hook 'haxe-mode-hook
	  (lambda ()
	    (flymake-mode)
	    (company-mode 1)
	    (set (make-local-variable 'company-backends)
		 '(haxe-company-backend))))

(defun flymake-start-syntax-check-process (cmd args dir)
  "Start syntax check process."
  (let* ((process nil))
    (condition-case err
	(progn
	  (when dir
	    (let ((default-directory dir))
	      (flymake-log 3 "starting process on dir %s" default-directory)))
	  (cd dir)
	  (setq process (apply 'start-process "flymake-proc" (current-buffer) cmd args))
	  (set-process-sentinel process 'flymake-process-sentinel)
	  (set-process-filter process 'flymake-process-filter)
          (push process flymake-processes)

          (setq flymake-is-running t)
          (setq flymake-last-change-time nil)
          (setq flymake-check-start-time (flymake-float-time))

	  (flymake-report-status nil "*")
	  (flymake-log 2 "started process %d, command=%s, dir=%s"
		       (process-id process) (process-command process)
                       default-directory)
	  process)
      (error
       (let* ((err-str (format "Failed to launch syntax check process '%s' with args %s: %s"
			       cmd args (error-message-string err)))
	      (source-file-name buffer-file-name)
	      (cleanup-f        (flymake-get-cleanup-function source-file-name)))
	 (flymake-log 0 err-str)
	 (funcall cleanup-f)
	 (flymake-report-fatal-status "PROCERR" err-str))))))
