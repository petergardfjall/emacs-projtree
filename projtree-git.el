;;; projtree.el --- Display project directory tree of visited file -*- lexical-binding: t -*-

(defvar projtree-git--cmd (executable-find "git")
  "Full system path to a git executable.")

(defun projtree-git--status (root-dir)
  "Run git status in the given git ROOT-DIR.
The status is returned as a hash table where the keys are
absolute file paths and values are status codes following the git
status porcelain format.  Note that up-to-date files do not have
entries in the resulting hash table."
  (with-temp-buffer
    (setq-local default-directory root-dir)
    (let* ((buf (current-buffer))
           (statuses (make-hash-table :test 'equal))
           (exit-code (call-process projtree-git--cmd nil buf nil "status" "--porcelain" "--untracked-files=normal" "--ignored=matching")))
      (if (> exit-code 0)
          (progn
            (message "projtree-git: git status for %s gave non-zero exit code: %d" root-dir exit-code)
            statuses)
        (projtree-git--parse-git-status-output buf)))))


(defun projtree-git--parse-git-status-output (buffer)
  "Parse git status output from BUFFER and return a status hash table.
BUFFER is assumed to hold git status in porcelain format and
nothing else.  The status hash table keys are absolute file paths
and values are status codes in the git status porcelain format."
  (with-current-buffer buffer
    (let ((root-dir default-directory)
          (statuses (make-hash-table :test 'equal)))
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
               (tokens (split-string line))
               (status (nth 0 tokens))
               ;; Normally the file follows the status code, but for renames the
               ;; new filename comes later: "R prior/path -> new/path"
               (file (if (equal status "R") (nth 3 tokens) (nth 1 tokens)))
               (path (projtree--abspath (expand-file-name file))))
          (puthash path status statuses)
          ;; Mark modification states for any ancestor directories.
          (dolist (ancestor (butlast (projtree--ancestor-paths root-dir path)))
            (pcase (string-to-char status)
              (?M (puthash ancestor "M" statuses))
              (?A (puthash ancestor "M" statuses))
              (?D (puthash ancestor "M" statuses))
              (?U (puthash ancestor "U" statuses))))
          ;; Next line of output.
          (forward-line 1)))
      statuses)))

(defun projtree-git--status-face (code)
  "Return the face corresponding to the given git status CODE.
The status CODE is expected to be on the git status porcelain format."
  (pcase (string-to-char code)
    (?M 'projtree-git-modified)
    (?U 'projtree-git-conflict)
    (?A 'projtree-git-added)
    (?R 'projtree-git-renamed)
    (?? 'projtree-git-untracked)
    (?! 'projtree-git-ignored)
    (_ nil)))

(provide 'projtree-git)

;;; projtree-git.el ends here
