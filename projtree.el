(require 'hierarchy)

;; TODO
(cl-defstruct (projtree (:constructor create-file)
                        (:conc-name ))
  cursor ;; TODO highlighted item in tree?
  expanded-paths ;; TODO hashtable of path->bool
  )


(defvar projtree--table (make-hash-table :test 'equal)
  "A table that stores project trees (`projtree' instances).
The project trees are keyed on project root path.")

(defvar projtree--expanded-paths (make-hash-table :test 'equal))

;; TODO (project-root path)
(defun projtree--expanded-p (path)
  (gethash path projtree--expanded-paths))

(defun projtree--expand (path)
  (puthash path t projtree--expanded-paths))

(defun projtree--toggle-expand (path)
  (puthash path (not (projtree--expanded-p path)) projtree--expanded-paths))



;; TODO: method: file-abspath
;; TODO: method: file-dir-p
;; TODO: method: file-children (optional: ignores which override file.ignores)
;; TODO: method: file-parent
;; TODO: method: file-toggle-expanded: (note: propagate upwards tree)

;; (create-file :path "~/dev/git/gitdrive" :expanded t)

;; options:
;; - exclude-patterns
;; - buffer
;; (projtree-open dir options)


;; TODO projtree--build: build a hierarchy with roots taken from
;; `project-known-project-roots'



(defun projtree--childrenfn (folder)
  (if (and (file-directory-p folder) (projtree--expanded-p folder))
      (directory-files folder t "^[^\\.].*$")
    nil))

(defun projtree--build (folders)
  (let ((h (hierarchy-new)))
    (dolist (folder folders)
      (let ((root (string-trim-right (expand-file-name folder) "/")))
        (projtree--expand root)
        (hierarchy-add-tree h root nil #'projtree--childrenfn)))
    h))

(defun projtree--from-known-projects ()
  (projtree--build (project-known-project-roots)))

(defun projtree--from-current-project ()
  (let ((proj-current (cdr (project-current))))
    (projtree--build (list proj-current))))

;; TODO `projtree-follow-mode': intercept window changes, like
;; `switch-to-buffer' to (1) mark the file and all parent directories expanded,
;; (2) render the project tree.
;; (defun on-window-change ()
;;   (let ((active-buf (current-buffer)))
;;     (message "current buffer is %s, visiting file %s." (buffer-name active-buf) (buffer-file-name active-buf))))
;; (add-hook 'window-configuration-change-hook #'on-window-change)


(defun projtree-open ()
  "Render the project tree."
  (interactive)
  ;; TODO only draw project tree from (cdr (project-current))?
  (message "projtree-open ...")
  (let ((proj-tree-hierarchy (projtree--from-current-project)))
    (hierarchy-tabulated-display
     proj-tree-hierarchy
     (hierarchy-labelfn-indent
      (hierarchy-labelfn-button
       ;; labelfn
       (lambda (path indent)
         (let ((file-name (file-name-nondirectory path)))
           (if (file-directory-p path)
               (insert (propertize file-name 'face '(dired-directory)))
             (insert (propertize file-name 'face '(default))))))
       ;; actionfn
       (lambda (path indent)
         (if (file-directory-p path)
             (progn
               (projtree--toggle-expand path)
               (projtree-open))
           (message "Opening %s ..." path)
           (find-file-other-window path)))))
     (projtree--get-filetree-buffer))))

(defun projtree--get-filetree-buffer ()
  (let ((buf (get-buffer-create "*filetree*")))
    (display-buffer-in-side-window buf '((side . left) (window-width . 30) (dedicated . t)))
    (let ((win (get-buffer-window buf)))
      ;; Make window dedicated to filetree buffer.
      (set-window-dedicated-p win t)
      ;; Make C-x 1 not close the window.
      (set-window-parameter win 'no-delete-other-windows t))
    buf))


;; (projtree-open)

;;;
;;; (provide: 'projtree)
;;; projtree.el ends here.
