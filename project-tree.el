(require 'hierarchy)

;; TODO
(cl-defstruct (file (:constructor create-file)
                    (:conc-name ))
  path
  expanded
  ignores)

;; TODO: method: file-abspath
;; TODO: method: file-dir-p
;; TODO: method: file-children (optional: ignores which override file.ignores)
;; TODO: method: file-parent
;; TODO: method: file-toggle-expanded: (note: propagate upwards tree)

;; (create-file :path "~/dev/git/gitdrive" :expanded t)

;; options:
;; - exclude-patterns
;; - buffer
;; (project-tree-open dir options)


;; TODO project-tree--build: build a hierarchy with roots taken from
;; `project-known-project-roots'



(defun project-tree--childrenfn (folder)
  ;; TODO (and (file-directory-p folder) (project-tree--expanded-p folder))
  (if (file-directory-p folder)
      (progn
        (message "directory: %s" folder)
        (directory-files folder t "^[^\\.].*$"))
    nil))

(defun project-tree--build (folders)
  (let ((h (hierarchy-new)))
    (dolist (folder folders)
      (let ((root (string-trim-right (expand-file-name folder) "/")))
        ;; TODO (project-tree--expand root)
        (hierarchy-add-tree h root nil #'project-tree--childrenfn)))
    h))

(defun project-tree--from-known-projects ()
  (project-tree--build (project-known-project-roots)))

;; TODO `project-tree-follow-mode': intercept window changes, like
;; `switch-to-buffer' to (1) mark the file and all parent directories expanded,
;; (2) render the project tree.
;; (defun on-window-change ()
;;   (let ((active-buf (current-buffer)))
;;     (message "current buffer is %s, visiting file %s." (buffer-name active-buf) (buffer-file-name active-buf))))
;; (add-hook 'window-configuration-change-hook #'on-window-change)


(defun project-tree-open ()
  (interactive)
  ;; TODO only draw project tree from (cdr (project-current))?
  (let ((proj-tree-hierarchy (project-tree--from-known-projects)))
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
         ;; TODO for directories: (project-tree--expand folder)
         (when (not (file-directory-p path))
           (message "Opening %s ..." path)
           (find-file-other-window path)))))
     (project-tree--get-filetree-buffer))))

(defun project-tree--get-filetree-buffer ()
  (let ((buf (get-buffer-create "*filetree*")))
    (display-buffer-in-side-window buf '((side . left) (window-width . 30) (dedicated . t)))
    (let ((win (get-buffer-window buf)))
      ;; Make window dedicated to filetree buffer.
      (set-window-dedicated-p win t)
      ;; Make C-x 1 not close the window.
      (set-window-parameter win 'no-delete-other-windows t))
    buf))


;; (project-tree-open)

;;;
;;; (provide: 'project-tree)
;;; project-tree.el ends here.
