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

;; TODO intercept window changes, like `switch-to-buffer' to expand the tree
;; (defun on-window-change ()
;;   (let ((active-buf (current-buffer)))
;;     (message "current buffer is %s, visiting file %s." (buffer-name active-buf) (buffer-file-name active-buf))))
;; (add-hook 'window-configuration-change-hook #'on-window-change)

;; (create-file :path "~/dev/git/gitdrive" :expanded t)

;; options:
;; - exclude-patterns
;; - buffer
;; (project-tree-open dir options)


;; TODO project-tree--build: build a hierarchy with roots taken from
;; `project-known-project-roots'



(defun project-tree--childrenfn (folder)
  (if (file-directory-p folder)
      (progn
        (message "directory: %s" folder)
        (directory-files folder t "^[^\\.].*$"))
    nil))

(defun project-tree--build (folders)
  (let ((h (hierarchy-new)))
    (dolist (folder folders)
      (let ((dir (expand-file-name folder)))
        (hierarchy-add-tree h dir nil #'project-tree--childrenfn)))
    h))

(defun project-tree--from-known-projects ()
  (project-tree--build (project-known-project-roots)))

(defun project-tree-open ()
  (interactive)
  (let ((proj-tree-hierarchy (project-tree--from-known-projects)))
    (switch-to-buffer
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
          (when (not (file-directory-p path))
            (message "Opening %s ..." path)
            (find-file-other-window path)))))
      (get-buffer-create "*filetree*")))))

(defun project-tree--get-filetree-buffer ()
  (let ((buf (get-buffer-create "*filetree*"))
        (display-buffer-in-side-window buf '((side . left) (window-width . 0.2)))
        (set-window-dedicated-p (get-buffer-window buf))
        buf)))

;; (defun project-tree-open (dir)
;;   (let* ((h (hierarchy-new))
;;          (root-folder (expand-file-name dir))
;;          (childrenfn (lambda (it)
;;                        (message "it: %s" it)
;;                        (if (file-directory-p it)
;;                            (progn
;;                              ;; TODO list all entry paths
;;                              (message "directory: %s" it)
;;                              (directory-files it t "^[^\\.].*$"))
;;                          (message "file: %s" it)
;;                          nil))))
;;     (hierarchy-add-tree h root-folder nil childrenfn)
;;     (switch-to-buffer (hierarchy-tabulated-display
;;                        h
;;                        (hierarchy-labelfn-indent
;;                         (hierarchy-labelfn-button
;;                          (lambda (item indent)
;;                            (insert (file-name-nondirectory item)))
;;                          (lambda (item indent)
;;                            (message "You clicked on: %s" item))))
;;                        (get-buffer-create "*filetree*")))))

;; (project-tree-open)

;;;
;;; (provide: 'project-tree)
;;; project-tree.el ends here.
