(require 'hierarchy)

;; TODO
(cl-defstruct (projtree (:constructor create-file)
                        (:conc-name ))
  cursor ;; TODO highlighted item in tree?
  expanded-paths ;; TODO hashtable of path->bool
  )

;; TODO
;; options:
;; - exclude-patterns
;; - buffer
;; (projtree-open dir options)



(defvar projtree--table (make-hash-table :test 'equal)
  "A table that stores project trees (`projtree' instances).
The project trees are keyed on project root path.")

(defvar projtree--expanded-paths (make-hash-table :test 'equal))

;; TODO (project-root path)
(defun projtree--expanded-p (path)
  (let ((path (projtree--abspath path)))
    (gethash path projtree--expanded-paths)))

(defun projtree--expand-path (path)
  (let ((path (projtree--abspath path)))
    (puthash path t projtree--expanded-paths)))

(defun projtree--expand-paths (paths)
  (dolist (p paths)
    (projtree--expand-path p)))

(defun projtree--toggle-expand (path)
  (let ((path (projtree--abspath path)))
    (puthash path (not (projtree--expanded-p path)) projtree--expanded-paths)))




;; TODO projtree--build: build a hierarchy with roots taken from
;; `project-known-project-roots'

(defun projtree--expand-status-symbol (path)
  (if (projtree--expanded-p path)
      "-"
    "+"))


(defun projtree--childrenfn (folder)
  (if (and (file-directory-p folder) (projtree--expanded-p folder))
      (directory-files folder t "^[^\\.].*$")
    nil))

(defun projtree--build (folders)
  (let ((h (hierarchy-new)))
    (dolist (folder folders)
      (let ((root (string-trim-right (expand-file-name folder) "/")))
        (projtree--expand-path root)
        (hierarchy-add-tree h root nil #'projtree--childrenfn)))
    h))


(defun projtree--from-known-projects ()
  (projtree--build (project-known-project-roots)))


(defun projtree--from-current-project ()
  (let ((proj-current (project-root (project-current))))
    (projtree--build (list proj-current))))

;; TODO `projtree-follow-mode': intercept window changes, like
;; `switch-to-buffer' to (1) mark the file and all parent directories expanded,
;; (2) render the project tree.
;; (defun on-window-change ()
;;   (let ((active-buf (current-buffer)))
;;     (message "current buffer is %s, visiting file %s." (buffer-name active-buf) (buffer-file-name active-buf))))
;; (add-hook 'window-configuration-change-hook #'on-window-change)


;; TODO split into projtree-{jump,open} and projtree-render(project
;; selected-path)? (selected-path can be used to place cursor at a certain
;; point/line in buffer)

(defun projtree--descendant-p (ancestor child)
  (let ((ancestor (projtree--abspath ancestor))
        (child (projtree--abspath child)))
    (string-prefix-p ancestor child)))

(defun projtree--ancestor-paths (project path)
  (let ((project (projtree--abspath project))
        (path (projtree--abspath path)))
    (when (not (projtree--descendant-p project path))
      (error "Path %s is not a sub-directory of %s" path project))
    (if (not (string-equal project path))
        (let ((parent (file-name-directory path)))
          (append (projtree--ancestor-paths project parent) (list path)))
      (list path))))


(defun projtree--display (proj-hierarchy)
  (hierarchy-tabulated-display
   proj-hierarchy
   (hierarchy-labelfn-indent
    (hierarchy-labelfn-button
     ;; labelfn
     (lambda (path indent)
       (let ((file-name (file-name-nondirectory path)))
         (if (file-directory-p path)
             (insert (propertize (format "%s %s" (projtree--expand-status-symbol path) file-name) 'face '(dired-directory)))
           (insert (propertize file-name 'face '(default))))))
     ;; actionfn
     (lambda (path indent)
       (if (file-directory-p path)
           (progn
             (projtree--toggle-expand path)
             (projtree-open))
         (message "Opening %s ..." path)
         (find-file-other-window path)))))
   (projtree--get-projtree-buffer)))


(defun projtree--render (project selected-path)
  ;; TODO: validate that project is a prefix of selected-path.
  (message "Opening project %s (selected path: %s)" project selected-path)
  (when selected-path
    (projtree--expand-paths (projtree--ancestor-paths project selected-path)))
  ;; create and display hierarchy rooted at project
  (let ((proj-hierarchy (projtree--build (list project))))
    (projtree--display proj-hierarchy))

  ;; highlight selected line in project tree buffer.
  (when selected-path
    (with-current-buffer (projtree--get-projtree-buffer)
      (message "tabulated-list-entries: %s" (mapcar #'car tabulated-list-entries))
      (let ((selected-linum (cl-position selected-path (mapcar #'car tabulated-list-entries) :test #'equal)))
        (projtree--highlight-row (+ selected-linum 1))))))

(defun projtree--highlight-row (linum)
  ;; TODO: create an overlay in the projtree buffer at the given line number.
  (with-current-buffer (projtree--get-projtree-buffer)
    (message "Highlighting row %s" linum)
    (save-excursion
      (goto-line linum)
      (let* ((start (line-beginning-position))
             (end (line-end-position))
             (hl-overlay (make-overlay start end)))
        (overlay-put hl-overlay 'face 'highlight)
      ))))


(defun projtree-open ()
  "Render a project tree rooted at the current project with the
currently visited project file (if any) highlighted."
  (interactive)
  (message "projtree-open ...")
  (let ((proj (project-root (project-current)))
        ;; TODO determine selected-file with timer or hook
        (selected-file (buffer-file-name (current-buffer))))
    (projtree--render proj selected-file)))


(defun projtree--get-projtree-buffer ()
  (let ((buf (get-buffer-create "*projtree*")))
    (display-buffer-in-side-window buf '((side . left) (window-width . 30) (dedicated . t)))
    (let ((win (get-buffer-window buf)))
      ;; Make window dedicated to projtree buffer.
      (set-window-dedicated-p win t)
      ;; Make C-x 1 not close the window.
      (set-window-parameter win 'no-delete-other-windows t))
    buf))


(defun projtree--abspath (path)
  "Return a normalized path (absolute and no trailing slash)."
  (string-trim-right (expand-file-name path) "/"))


;; (projtree-open)

;;;
;;; (provide: 'projtree)
;;; projtree.el ends here.
