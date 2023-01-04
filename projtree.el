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


(defun projtree--expand-status-symbol (path)
  (if (projtree--expanded-p path)
      "-"
    "+"))


(defun projtree--childrenfn (folder)
  (if (and (file-directory-p folder) (projtree--expanded-p folder))
      ;; TODO include hidden files
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
  (let ((buf (projtree--get-projtree-buffer)))
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
     buf)))


(defun projtree--render (project selected-path)
  ;; TODO: validate that project is a prefix of selected-path.
  (message "Opening project %s (selected path: %s)" project selected-path)
  (when selected-path
    (projtree--expand-paths (projtree--ancestor-paths project selected-path)))
  ;; create and display hierarchy rooted at project
  (let ((proj-hierarchy (projtree--build (list project))))
    (projtree--display proj-hierarchy))
  (projtree--clear-highlight)
  (when selected-path
    (projtree--highlight-file selected-path))
  (recenter))


(defun projtree--highlight-file (path)
  "Highlight a certain PATH in the project tree buffer."
  (with-current-buffer (projtree--get-projtree-buffer)
    (let ((selected-linum (cl-position path (mapcar #'car tabulated-list-entries) :test #'equal)))
      (projtree--highlight-row (+ selected-linum 1)))))


(defface projtree-highlight
  '((t :inherit highlight :extend t))
  "Default face for highlighting the visited file in the project tree."
  :group 'projtree)

(defvar projtree--hl-overlay nil)

(defun projtree--clear-highlight ()
  (when projtree--hl-overlay
    (delete-overlay projtree--hl-overlay)))

(defun projtree--highlight-row (line-number)
  "Highlight a certain LINE-NUMBER in the project tree buffer."
  (with-current-buffer (projtree--get-projtree-buffer)
    (projtree--clear-highlight)
    (goto-line line-number)
    (let* ((start (line-beginning-position))
           (end (line-end-position))
           (hl-overlay (make-overlay start (+ 1 end))))
      (overlay-put hl-overlay 'face 'projtree-highlight)
      (overlay-put hl-overlay 'before-string (propertize "X" 'display (list 'left-fringe 'right-triangle)))
      (setq projtree--hl-overlay hl-overlay))))


(defun projtree-open ()
  "Render a project tree rooted at the current project.
The currently visited project file (if any) is highlighted."
  (interactive)
  (message "projtree-open ...")
  (let ((proj (project-root (project-current)))
        (selected-file (buffer-file-name projtree--visited-buffer)))
    (projtree--render proj selected-file)))

(defun projtree-close ()
  (interactive)
  (let ((buf (get-buffer "*projtree*")))
    (when buf
      (kill-buffer buf))))

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


(defun projtree--call-async (fn)
  "Call function FN asynchronously."
  (run-with-idle-timer 0 nil (funcall fn)))


(defvar projtree--visited-buffer nil
  "Tracks the currently visited buffer in `projtree-mode'.")

;; (defvar projtree--selected-path nil
;;   "Tracks the file visited by the current buffer in `projtree-mode'.")


(defun projtree--render-on-buffer-switch ()
  (when (not (minibufferp (current-buffer)))
    (let ((prior-buffer projtree--visited-buffer)
          (current-buffer (current-buffer)))
      (when (not (eq prior-buffer current-buffer))
        (setq projtree--visited-buffer current-buffer)
        (projtree-open)))))

;;;###autoload
(define-minor-mode projtree-mode
  "TODO describe."
  :lighter nil ;; Do not display on mode-line.
  (if projtree-mode
      (progn
        (add-hook 'window-configuration-change-hook #'projtree--render-on-buffer-switch)
        (setq projtree--visited-buffer (current-buffer))
        (projtree-open))
    (remove-hook 'window-configuration-change-hook #'projtree--render-on-buffer-switch)
    (projtree-close)))


(provide 'projtree)

;;; projtree.el ends here.
