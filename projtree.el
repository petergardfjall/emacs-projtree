;;; projtree.el --- Display project directory tree of visited file.  -*- lexical-binding: t -*-
(require 'hierarchy)

(cl-defstruct projtree-set
  "A collection of project trees (`projtree' instances).
The project trees are identified by project root path."
  trees)

(defun projtree-set--new ()
  "Create an empty `projtree-set'."
  (make-projtree-set :trees (make-hash-table :test 'equal)))

(defun projtree-set->get-projtree (self project-root)
  "Get project tree from the projtree-table SELF for the given PROJECT-ROOT.
If the requested `projtree' does not already exist it is created."
  (let* ((trees (projtree-set-trees self))
         (tree (gethash project-root trees)))
    ;; Add a new project tree if one doesn't exist.
    (when (not tree)
      (puthash project-root (projtree->new project-root) trees))
    (gethash project-root trees)))


(defvar projtree--active-set nil
  "Holds the active `projtree-set'.")

(defun projtree-active-set ()
  (when (not projtree--active-set)
    (setq projtree--active-set (projtree-set--new)))
  projtree--active-set)



(cl-defstruct projtree
  "TODO"
  ;; The root directory of the project tree.
  root
  ;; Tracks which paths of the project tree are expanded (to be displayed).
  expanded-paths
  ;; Tracks which path in the project tree is currently visited (and therefore
  ;; should be highlighted).
  ;; TODO: actually make use of this
  selected-path)

(defun projtree->new (project-root)
  "Create an empty projtree rooted at directory PROJECT-ROOT."
  (make-projtree :root project-root
                 :expanded-paths (make-hash-table :test 'equal)
                 :selected-path nil))

(defun projtree->root (self)
  (projtree-root self))

(defun projtree->expanded-p (self path)
  (let ((path (projtree--abspath path)))
    (gethash path (projtree-expanded-paths self))))

(defun projtree->expand-path (self path)
  (let ((path (projtree--abspath path)))
    (puthash path t (projtree-expanded-paths self))))

(defun projtree->expand-paths (self paths)
  (dolist (p paths)
    (projtree->expand-path self p)))

(defun projtree->toggle-expand (self path)
  (let ((path (projtree--abspath path))
        (value (not (projtree->expanded-p self path))))
    (puthash path value (projtree-expanded-paths self))))

(defun projtree->display (self buffer)
  "Display project tree SELF in the given BUFFER.
Overwrites any prior BUFFER content."
  (let ((proj-hierarchy (projtree->_build-hierarchy self)))
    (hierarchy-tabulated-display
     proj-hierarchy
     (hierarchy-labelfn-indent
      (hierarchy-labelfn-button
       ;; labelfn
       (lambda (path indent)
         (let ((file-name (file-name-nondirectory path)))
           (if (file-directory-p path)
               (insert (propertize (format "%s %s" (projtree->_expand-status-symbol self path) file-name) 'face '(dired-directory)))
             (insert (propertize file-name 'face '(default))))))
       ;; actionfn
       (lambda (path indent)
         (if (file-directory-p path)
             (progn
               (projtree->toggle-expand self path)
               ;; TODO: can we just do (projtree->display self buffer)?
               (projtree-open))
           (message "Opening %s ..." path)
           (find-file-other-window path)))))
     buffer)))

(defun projtree->_expand-status-symbol (self path)
  (if (projtree->expanded-p self path)
      "-"
    "+"))

(defun projtree->_children-fn (self)
  "Return a function for SELF that lists child files for expanded directories."
  (lambda (folder)
    (if (and (file-directory-p folder)
             (projtree->expanded-p self folder))
        ;; TODO include hidden files but avoid "." and ".."
        (directory-files folder t "^[^\\.].*$")
      nil)))

(defun projtree->_build-hierarchy (self)
  (let* ((root-dir (projtree--abspath (projtree->root self))))
    ;; TODO maybe move to construction
    (projtree->expand-path self root-dir)
    (let ((h (hierarchy-new)))
      (hierarchy-add-tree h root-dir nil (projtree->_children-fn self))
      h)))


(defun projtree--current ()
  "Return the project tree rooted at the current/most recently visited file.
Will return nil if the visited file is not in a project structure."
  (let ((root (projtree--project-root projtree--visited-buffer)))
    (if root
        (projtree-set->get-projtree (projtree-active-set) root)
      nil)))

(defun projtree--project-root (buffer)
  "Return the root project directory of BUFFER or nil if none is available."
  (with-current-buffer buffer
    (let* ((buffer-dir default-directory)
           (project (project-current nil buffer-dir)))
      (if project
          (project-root project)
        nil))))


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


;; TODO make selected-path part of projtree?
(defun projtree--render (projtree selected-path)
  (message "Opening project %s (selected path: %s)" (projtree->root projtree) selected-path)
  (when selected-path
    ;; TODO in project trees table
    (let ((root (projtree->root projtree)))
      (projtree->expand-paths projtree (projtree--ancestor-paths root selected-path))))
  (projtree->display projtree (projtree--get-projtree-buffer))
  (projtree--clear-highlight)
  (when selected-path
    (projtree--highlight-file selected-path)))


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
    (goto-line line-number)
    (let* ((start (line-beginning-position))
           (end (line-end-position))
           (hl-overlay (make-overlay start (+ 1 end))))
      (overlay-put hl-overlay 'face 'projtree-highlight)
      (overlay-put hl-overlay 'before-string (propertize "X" 'display (list 'left-fringe 'right-triangle)))
      (setq projtree--hl-overlay hl-overlay)
      ;; Move point to highlighted row in in project tree buffer window.
      (set-window-point (get-buffer-window (current-buffer)) start))))




(defun projtree-open ()
  "Render a project tree rooted at the current project.
The currently visited project file (if any) is highlighted."
  (interactive)
  (let ((projtree (projtree--current))
        (selected-file (buffer-file-name projtree--visited-buffer)))
    (when projtree
      ;; TODO (projtree->render projtree)
      (projtree--render projtree selected-file))))


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
  "Return a normalized PATH (absolute and no trailing slash)."
  (string-trim-right (expand-file-name path) "/"))


(defun projtree--call-async (fn)
  "Call function FN asynchronously."
  (run-with-idle-timer 0 nil (funcall fn)))


(defvar projtree--visited-buffer nil
  "Tracks the currently visited buffer in `projtree-mode'.")


(defun projtree--file-visiting-buffer-p (buffer)
  "Return t for a BUFFER that is visiting a file, nil otherwise."
  (buffer-file-name buffer))


(defun projtree--render-on-buffer-switch ()
  "TODO."
  ;; No need to re-render project tree if we're in the mini-buffer, in the
  ;; project tree buffer, or any other buffer not visiting a file.
  (let ((curr-buf (current-buffer)))
    (when (projtree--file-visiting-buffer-p curr-buf)
      (let ((prior-buffer projtree--visited-buffer))
        (when (not (eq prior-buffer curr-buf))
          (setq projtree--visited-buffer curr-buf)
          (projtree-open))))))


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
