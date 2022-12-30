;;; projtree.el --- Display project directory tree of visited file  -*- lexical-binding: t -*-
;;
;; Copyright © 2023 Peter Gardfjäll <peter.gardfjall.work@gmail.com>
;;
;; Author: Peter Gardfjäll <peter.gardfjall.work@gmail.com>
;; URL: https://github.com/petergardfjall/emacs-projtree
;; Keywords: workspace, project
;; Package-Requires: ((emacs "28.1"))
;; Version: 0.1.0
;; Homepage: https://github.com/petergardfjall/emacs-projtree
;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; `projtree-mode' is an Emacs minor mode that shows a file explorer (the
;; project tree buffer) for the code tree of the current project (as determined
;; by project.el).  `projtree-mode' follows the active buffer such that it
;; always displays the file tree rooted at the project root folder of the
;; currently visited file.
;;
;;; Code:
;;

(require 'hierarchy)
(require 'project)

(defcustom projtree-window-width 30
  "The width in characters to use for the project tree buffer."
  :type 'natnum
  :group 'projtree)

(defcustom projtree-window-placement 'left
  "The placement of the project tree window."
  :type '(choice (const :tag "Left-hand side" left)
                 (const :tag "Right-hand side" right))
  :group 'projtree)

(defcustom projtree-buffer-name "*projtree*"
  "The name to use for the project tree buffer."
  :type 'string
  :group 'projtree)

(defcustom projtree-show-git-status t
  "Whether to show git status for project tree files and folders."
  :type 'boolean
  :group 'projtree)

(defcustom projtree-profiling-enabled nil
  "Whether to output performance profiling."
  :type 'boolean
  :group 'projtree)

(defface projtree-highlight
  '((t :inherit highlight :extend t))
  "Face for highlighting the visited file in the project tree."
  :group 'projtree)

(defface projtree-dir
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting unmodified folders in the project tree."
  :group 'projtree)

(defface projtree-file
  '((t :inherit default))
  "Face for highlighting unmodified files in the project tree."
  :group 'projtree)

(defface projtree-git-modified
  '((t :inherit (warning)))
  "Face for highlighting modified files and folders in the project tree."
  :group 'projtree)

(defface projtree-git-added
  '((t :inherit success))
  "Face for highlighting added files in the project tree."
  :group 'projtree)

(defface projtree-git-renamed
  '((t :inherit (success italic)))
  "Face for highlighting renamed files in the project tree."
  :group 'projtree)

(defface projtree-git-ignored
  '((t :inherit shadow :strike-through t))
  "Face for highlighting ignored files in the project tree."
  :group 'projtree)

(defface projtree-git-untracked
  '((t :inherit shadow))
  "Face for highlighting untracked files in the project tree."
  :group 'projtree)

(defface projtree-git-conflict
  '((t :inherit error))
  "Face for highlighting files with unresolved conflicts in the project tree."
  :group 'projtree)

(defvar projtree-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")    'projtree-open)
    map)
  "Keybindings available in the project tree buffer.")


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
  "Represents a single project tree and its state."

  ;; The root directory of the project tree.
  root
  ;; The current cursor position in the project tree buffer for this project
  ;; tree. This will be set whenever a file is opened or a folder is
  ;; opened/folded in the project tree buffer to avoid jumpy cursor behavior on
  ;; re-rendering of the tree.
  cursor
  ;; Tracks which paths of the project tree are expanded (to be displayed).
  expanded-paths
  ;; Tracks which path in the project tree is currently visited (and therefore
  ;; should be highlighted).
  selected-path)

(defun projtree->new (project-root)
  "Create an empty projtree rooted at directory PROJECT-ROOT."
  (make-projtree :root (projtree--abspath project-root)
                 :cursor nil
                 :expanded-paths (make-hash-table :test 'equal)
                 :selected-path nil))

(defun projtree->root (self)
  (projtree-root self))

(defun projtree->cursor (self)
  (projtree-cursor self))

(defun projtree->set-cursor (self point)
  (setf (projtree-cursor self) point))

(defun projtree->selected-path (self)
  (projtree-selected-path self))

(defun projtree->set-selected-path (self path)
  (let ((path (projtree--abspath path)))
    (setf (projtree-selected-path self) path)))

(defun projtree->expanded-p (self path)
  (let ((path (projtree--abspath path)))
    (gethash path (projtree-expanded-paths self))))

(defun projtree->expand-path (self path)
  (let ((path (projtree--abspath path)))
    (puthash path t (projtree-expanded-paths self))))

(defun projtree->expand-paths (self paths)
  (dolist (p paths)
    (projtree->expand-path self p)))

(defun projtree->expand-to-root (self path)
  (let* ((root (projtree->root self))
         (ancestors (projtree--ancestor-paths root path)))
    (projtree->expand-paths self ancestors)))

(defun projtree->toggle-expand (self path)
  (let ((path (projtree--abspath path))
        (value (not (projtree->expanded-p self path))))
    (puthash path value (projtree-expanded-paths self))))

(defun projtree->display (self buffer)
  "Displays SELF in BUFFER."
  ;; Change the default-directory of the project tree buffer to make git status
  ;; execute in the right context.
  (with-current-buffer buffer
    (setq-local default-directory (projtree->root self)))
  ;; Make sure path to visited file is unfolded in project tree.
  (let ((selected-path (projtree->selected-path self)))
    (when selected-path
      (projtree->expand-to-root self selected-path)))
  ;; Display project tree in project tree buffer.
  (projtree->_display-tree self buffer)
  ;; Highlight visited file in project tree buffer.
  (projtree->_highlight-selected-path self buffer)
  ;; Add extra project tree buffer key bindings on top of existing ones.
  (with-current-buffer buffer
    (use-local-map (make-composed-keymap projtree-buffer-map
                                         (current-local-map))))
  ;; Move cursor to saved position (if any).
  (when (projtree->cursor self)
    (with-current-buffer buffer
      (set-window-point (get-buffer-window buffer) (projtree->cursor self))))
  (with-current-buffer buffer
      (setq-local mode-line-format (list "%e" mode-line-front-space "projtree: " (file-name-nondirectory (projtree->root self))))))


(autoload 'projtree-git--status "projtree-git")

(defun projtree->_git-statuses (self)
  (if projtree-show-git-status
      (projtree-git--status (projtree->root self))
    nil))

(defun projtree->_git-status (self git-statuses path)
  (let ((path-status (gethash path git-statuses))
        (root-dir (projtree->root self)))
    (if path-status
        path-status
      ;; If no git status is found for path, check to see if any ancestor
      ;; directory is ignored/untracked.
      (let ((ignored/untracked-ancestor (cl-find-if (lambda (ancestor-path)
                                                     (let ((status (gethash ancestor-path git-statuses)))
                                                       (or (equal status "!!")
                                                           (equal status "??"))))
                                                    (butlast (projtree--ancestor-paths root-dir path)))))
        (if ignored/untracked-ancestor
            (gethash ignored/untracked-ancestor git-statuses)
          nil)))))


(defun projtree->_display-tree (self buffer)
  "Display project tree SELF in the given BUFFER.
Overwrites any prior BUFFER content."
  (let ((proj-hierarchy (projtree->_build-hierarchy self))
        (git-statuses (projtree->_git-statuses self)))
    (hierarchy-tabulated-display
     proj-hierarchy
     (hierarchy-labelfn-indent
      (hierarchy-labelfn-button
       ;; labelfn
       (lambda (path indent)
         (projtree->_render-tree-entry self path git-statuses))
       ;; actionfn
       (lambda (path indent)
         ;; Remember cursor position in project tree buffer.
         (projtree->set-cursor self (point))
         (if (file-directory-p path)
             (progn
               (projtree->toggle-expand self path)
               (projtree->display self buffer))
           ;; Open clicked file and switch to that buffer.
           (let ((opened-buf (find-file-noselect path)))
             (display-buffer-use-some-window opened-buf
                                           (list (cons 'inhibit-same-window t)))
             (switch-to-buffer opened-buf))))))
     buffer)
    ;; Change the project tree buffer title from "Item name" to "Project tree".
    (with-current-buffer buffer
      (setq-local tabulated-list-format (vector '("Project tree" 0 nil)))
      (tabulated-list-init-header))))

(defun projtree->_render-tree-entry (self path &optional git-statuses)
  (let* ((filename (file-name-nondirectory path))
         (is-dir (file-directory-p path))
         (git-status (when git-statuses (projtree->_git-status self git-statuses path)))
         (git-face (when git-status (projtree-git--status-face git-status))))
    (if is-dir
        ;; Directory.
        (progn
          (let ((expand-symbol (if (projtree->expanded-p self path) "-" "+")))
            (insert (propertize expand-symbol 'face 'projtree-dir))
            (insert (propertize " " 'face 'default))
            (insert (propertize filename 'face (or git-face 'projtree-dir)))))
      ;; Regular file.
      (insert (propertize " " 'face 'default))
      (insert (propertize filename 'face (or git-face 'projtree-file))))))

(defvar projtree--hl-overlay nil)

(defun projtree->_highlight-selected-path (self buffer)
  (let ((selected-path (projtree->selected-path self)))
    ;; First clear any old highlight overlay.
    (when projtree--hl-overlay
      (delete-overlay projtree--hl-overlay))
    ;; Then produce a new highlight overlay for the visited file.
    (when selected-path
      (projtree--highlight-file selected-path buffer))))

(defun projtree--highlight-file (path buffer)
  "Highlight a certain PATH in BUFFER."
  (with-current-buffer buffer
    ;; Note: be defensive (when-let). Visited path may have been deleted.
    (when-let ((selected-linum (cl-position path (mapcar #'car tabulated-list-entries) :test #'equal)))
      (projtree--highlight-row (+ selected-linum 1) buffer))))

(defun projtree--highlight-row (line-number buffer)
  "Highlight a certain LINE-NUMBER in BUFFER."
  (with-current-buffer buffer
    (goto-line line-number)
    (let* ((start (line-beginning-position))
           (end (line-end-position))
           (hl-overlay (make-overlay start (+ 1 end))))
      (overlay-put hl-overlay 'face 'projtree-highlight)
      (overlay-put hl-overlay 'before-string (propertize "X" 'display (list 'left-fringe 'right-triangle)))
      (setq projtree--hl-overlay hl-overlay)
      ;; Move point to highlighted row in in project tree buffer window.
      (set-window-point (get-buffer-window (current-buffer)) start))))


(defun projtree->_expand-status-symbol (self path)
  (if (projtree->expanded-p self path)
      "-"
    "+"))

(defun projtree->_children-fn (self)
  "Return a function for SELF that lists child files for expanded directories."
  (lambda (folder)
    (if (and (file-directory-p folder)
             (projtree->expanded-p self folder))
        ;; Ignore "." and ".." and sort on (1) directory (2) name.
        (let ((files (directory-files folder t)))
          (sort (seq-filter (lambda (f)
                              (not (or (equal (file-name-nondirectory f) ".")
                                       (equal (file-name-nondirectory f) ".."))))
                            files)
                (lambda (f1 f2)
                  (if (xor (file-directory-p f1)
                           (file-directory-p f2))
                      (file-directory-p f1)
                    (string< f1 f2)))))
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
           ;; Note: replace `project-find-functions' for the duration of the
           ;; call to `project-current' to make it also recognize project roots
           ;; in the `project-list-file'.
           (project-find-functions (projtree--project-find-functions))
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



(defun projtree-open ()
  "Open a buffer that displays a project tree rooted at the current project.
The currently visited project file (if any) is highlighted."
  (interactive)
  (let ((projtree (projtree--current)))
    (when projtree
      (projtree->display projtree (projtree--get-projtree-buffer)))))

(defun projtree-close ()
  (interactive)
  (let ((buf (get-buffer projtree-buffer-name)))
    (when buf
      (kill-buffer buf))))

(defun projtree--get-projtree-buffer ()
  (let ((buf (get-buffer-create projtree-buffer-name)))
    (display-buffer-in-side-window buf `((side . ,projtree-window-placement) (window-width . ,projtree-window-width) (dedicated . t)))
    (let ((win (get-buffer-window buf)))
      ;; Make window dedicated to projtree buffer.
      (set-window-dedicated-p win t)
      ;; Make C-x 1 not close the window.
      (set-window-parameter win 'no-delete-other-windows t)
      ;; Frame resizing should not affect the size of the project tree window.
      (window-preserve-size win t t))
    buf))


(defun projtree--abspath (path)
  "Return a normalized PATH (absolute and no trailing slash)."
  (string-trim-right (expand-file-name path) "/"))


(defun projtree--call-async (fn)
  "Call function FN asynchronously."
  (run-with-idle-timer 0 nil fn))


(defvar projtree--visited-buffer nil
  "Tracks the currently visited buffer in `projtree-mode'.")


(defun projtree--file-visiting-buffer-p (buffer)
  "Return t for a BUFFER that is visiting an existing file, nil otherwise."
  (let ((buffer-file (buffer-file-name buffer)))
    ;; Check both that the buffer visits a file and that the file exists. For a
    ;; newly created buffer, the file may not yet exist on the filesystem.
    (and buffer-file
         (file-exists-p buffer-file))))


(defun projtree--render-on-buffer-switch ()
  "A hook function intended to be invoked on buffer switch."
  
  ;; No need to re-render project tree if the current buffer has not changed
  ;; with this window change.
  (unless (equal (current-buffer) (window-old-buffer))
    (let ((curr-buf (current-buffer)))
      ;; No need to re-render project tree if we're in a buffer not visiting a
      ;; file (the mini-buffer, the project tree buffer, etc).
      (when (projtree--file-visiting-buffer-p curr-buf)
        (projtree--set-visited-buffer curr-buf)
        ;; We want follow-mode when switching to a file buffer (and cursor has
        ;; left project tree).
        (projtree--forget-cursor)
        (projtree--call-async #'projtree-open)))))


(defun projtree--forget-cursor ()
  (when-let ((projtree (projtree--current)))
    (projtree->set-cursor projtree nil)))

(defun projtree--set-visited-buffer (buffer)
  (setq projtree--visited-buffer buffer)
  ;; Also mark path as selected in current project tree.
  (let ((projtree (projtree--current))
        (visited-file (buffer-file-name buffer)))
    (when (and projtree visited-file)
      (projtree->set-selected-path projtree visited-file))))

(defun projtree--try-project-list (dir)
  "Find a project containing DIR from the `project-list-file'."
  (catch :root
    (dolist (it (project-known-project-roots))
      (when (and (string-prefix-p it dir)
                 (file-directory-p it))
        (throw :root (cons 'transient it))))))

(defun projtree--project-find-functions ()
  "Return a wsp replacement for `project-find-functions'.

It extends the built-in `project-find-functions' list (whose
functions are called to find the project containing a given
directory) which by default only finds projects that are
version-controlled (through `project-try-vc').  We add a function
to also look for project directories that are listed in the
`project-list-file'.  When using this with `project-current' a
projtree will be correctly rendered also for non-git projects."
  (append '(projtree--try-project-list) project-find-functions))


(autoload 'projtree-profiling-enable "projtree-profiling")
(autoload 'projtree-profiling-disable "projtree-profiling")

;;;###autoload
(define-minor-mode projtree-mode
  "A minor mode that displays a project file explorer for the visited file."
  :lighter nil ;; Do not display on mode-line.
  (if projtree-mode
      (progn
        (when projtree-profiling-enabled
          (projtree-profiling-enable))
        (add-hook 'window-configuration-change-hook #'projtree--render-on-buffer-switch)
        (projtree--set-visited-buffer (current-buffer))
        (projtree-open))
    (remove-hook 'window-configuration-change-hook #'projtree--render-on-buffer-switch)
    (projtree-close)
    (when projtree-profiling-enabled
      (projtree-profiling-disable))))



(provide 'projtree)

;;; projtree.el ends here.
