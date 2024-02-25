;;; projtree.el --- Display project directory tree of visited file -*- lexical-binding: t -*-
;;
;; Copyright © 2023 Peter Gardfjäll <peter.gardfjall.work@gmail.com>
;;
;; Author: Peter Gardfjäll <peter.gardfjall.work@gmail.com>
;; URL: https://github.com/petergardfjall/emacs-projtree
;; Keywords: workspace, project
;; Package-Requires: ((emacs "27.0"))
;; Version: 0.0.1
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
;;
;;; Commentary:
;;
;; `projtree-mode' is an Emacs minor mode that shows a file explorer for the
;; currently visited file in a side window with a `*projtree*' buffer.  The
;; project of the visited file is determined by `project.el'.  `projtree-mode'
;; follows the active buffer, meaning that it always displays the file tree
;; rooted at the directory root folder of the currently visited file.
;;
;;; Code:
(defvar projtree-profiling--on nil)

(defun projtree-profiling--new-fn-timer (fn-name)
  "Create an advice function that times the execution of calls to FN-NAME."
  (lambda (fn &rest args)
    (let ((start-time (current-time))
          (result (apply fn args)))
      (message "%s completed in %.2f ms" fn-name (* 1000 (float-time (time-subtract (current-time) start-time))))
      result)))

(defvar projtree-profiling--timing-fn-alist
  `((projtree->display . ,(projtree-profiling--new-fn-timer "projtree->display"))
    (projtree->_display-tree . ,(projtree-profiling--new-fn-timer "projtree->_display-tree"))
    (projtree->_highlight-selected-path . ,(projtree-profiling--new-fn-timer "projtree->_highlight-selected-path"))
    (projtree-git--status . ,(projtree-profiling--new-fn-timer "projtree-git--status"))))

;;;###autoload
(defun projtree-profiling-toggle ()
  (if projtree-profiling--on
      (projtree-profiling-enable)
    (projtree-profiling-disable))
  (setq projtree-profiling--on (not projtree-profiling--on)))

;;;###autoload
(defun projtree-profiling-enable ()
  (dolist (it projtree-profiling--timing-fn-alist)
    (let ((fn (car it))
          (advice-fn (cdr it)))
      (message "advicing %s ..." fn)
      (advice-add fn :around advice-fn))))

;;;###autoload
(defun projtree-profiling-disable ()
  (dolist (it projtree-profiling--timing-fn-alist)
    (let ((fn (car it))
          (advice-fn (cdr it)))
      (advice-remove fn advice-fn))))

(provide 'projtree-profiling)

;;; projtree-profiling.el ends here
