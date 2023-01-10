;;; projtree.el --- Display project directory tree of visited file -*- lexical-binding: t -*-

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
