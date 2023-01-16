# projtree

`projtree-mode` is an Emacs minor mode that shows a file explorer (the
`*projtree*` buffer) for the code tree of the _current project_ (as determined
by `project.el`). `projtree-mode` follows the active buffer, meaning that it
always displays the file tree rooted at the directory root folder of the
currently visited file.

`projtree` is similar in spirit to the excellent
[treemacs](https://github.com/Alexander-Miller/treemacs/) but smaller in scope.

It builds on standard Emacs and relies on `hierarchy.el` to render the tree. For
git status highlighting, the `git` command to be present on the system.

## Features

- The project tree always displays the project tree of the active buffer.
- The file of the visited buffer is highlighted in the tree.
- The git status of files/directories is highlighted in the tree.
- Files can be opened from the project tree.

- `postrace-push`: pushes the buffer position (marker) at point to the position
  stack.

TODO

## Screenshots

![TODO](screenshots/TODO.gif)

## Install

- Via `use-package`:

  ```emacs-lisp
  (use-package projtree
   :ensure t
   :commands (projtree-mode))
  ```

- Via `use-package` and `straight.el`:

  ```emacs-lisp
  (use-package projtree
    :straight (emacs-projtree :type git :host github
                  :repo "petergardfjall/emacs-projtree")
    :commands (projtree-mode))
  ```

- By adding `projtree.el` to your `~/.emacs.d/lisp/`:

  ```emacs-lisp
  (add-to-list 'load-path "~/.emacs.d/lisp/")
  (require 'projtree)
  ```

After getting `projtree.el` onto your Emacs load-path just issue the
`M-x projtree-mode` command.

## Customization
