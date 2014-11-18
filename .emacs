(load (expand-file-name (concat (getenv "HOME") "/.emacs.d/init")))
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
