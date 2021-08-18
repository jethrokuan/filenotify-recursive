# Filenotify-recursive

Like `filenotify`, but recursive.

## Usage

``` emacs-lisp
;; To start watching a directory recursively
(fnr-add-watch "my-dir" '(change) #'my-callback) ;; => uuid

;; To cancel the watch
(fnr-rm-watch uuid)
```
