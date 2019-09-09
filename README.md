g/christensen's technical blog

test

```emacs
;; get the destination org file path specified in Ubiquity
(defun capture-get-destination-file ()
  ;; `capture-decoded-org-protocol-query' global variable contains
  ;; org-protocol url query parameters, stored earlier (see below)
  (plist-get capture-decoded-org-protocol-query :file))
```