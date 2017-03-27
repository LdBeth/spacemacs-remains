(defvar maxima-install-path nil
  "Where the maxima/{version}/emacs is.")

(when maxima-install-path
  (push maxima-install-path load-path))
