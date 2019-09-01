;;; mugu-dumbjump --- wrapper and utility to dumpjump package -*- lexical-binding: t -*-
;;; Commentary:
(require 'dumb-jump)

;;; Code:
(defmacro with-dump-jump-fallback (&rest jump_body)
  "Eval JUMP_BODY and default to `dumb-jump-go' if it failed."
  `(let ((before-jump-point (point)))
     (ignore-errors (progn ,@jump_body))
     (when (eq (point) before-jump-point)
       (dumb-jump-go))))

(provide 'mugu-dumbjump)
;;; mugu-dumbjump ends here
