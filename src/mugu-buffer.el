;;; mugu-buffer --- Provide tools for buffer management -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'dash)
(require 'ivy)


(defun mugu-buffer-switch (buffer)
  "Switch to a BUFFER but respect `display-buffer-alist' rules and provide hook.
Call `mugu-buffer-before-switch-functions' just before displaying new buffer.
The new buffer will be displayed in a window according to `display-buffer-alist'.
This means the current window may change after the buffer has switched."
  (interactive (list (mugu-buffer--select-buffer)))
  (run-hook-with-args 'mugu-buffer-before-switch-functions buffer)
  (display-buffer buffer)
  (select-window (get-buffer-window buffer))
  (run-hooks 'mugu-buffer-after-switch-hook))

(provide 'mugu-buffer)
;;; mugu-buffer ends here
