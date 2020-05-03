;;; mugu-buffer --- Provide tools for buffer management -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'dash)
(require 'ivy)

(defvar mugu-buffer-before-switch-functions nil
  "A list of function called before `mugu-buffer-switch' display the new buffer.
Function should accept one argument: the buffer which will be switched too.")
(defvar mugu-buffer-after-switch-hook nil
  "A hook run after `mugu-buffer-switch' has loaded the new buffer.")

(defun mugu-buffer--select-buffer ()
  "Select a buffer through ivy and return it.
The buffer is not displayed.
The part where virtual buffer is retrieved is hacky but... ivy provides no
binding for it and it is a really usefull feature but reimplementing it would be
pointless so..."
  (let* ((ivy-inhibit-action t)
         (buffer-name (substring-no-properties (ivy-switch-buffer)))
         (buffer-virtual (assoc buffer-name ivy--virtual-buffers)))
    (or (get-buffer buffer-name)
        (find-file-noselect (cdr buffer-virtual)))))

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
