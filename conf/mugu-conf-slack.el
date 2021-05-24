;;; mugu-conf-slack --- #{Summary} -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'use-package)

(use-package slack
  ;; General settings
  :commands (slack-start mugu-feature-slack)
  :custom
  (slack-buffer-emojify t)
  (slack-prefer-current-team t)
  (slack-buffer-create-on-notify t)
  (slack-modeline-count-only-subscribed-channel nil)
  (slack-enable-wysiwyg t)
  (slack-render-image-p t)
  (slack-typing-visibility 'buffer)
  (slack-file-dir "~/Downloads/")
  (slack-enable-global-mode-string t))

(use-package slack
  ;; Global bindings
  :defer
  :general
  ([remap mugu-feature-slack] #'mugu-slack-menu/body)
  :pretty-hydra
  (mugu-slack-menu
   (:color blue :hint nil)
   ("Select room"
    (("sl" tracking-next-buffer "next unread")
     ("sr" slack-select-rooms "any room")
     ("ss" slack-select-unread-rooms "unread"))
    "Select specific channel"
    (("si" slack-im-select "instant message")
     ("sg" slack-group-select "group chat")
     ("sc" slack-channel-select "channel chat"))
    "Select things"
    (("su" slack-user-select "user")
     ("sf" slack-file-list "files")
     ("sf" slack-stars-list "stars"))
    "Upload things"
    (("ui" slack-clipboard-image-upload "image" :color blue)
     ("uf" slack-file-upload "image" :color blue)
     ("us" slack-file-upload-snippet "snippet" :color blue))
    "Global"
    (("ct" slack-change-current-team "change team")
     ("r" slack-conversations-list-update "change team")))))

(use-package slack
  ;; Local message major mode bindings
  :defer
  :general
  (:keymaps '(slack-mode-map) :states '(insert)
            "C-<return>" #'newline
            "S-<return>" #'newline)
  :general
  (:keymaps '(slack-thread-message-buffer-mode-map)
            [remap mugu-menu-call-mode-menu] #'major-mode-hydras/slack-message-buffer-mode/body)
  :mode-hydra
  (slack-message-buffer-mode
   (:title "slack" :color red :hint nil)
   ("Select room"
    (("sl" tracking-next-buffer "next unread")
     ("sr" slack-select-rooms "any room")
     ("ss" slack-select-unread-rooms "unread"))
    "Navigation"
    (("j" slack-buffer-goto-next-message "↓")
     ("k" slack-buffer-goto-prev-message "↓")
     ("gg" slack-buffer-goto-first-message "first")
     ("G" slack-buffer-goto-last-message "last"))
    "Upload"
    (("ui" slack-clipboard-image-upload "image" :color blue)
     ("uf" slack-file-upload "image" :color blue))
    "Message"
    (("md" slack-message-delete "delete")
     ("me" slack-message-edit "edit")
     ("ra" slack-message-add-reaction "add reaction")
     ("rd" slack-message-remove-reaction "remove reaction")))))

(use-package alert
  :defer
  :commands (alert)
  :custom
  (alert-fade-time 2)
  (alert-reveal-time 2)
  (alert-default-style 'libnotify))

(use-package lui
  :defer
  :straight nil
  :custom
 '(lui-time-stamp-position 'right-margin)
  (lui-fill-type " ")
  (lui-track-indicator 'fringe)
  (lui-fill-column 100)
  (lui-flyspell-p t))

(use-package mugu-slack-fill
  :straight nil
  :custom
  (mugu-slack-fill-column 100)
  :hook
  (slack-message-buffer-mode . mugu-slack-fill-mode)
  (slack-message-buffer-mode . guess-language-mode))

(use-package mugu-slack-wconf
  :straight nil
  :after slack
  :config
  (mugu-slack-wconf-mode))

(use-package mugu-slack-wconf-tab
  :straight nil
  :after (slack mugu-tab)
  :config
  (mugu-slack-wconf-tab-mode))

(use-package mugu-slack-ext
  :straight nil
  :after slack
  :demand
  :general
  (:keymaps '(mugu-slack-ext-mode-map)
            [remap tracking-next-buffer] #'mugu-slack-ext-next-unread
            [remap slack-select-unread-rooms] #'mugu-slack-ext-select-unread)
  :config
  (mugu-slack-ext-mode))

(use-package mugu-slack-link
  :straight nil
  :after (slack link-hint)
  :hook
  (slack-message-buffer-mode . mugu-slack-link-mode)
  (slack-file-info-buffer-mode . mugu-slack-link-mode))

(provide 'mugu-conf-slack)
;;; mugu-conf-slack ends here
