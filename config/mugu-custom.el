(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-output-view-style
   (quote
    (("^dvi$"
      ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$")
      "%(o?)dvips -t landscape %d -o && gv %f")
     ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f")
     ("^dvi$"
      ("^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "^landscape$")
      "%(o?)xdvi %dS -paper a4r -s 0 %d")
     ("^dvi$" "^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "%(o?)xdvi %dS -paper a4 %d")
     ("^dvi$"
      ("^\\(?:a5\\(?:comb\\|paper\\)\\)$" "^landscape$")
      "%(o?)xdvi %dS -paper a5r -s 0 %d")
     ("^dvi$" "^\\(?:a5\\(?:comb\\|paper\\)\\)$" "%(o?)xdvi %dS -paper a5 %d")
     ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d")
     ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d")
     ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d")
     ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d")
     ("^dvi$" "." "%(o?)xdvi %dS %d")
     ("^pdf$" "." "zathura %o --page %(outpage)")
     ("^html?$" "." "netscape %o"))))
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#c5c8c6" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#1d1f21"))
 '(ansi-term-color-vector
   [unspecified "#14191f" "#d15120" "#81af34" "#deae3e" "#7e9fc9" "#a878b5" "#7e9fc9" "#dcdddd"] t)
 '(backup-by-copying-when-mismatch nil)
 '(column-number-mode t)
 '(company-auto-complete t)
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "f0d8af755039aa25cd0792ace9002ba885fd14ac8e8807388ab00ec84c9497d7" "9b402e9e8f62024b2e7f516465b63a4927028a7055392290600b776e4a5b9905" "cb18233197cedab557c70d171b511bed49cc702f428750925280090c31498bd2" "1edf370d2840c0bf4c031a044f3f500731b41a3fd96b02e4c257522c7457882e" "91fba9a99f7b64390e1f56319c3dbbaed22de1b9676b3c73d935bf62277b799c" "d1a42ed39a15a843cccadf107ee0242b5f78bfbb5b70ba3ce19f3ea9fda8f52d" "b6db49cec08652adf1ff2341ce32c7303be313b0de38c621676122f255ee46db" "a7b47876e5da7cac6f5e61cca7a040a365ca2c498823654bd4076add8edf34c5" "aed73c6d0afcf2232bb25ed2d872c7a1c4f1bda6759f84afc24de6a1aec93da8" "d5aac94c0051c3acec2b274347b343372b4e64c3e226be7b7c56725ea26b1ba8" "0bd7a42bd443517e5e61dac3cabc24018fbd0c6b2b4199b3c4efd9e3727efd30" "d69a0f6d860eeff5ca5f229d0373690782a99aee2410a3eed8a31332a7101f1e" "930227e22122d1881db7c2c1ae712dcf715697a1c4d9864f8107a2c3c2da9f8b" "b1bcb837df0455af8e91114b7a3bddfa084cde32ceb16b1b468d5e5e8605a835" "2162da67ce86c514aff010de1b040fb26663ca42afebc2de26515d741121c435" "e1551b5516e0a439b6ab019ba00cee866e735f66f22ff67a5d882ad0f1383454" "50e7f9d112e821e42bd2b8410d50de966c35c7434dec12ddea99cb05dd368dd8" "03e3e79fb2b344e41a7df897818b7969ca51a15a67dc0c30ebbdeb9ea2cd4492" "e254f8e18ba82e55572c5e18f3ac9c2bd6728a7e500f6cc216e0c6f6f8ea7003" "43aeadb0c8634a9b2f981ed096b3c7823c511d507a51c604e4667becb5ef6e35" "101a10b15bbbd0d5a0e56e4773e614962197886780afb2d62523a63a144ad96c" "c70cc9c4c6257d70f5c11b90cb9e8b1e54e6edd6aa43f39879746e16a70533f5" "aa87469691932ff791f966bffb885ecd97ebfa4dc4d42e479f3819ac4a3fbcaf" "9f6750057fefba39c184783c7b80ddd9c63bc6e8064846b423b4362c9e930404" "06fc6014871028d24b4e03db24b9efee48bd73dce0afdc15e9124f09fab64afa" "890d09dcc8d2326e98eee74b307b2cc42f07ab7701bcff521e6152aa3e08f7a8" "9e87bddff84cbce28c01fa05eb22f986d770628fa202cd5ca0cd7ed53db2f068" "b4ec581daad15aa7020b722523dc6bcea850bfbdbe31bfeb11c45ea51899bd75" "e8e744a1b0726814ac3ab86ad5ccdf658b9ff1c5a63c4dc23841007874044d4a" "f2503f0a035c2122984e90eb184185769ee665de5864edc19b339856942d2d2d" "e8bba3c8e8caea2c7a8b6932b0db8d9bdb468c9b44bf554f37b56093d23fde57" "f245c9f24b609b00441a6a336bcc556fe38a6b24bfc0ca4aedd4fe23d858ba31" "0b6645497e51d80eda1d337d6cabe31814d6c381e69491931a688836c16137ed" "76bd62f6ce376bf0597fab7f478eaa98cd94a7b41f0ae46de63a958fbe99c1d9" "09669536b4a71f409e7e2fd56609cd7f0dff2850d4cbfb43916cc1843c463b80" "294834baa9ca874795a3181cce7aaf228b1e3fb3899587ffd3ae7546de328c90" "f21caace402180ab3dc5157d2bb843c4daafbe64aadc362c9f4558ac17ce43a2" "8e3f020f1ce69cfa0c1ebee4e198feb28dd7eb31b7d77927e9c790819210c654" "e033c4abd259afac2475abd9545f2099a567eb0e5ec4d1ed13567a77c1919f8f" "db9feb330fd7cb170b01b8c3c6ecdc5179fc321f1a4824da6c53609b033b2810" "59ca830d4df5e79503b79103485d28c6a578ca14d526ffc6a43596808daf1282" "16d6e7f87846801e17e0c8abc331cf6fa55bec73185a86a431aca6bec5d28a0a" "6916fa929b497ab630e23f2a4785b3b72ce9877640ae52088c65c00f8897d67f" "e681c4fc684a543ce97c2d55082c6585182c0089f605dc9a5fe193870f03edc6" "82cbb553a225b75ee49901fa06562941fbfe5e6fed24cda985e7ea59af7ddc80" "ad68cb14359254795c6b96d76334aaacb739c04f64a4a8567964d4a20aa723d2" "1462969067f2ff901993b313085d47e16badeec58b63b9ed67fa660cebaaddae" "e2e4e109357cfcebccb17961950da6b84f72187ade0920a4494013489df648fe" "75c0b9f9f90d95ac03f8647c75a91ec68437c12ff598e2abb22418cd4b255af0" "bf81a86f9cfa079a7bb9841bc6ecf9a2e8999b85e4ae1a4d0138975921315713" "e24679edfdea016519c0e2d4a5e57157a11f928b7ef4361d00c23a7fe54b8e01" "d43120398682953ef18fd7e11e69c94e44d39bb2ab450c4e64815311542acbff" "cdfb22711f64d0e665f40b2607879fcf2607764b2b70d672ddaa26d2da13049f" "3fb38c0c32f0b8ea93170be4d33631c607c60c709a546cb6199659e6308aedf7" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "c7fb35ba0e1e7f2e4b48ba1508ce5ee309192c6e5e671dba296dc259844426e6" "3625c04fa4b8a802e96922d2db3f48c9cb2f93526e1dc24ba0b400e4ee4ccd8a" "ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "b1471d88b39cad028bd621ae7ae1e8e3e3fca2c973f0dfe3fd6658c194a542ff" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "9370aeac615012366188359cb05011aea721c73e1cb194798bc18576025cabeb" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "57072d797dc09fcf563051a85a29d6a51d6f2b1a602e029c35b05c30df319b2a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "8eef22cd6c122530722104b7c82bc8cdbb690a4ccdd95c5ceec4f3efa5d654f5" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(edt-bottom-scroll-margin 0)
 '(edt-top-scroll-margin 0)
 '(electric-pair-mode t)
 '(ess-swv-pdflatex-commands (quote ("texi2pdf" "pdflatex" "make" "xelatex")))
 '(evil-ex-search-vim-style-regexp t)
 '(evil-insert-state-modes
   (quote
    (comint-mode erc-mode eshell-mode geiser-repl-mode gud-mode inferior-apl-mode inferior-caml-mode inferior-emacs-lisp-mode inferior-j-mode inferior-python-mode inferior-scheme-mode inferior-sml-mode internal-ange-ftp-mode prolog-inferior-mode reb-mode shell-mode slime-repl-mode term-mode wdired-mode)))
 '(evil-motion-state-modes
   (quote
    (apropos-mode Buffer-menu-mode calendar-mode color-theme-mode command-history-mode compilation-mode dictionary-mode ert-results-mode help-mode Man-mode speedbar-mode undo-tree-visualizer-mode view-mode woman-mode)))
 '(evil-overriding-maps
   (quote
    ((Buffer-menu-mode-map)
     (color-theme-mode-map)
     (comint-mode-map)
     (compilation-mode-map)
     (grep-mode-map)
     (dictionary-mode-map)
     (ert-results-mode-map . motion)
     (Info-mode-map . motion)
     (speedbar-key-map)
     (speedbar-file-key-map)
     (speedbar-buffers-key-map))) nil nil "todo?")
 '(evil-search-module (quote evil-search))
 '(evil-symbol-word-search t)
 '(evil-toggle-key "C-z" nil nil "maybe to do")
 '(evil-vsplit-window-right t)
 '(fci-rule-character-color "#192028")
 '(fci-rule-color "#282a2e")
 '(fill-column 80)
 '(flycheck-clang-language-standard "c++11")
 '(flycheck-haskell-ghc-executable "/usr/bin/ghc")
 '(flycheck-haskell-hlint-executable "/usr/bin/cabal exec hlint")
 '(gdb-many-windows t)
 '(global-hl-line-mode t)
 '(global-semantic-decoration-mode t)
 '(global-semantic-highlight-func-mode t)
 '(global-semantic-idle-summary-mode t)
 '(helm-autoresize-max-height 70)
 '(helm-autoresize-min-height 20)
 '(helm-autoresize-mode t)
 '(ibuffer-saved-filter-groups
   (quote
    (("groups"
      ("nasa-filer"
       (filename . "/home/nathan/devel/nasa/filer/")))
     ("default"
      ("dired"
       (mode . dired-mode))
      ("haskell"
       (mode . haskell-mode))
      ("python"
       (mode . python-mode))
      ("notes"
       (or
        (name . "^\\*Calendar\\*$")
        (name . "^diary$")
        (mode . org-mode)))
      ("*buffer*"
       (name . "\\*.*\\*"))))))
 '(ibuffer-saved-filters
   (quote
    (("builtin-buffers"
      ((name . "^\\*.*\\*$")))
     ("nasa-filer"
      ((filename . "/home/nathan/devel/nasa/filer/")))
     ("gnus"
      ((or
        (mode . message-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode))))
     ("programming"
      ((or
        (mode . emacs-lisp-mode)
        (mode . cperl-mode)
        (mode . c-mode)
        (mode . java-mode)
        (mode . idl-mode)
        (mode . lisp-mode)))))))
 '(ido-everywhere t)
 '(indent-tabs-mode nil)
 '(kill-do-not-save-duplicates t)
 '(kill-ring-max 200)
 '(latex-run-command "pdflatex")
 '(mark-ring-max 100)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files
   (quote
    ("~/newMaster.org" "~/mugu-ft1.org" "~/mugu-ft2.org")))
 '(org-columns-ellipses "…")
 '(org-ellipsis "…")
 '(org-footnote-auto-adjust t)
 '(org-footnote-auto-label nil)
 '(org-startup-folded (quote content))
 '(org-startup-truncated nil)
 '(org-use-sub-superscripts (quote {}))
 '(powerline-evil-tag-style (quote verbose))
 '(preview-TeX-style-dir "/home/nathan/.emacs.d/elpa/auctex-11.87.2/latex")
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset 0)
 '(semantic-mode t)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(tab-always-indent (quote complete))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(use-file-dialog nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(verilog-auto-delete-trailing-whitespace t)
 '(verilog-auto-lineup (quote all))
 '(verilog-auto-newline nil)
 '(verilog-case-indent 4)
 '(verilog-cexp-indent 4)
 '(verilog-indent-begin-after-if nil)
 '(verilog-indent-level 4)
 '(verilog-indent-level-behavioral 4)
 '(verilog-indent-level-declaration 4)
 '(verilog-indent-level-module 4)
 '(whitespace-line-column 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono for Powerline" :foundry "outline" :slant normal :weight normal :height 83 :width normal))))
 '(dired-directory ((t (:inherit font-lock-function-name-face :weight bold))))
 '(flycheck-info ((t (:underline nil))))
 '(flycheck-warning ((t (:underline nil))))
 '(font-lock-string-face ((t (:foreground "#CC9393"))))
 '(helm-selection ((t (:background "#3e4446" :underline nil))))
 '(helm-source-header ((t nil)))
 '(helm-swoop-target-line-block-face ((t (:inherit helm-swoop-target-line-face))))
 '(helm-swoop-target-line-face ((t (:background "#2B2B2B" :foreground "#F0DFAF"))))
 '(helm-swoop-target-word-face ((t (:background "#2B2B2B" :foreground "#F0DFAF"))))
 '(highlight-indentation-face ((t (:background "#3E3E3E"))))
 '(info-title-1 ((t (:inherit info-title-2 :height 1.1))))
 '(info-title-2 ((t (:inherit info-title-3 :height 0.9))))
 '(linum ((t (:background "#4f4f4f" :foreground "#656555"))))
 '(mode-line-highlight ((t (:box nil))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#404045" :foreground "gray60" :inverse-video nil :box nil :weight light))))
 '(org-block-background ((t (:background "gray23"))))
 '(org-block-begin-line ((t (:inherit org-meta-line :background "gray21"))) t)
 '(org-block-end-line ((t (:inherit org-meta-line :background "gray21"))) t)
 '(show-paren-match ((t (:background "#4f4f4f" :weight normal))))
 '(show-paren-mismatch ((t (:background "#dca3a3" :foreground "#6f6f6f" :weight bold))))
 '(tooltip ((t (:inherit default :background "lightyellow" :foreground "black"))))
 '(trailing-whitespace ((t nil)))
 '(variable-pitch ((t (:height 1.0 :family "Courier"))))
 '(whitespace-line ((t nil))))
