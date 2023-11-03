(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu services)
             (gnu packages admin)
             (guix gexp))


(home-environment
 (packages (list htop
                 emacs
                 emacs-use-package
                 emacs-guix
                 guile
                 guix
                 emacs-no-littering
                 emacs-which-key
                 emacs-smartparens
                 emacs-undo-tree
                 emacs-color-theme-modern
                 emacs-emojify
                 emacs-all-the-icons
                 emacs-minions
                 emacs-doom-modeline
                 emacs-diminish
                 emacs-alert
                 emacs-pinentry
                 pinentry-emacs
                 emacs-helpful
                 emacs-hydra
                 emacs-vertico
                 emacs-orderless
                 emacs-corfu
                 emacs-consult
                 emacs-marginalia
                 emacs-embark
                 emacs-avy
                 emacs-expand-region
                 emacs-parinfer-mode
                 emacs-origami-el
                 emacs-pass
                 emacs-auth-source-pass
                 emacs-dirvish
                 emacs-fd
                 poppler
                 ffmpegthumbnailer
                 mediainfo
                 unzip
                 tar
                 emacs-openwith
                 emacs-rg
                 emacs-edit-server
                 emacs-org
                 imagemagick
                 emacs-org-roam
                 emacs-ox-gfm
                 emacs-eglot
                 emacs-consult-eglot
                 emacs-dap-mode
                 emacs-company
                 emacs-magit
                 emacs-magit-todos
                 git
                 git:send-email
                 emacs-projectile
                 emacs-counsel-projectile
                 ccls
                 emacs-ccls
                 emacs-paredit
                 emacs-geiser
                 emacs-geiser-guile
                 emacs-markdown-mode
                 emacs-markdown-preview-mode
                 emacs-web-mode
                 emacs-yaml-mode
                 python-lsp-server
                 emacs-flycheck
                 emacs-yasnippet
                 emacs-yasnippet-snippets
                 emacs-smartparens
                 emacs-rainbow-delimiters
                 emacs-rainbow-mode
                 emacs-csv
                 emacs-csv-mode
                 emacs-multi-term
                 emacs-vterm
                 emacs-multi-vterm
                 emacs-vterm-toggle
                 emacs-eshell-up
                 emacs-eshell-toggle
                 emacs-eshell-syntax-highlighting
                 emacs-eshell-prompt-extras
                 emacs-eshell-did-you-mean
                 emacs-eshell-bookmark
                 emacs-mu4e-alert
                 emacs-org-mime
                 emacs-pdf-tools
                 plantuml
                 emacs-plantuml-mode
                 emacs-guix
                 emacs-daemons
                 emacs-dockerfile-mode
                 emacs-key-chord
                 emacs-buffer-env
                 emacs-inheritenv
                 ))
 (services
  (list
   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (bash-profile (list (plain-file "bash-profile" "\
export HISTFILE=$XDG_CACHE_HOME/.bash_history")))))

   (simple-service 'test-config
                   home-xdg-configuration-files-service-type
                   (list `("test.conf"
                           ,(plain-file "tmp-file.txt"
                                        "the content of
                                          ~/.config/test.conf")))))))
