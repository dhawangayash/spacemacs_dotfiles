;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs


   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(csv
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-complete-with-key-sequence '"jk"
                      auto-completion-complete-with-key-sequence-delay 0.2
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-idle-delay 0.2
                      ;; This will enable documentation.
                      ;; auto-completion-use-company-box t
                      auto-completion-enable-sort-by-usage t)
     ;; better-defaults
     ;; (bibtex
     ;;  :variable
     ;;  bibtex-completion-bibliography (expand-file-name "~/Dropbox/org/dgg_bib.bib")
     ;;  bibtex-completion-pdf-field "File"
     ;;  ;; org-ref stuff (but used by bibtex layer)
     ;;  org-ref-default-bibliography (list bibtex-completion-bibliography)
     ;;  org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
     ;;  )
     command-log
     emacs-lisp
     epub
     eww
     git
     ;; helm
     (ivy :variables ivy-enable-advanced-buffer-information t)
     html
     ibuffer
     json
     ;; lsp
     markdown
     multiple-cursors
     ;; adding org roam variables
     (org
      :variables
      org-directory (expand-file-name "~/Dropbox/org")
      org-default-notes-file (concat org-directory "/org_notes.org")
      org-enable-roam-support t
      org-enable-roam-server t
      org-enable-roam-protocol t
      org-roam-directory (concat org-directory "/roam")
      org-roam-db-location (concat org-roam-directory "/db/org-roam.db")
      org-roam-v2-ack t
      )
     pdf
     (plantuml
      :variables
      plantuml-jar-path "~/Downloads/plantuml.jar"
      org-plantuml-jar-path "~/Downloads/plantuml.jar")
     python
     ;; projectile
     shell-scripts
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     ;; speed-reading -- use phrasereader.com instead
     (sql :variables sql-capitalize-keywords t)
     ;; https://develop.spacemacs.org/layers/+checkers/spell-checking/README.html#disabling-by-default
     (spell-checking
      :variable spell-checking-enable-by-default nil)
     ;; syntax-checking
     ;; version-control
     ;; treemacs
     ;; themes-megapack
     ;; yaml
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(
                                      (atomic-chrome
                                       :ensure
                                       :config
                                       (atomic-chrome start-server))
                                      ;; https://emacs.stackexchange.com/questions/54092/spacemacs-helm-org-layer-is-missing
                                      hl-anything
                                      helm-org
                                      pyenv-mode
                                      imenu-anywhere
                                      ;;; second brain
                                      ;; org-roam-bibtex
                                      (org-pdftools :location (recipe :fetcher github :repo "fuxialexander/org-pdftools"))
                                      org-noter
                                      org-noter-pdftools
                                      ;; themes
                                      leuven-theme
                                      )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.

  ;; Fix missing ELPA Bug from startup
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default spacemacs-27.1.pdmp)
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(leuven
                         spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 10.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.2

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   ;; dotspacemacs-line-numbers 'visual
   dotspacemacs-line-numbers t

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (eval-after-load 'org
    (lambda()
      (require 'org)
      (require 'flycheck)

      ;; General config
      (setq org-startup-indented t)
      ;; Disable "ask to execute code block"  because it's annoying
      (setq org-confirm-babel-evaluate nil)))
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (spacemacs/set-leader-keys "ji" 'ivy-imenu-anywhere)
  ;; TO suppress package cl warning for Emacs 27.1
  ;; https://github.com/kiwanami/emacs-epc/issues/35
  ;; didn't work
  (setq byte-compile-warnings '(cl-functions))

  (spacemacs//add-to-load-path (expand-file-name "~/workdir/spacemacs_dotfiles/"))
  (load (expand-file-name "~/workdir/spacemacs_dotfiles/dgg.el"))
  (eval-after-load 'org
    (org-babel-load-file (expand-file-name "~/workdir/spacemacs_dotfiles/dgg-settings.org")))
  (load (expand-file-name "~/workdir/spacemacs_dotfiles/dgg-xah-functions.el"))
  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(beacon-color "#ff9da4")
 '(custom-enabled-themes nil)
 '(custom-safe-themes '(default))
 '(electric-pair-mode t)
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#6272a4")
 '(fill-column 100)
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(golden-ratio-mode nil)
 '(helm-completion-style 'emacs)
 '(hl-sexp-background-color "#efebe9")
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(jdee-db-active-breakpoint-face-colors (cons "#171F24" "#237AD3"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#171F24" "#579C4C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#171F24" "#777778"))
 '(objed-cursor-color "#ff5555")
 '(org-agenda-files
   '("/home/dgg/Dropbox/org/org-roam.bak/scratch/2021-05-12.org" "/home/dgg/Dropbox/org/org-roam.bak/20210512234800-a_beautiful_idea_to_test_this_out.org" "/home/dgg/Dropbox/org/org-roam.bak/20210512235108-another_file_for_bibtex.org" "/home/dgg/Dropbox/org/org-roam.bak/20210513000828-working_on_this_file.org" "/home/dgg/Dropbox/org/org-roam.bak/20210513001806-wow_testing_bibtex.org" "/home/dgg/Dropbox/org/org-roam.bak/20210513222926-stock_tips_from_uncle.org" "/home/dgg/Dropbox/org/org-roam.bak/20210514133326-dfs_from_geeta_algo_huddle.org" "/home/dgg/Dropbox/org/org-roam.bak/20210514141957-depth_first_search_graph_traversal.org" "/home/dgg/Dropbox/org/org-roam.bak/20210514144510-leetcode_vs_formal_approach_to_problem_solving.org" "/home/dgg/Dropbox/org/org-roam.bak/20210516184441-ediff_customization_commands.org" "/home/dgg/Dropbox/org/org-roam.bak/20210517085859-dfs_contract_and_code.org" "/home/dgg/Dropbox/org/org-roam.bak/20210521132102-count_the_number_of_bits_set_in_a_Integer_Number.org" "/home/dgg/Dropbox/org/org-roam.bak/20210526181704-payments_geo_biz_logic.org" "/home/dgg/Dropbox/org/org-roam.bak/20210526181900-open_wallet_vs_closed_wallet.org" "/home/dgg/Dropbox/org/org-roam.bak/20210527112602-dynamic_programming_and_bitmasks.org" "/home/dgg/Dropbox/org/org-roam.bak/20210528040458-java_bit_shift_and_logical_operators.org" "/home/dgg/Dropbox/org/org-roam.bak/20210528150555-binary_search_template_jiuzhang.org" "/home/dgg/Dropbox/org/org-roam.bak/20210606104047-graph_representations_and_implementations.org" "/home/dgg/Dropbox/org/org-roam.bak/20210622025308-the_effective_engineer.org" "/home/dgg/Dropbox/org/org-roam.bak/20210630003823-java_data_structures.org" "/home/dgg/Dropbox/org/org-roam.bak/20210901134952-python_sorting_docs.org" "/home/dgg/Dropbox/org/org-roam.bak/ActiveMQ_in_Action.org" "/home/dgg/Dropbox/org/org-roam.bak/Algorithms - Sanjoy Dasgupta, Christos H. Papadimitriou, and Umesh V. Vazirani.org" "/home/dgg/Dropbox/org/org-roam.bak/Algorithms.org" "/home/dgg/Dropbox/org/org-roam.bak/Design Patterns Explained A New  Perspectiveon Object Oriented Design.org" "/home/dgg/Dropbox/org/org-roam.bak/Dhawan Session Notes.org" "/home/dgg/Dropbox/org/org-roam.bak/Eknath Easwaran-The Bhagavad Gita  -Nilgiri Press (2007).org" "/home/dgg/Dropbox/org/org-roam.bak/Modern_Java_in_Action.org" "/home/dgg/Dropbox/org/org-roam.bak/The Effective Engineer.org" "/home/dgg/Dropbox/org/org-roam.bak/The Prophet.org" "/home/dgg/Dropbox/org/roam/daily/2021-09-10.org" "/home/dgg/Dropbox/org/roam/scratch/2021-05-12.org" "/home/dgg/Dropbox/org/roam/scratch/2021-09-10.org" "/home/dgg/Dropbox/org/roam/scratch/2021-10-04.org" "/home/dgg/Dropbox/org/roam/20210512234800-a_beautiful_idea_to_test_this_out.org" "/home/dgg/Dropbox/org/roam/20210512235108-another_file_for_bibtex.org" "/home/dgg/Dropbox/org/roam/20210513000828-working_on_this_file.org" "/home/dgg/Dropbox/org/roam/20210513001806-wow_testing_bibtex.org" "/home/dgg/Dropbox/org/roam/20210513222926-stock_tips_from_uncle.org" "/home/dgg/Dropbox/org/roam/20210514133326-dfs_from_geeta_algo_huddle.org" "/home/dgg/Dropbox/org/roam/20210514141957-depth_first_search_graph_traversal.org" "/home/dgg/Dropbox/org/roam/20210514144510-leetcode_vs_formal_approach_to_problem_solving.org" "/home/dgg/Dropbox/org/roam/20210516184441-ediff_customization_commands.org" "/home/dgg/Dropbox/org/roam/20210517085859-dfs_contract_and_code.org" "/home/dgg/Dropbox/org/roam/20210521132102-count_the_number_of_bits_set_in_a_Integer_Number.org" "/home/dgg/Dropbox/org/roam/20210526181704-payments_geo_biz_logic.org" "/home/dgg/Dropbox/org/roam/20210526181900-open_wallet_vs_closed_wallet.org" "/home/dgg/Dropbox/org/roam/20210527112602-dynamic_programming_and_bitmasks.org" "/home/dgg/Dropbox/org/roam/20210528040458-java_bit_shift_and_logical_operators.org" "/home/dgg/Dropbox/org/roam/20210528150555-binary_search_template_jiuzhang.org" "/home/dgg/Dropbox/org/roam/20210606104047-graph_representations_and_implementations.org" "/home/dgg/Dropbox/org/roam/20210622025308-the_effective_engineer.org" "/home/dgg/Dropbox/org/roam/20210630003823-java_data_structures.org" "/home/dgg/Dropbox/org/roam/20210901134952-python_sorting_docs.org" "/home/dgg/Dropbox/org/roam/20210910213249-closures.org" "/home/dgg/Dropbox/org/roam/20211004075833-linux_and_c.org" "/home/dgg/Dropbox/org/roam/ActiveMQ_in_Action.org" "/home/dgg/Dropbox/org/roam/Algorithms - Sanjoy Dasgupta, Christos H. Papadimitriou, and Umesh V. Vazirani.org" "/home/dgg/Dropbox/org/roam/Algorithms.org" "/home/dgg/Dropbox/org/roam/Design Patterns Explained A New  Perspectiveon Object Oriented Design.org" "/home/dgg/Dropbox/org/roam/Dhawan Session Notes.org" "/home/dgg/Dropbox/org/roam/Eknath Easwaran-The Bhagavad Gita  -Nilgiri Press (2007).org" "/home/dgg/Dropbox/org/roam/Modern_Java_in_Action.org" "/home/dgg/Dropbox/org/roam/The Effective Engineer.org" "/home/dgg/Dropbox/org/roam/The Prophet.org" "/home/dgg/Dropbox/org/Getting Started with Orgzly.org" "/home/dgg/Dropbox/org/Laryssa.org" "/home/dgg/Dropbox/org/Resources.org" "/home/dgg/Dropbox/org/Rishik-words.org" "/home/dgg/Dropbox/org/adcom.org" "/home/dgg/Dropbox/org/archive.org" "/home/dgg/Dropbox/org/areas.org" "/home/dgg/Dropbox/org/books_read.org" "/home/dgg/Dropbox/org/dailies.org" "/home/dgg/Dropbox/org/dgg_bib.org" "/home/dgg/Dropbox/org/done.org" "/home/dgg/Dropbox/org/done_map.org" "/home/dgg/Dropbox/org/dropbox.org" "/home/dgg/Dropbox/org/elisp.org" "/home/dgg/Dropbox/org/gtd.org" "/home/dgg/Dropbox/org/inbox.org" "/home/dgg/Dropbox/org/interview.org" "/home/dgg/Dropbox/org/investment.org" "/home/dgg/Dropbox/org/journal.org" "/home/dgg/Dropbox/org/linear-algebra.org" "/home/dgg/Dropbox/org/linux.org" "/home/dgg/Dropbox/org/my_thoughts.org" "/home/dgg/Dropbox/org/notes.org" "/home/dgg/Dropbox/org/paypal_h4_ead.org" "/home/dgg/Dropbox/org/projects.org" "/home/dgg/Dropbox/org/reading.org" "/home/dgg/Dropbox/org/recommendation.org" "/home/dgg/Dropbox/org/rishik.org" "/home/dgg/Dropbox/org/sai.org" "/home/dgg/Dropbox/org/shopping list.org" "/home/dgg/Dropbox/org/standup.org" "/home/dgg/Dropbox/org/tepper.org" "/home/dgg/Dropbox/org/testing.org" "/home/dgg/Dropbox/org/todo.org" "/home/dgg/Dropbox/org/venmo.org" "/home/dgg/Dropbox/org/vimium.org" "/home/dgg/Dropbox/1_Projects/BuildingASecondBrain/bsb.org" "/home/dgg/Dropbox/1_Projects/DFS/robot-room-cleaner.org" "/home/dgg/Dropbox/1_Projects/Estate Planning/What Attorney needs from you.org" "/home/dgg/Dropbox/1_Projects/KhanLabsSchool/school-rishik.org" "/home/dgg/Dropbox/1_Projects/LongestCommonSubsequence/lcs.org" "/home/dgg/Dropbox/1_Projects/Python_Heaps/heaps.org" "/home/dgg/Dropbox/1_Projects/Python_Heaps/ramblings.org" "/home/dgg/Dropbox/1_Projects/RishikProgress_3.5/rishik_progress.org" "/home/dgg/Dropbox/1_Projects/TeachRishikMath/What-Children-Know-and-Need-to-Know-About-Pattern-and-Algebra-Expanded-Version.org" "/home/dgg/Dropbox/1_Projects/TeachRishikMath/notes_on_how_to_teach_pattern.org" "/home/dgg/Dropbox/1_Projects/TheAlgorithmDesignManual/ch01/NearestNeighbor_X_ProjectionApproach.org" "/home/dgg/Dropbox/1_Projects/TheAlgorithmDesignManual/SkienaTheAlgorithmDesignManual.org" "/home/dgg/Dropbox/1_Projects/Vazhirani_Algos/Algorithms - Sanjoy Dasgupta, Christos H. Papadimitriou, and Umesh V. Vazirani.org" "/home/dgg/Dropbox/1_Projects/subsets/example.org" "/home/dgg/Dropbox/1_Projects/subsets/subset.org" "/home/dgg/Dropbox/1_Projects/rishik_progress.org" "/home/dgg/Dropbox/2_Areas/my_linux_commands/cmd.org" "/home/dgg/Dropbox/2_Areas/rishik_kumon_progress/rishik_kumon_done.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/ask_ron/Geometry.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/ask_ron/Quant_backsolve.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/q/ds/testingcases_part1.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/q/magoosh/ps.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/q/official_esr (MTVL16092c0c9.local's conflicted copy 2019-10-11).org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/q/official_esr.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/q/og_guide_2019.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/reading/InnovatorSolution.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/reading/notes.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/sc/cr/cr-tamg-club.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/sc/modifiers/modifiers.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/sc/parallelism/parallelism-sc-2.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/sc/parallelism/parallelism-sc.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/sc/pronouns/pronoun.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/sc/drills.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/time_sheet/princetonAlgos.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/time_sheet/prob_book.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/20181223_review.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/ron_dec22_2018.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/ron_notes_on_review.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/MGMT/TamgNotes/tamg.org" "/home/dgg/Dropbox/4_Archives/Archived Projects For Future/Udacity_AI/lesson_4.org" "/home/dgg/Dropbox/4_Archives/Done/2019/adcom/adcom/essay.org" "/home/dgg/Dropbox/4_Archives/Done/2019/adcom/adcom/notes.org" "/home/dgg/Dropbox/4_Archives/Done/2019/adcom/tsb-essay.org" "/home/dgg/Dropbox/4_Archives/Done/2020/obamas_speech.org" "/home/dgg/Dropbox/4_Archives/Done/2021/RC_Veritas_Article/Reading_COMP_Vertias.org" "/home/dgg/Dropbox/4_Archives/Done/2021/Toxic_Phrases/Toxic_Phrases.org" "/home/dgg/Dropbox/4_Archives/pluralsights/beyond-the-basics.org" "/home/dgg/workdir/inter/interview/Arrays/explanation/238-Product-of-Array-Except-Self.org" "/home/dgg/workdir/inter/interview/Arrays/explanation/26_RemoveDuplicatesFromSortedArray.org" "/home/dgg/workdir/inter/interview/Arrays/explanation/54-spiral-matrix.org" "/home/dgg/workdir/inter/interview/Arrays/explanation/69_sqrt.org" "/home/dgg/workdir/inter/interview/Arrays/explanation/81_Search_in_Rotated_Sorted_Array_II.org" "/home/dgg/workdir/inter/interview/Arrays/explanation/LC-7-reverse-integer.org" "/home/dgg/workdir/inter/interview/Arrays/explanation/PancakeSorting.org" "/home/dgg/workdir/inter/interview/Arrays/explanation/li-65-median-of-two-sorted-arrays.org" "/home/dgg/workdir/inter/interview/Interview/explanation/order-check.org" "/home/dgg/workdir/inter/interview/Interview/TopKFrequentItems.org" "/home/dgg/workdir/inter/interview/Interview/suprema-para-details-question.org" "/home/dgg/workdir/inter/interview/books/nodejs-8-the-right-way.org" "/home/dgg/workdir/inter/interview/courses/web.stanford.edu/class/archive/cs/cs161/cs161.1168/lecture12.org" "/home/dgg/workdir/inter/interview/courses/AlgorithmicThinking_Sample_ch1.org" "/home/dgg/workdir/inter/interview/datastructures-explained/TreeMap.org" "/home/dgg/workdir/inter/interview/datastructures-explained/bitmask.org" "/home/dgg/workdir/inter/interview/design_patter/Singleton.org" "/home/dgg/workdir/inter/interview/dynamic_programming/explanation/198-house-robber.org" "/home/dgg/workdir/inter/interview/dynamic_programming/explanation/403-Frog-Jump.org" "/home/dgg/workdir/inter/interview/dynamic_programming/explanation/LC-279-perfect-squares.org" "/home/dgg/workdir/inter/interview/dynamic_programming/explanation/LongestCommonSubsequence.org" "/home/dgg/workdir/inter/interview/dynamic_programming/explanation/ZeroOneKnapSack.org" "/home/dgg/workdir/inter/interview/dynamic_programming/explanation/contiguous-sum.org" "/home/dgg/workdir/inter/interview/dynamic_programming/explanation/ik_dynamic_programming.org" "/home/dgg/workdir/inter/interview/dynamic_programming/explanation/lc-1235-max-profit-in-job-scheduling.org" "/home/dgg/workdir/inter/interview/dynamic_programming/explanation/longest-common-substring.org" "/home/dgg/workdir/inter/interview/dynamic_programming/paint_house.org" "/home/dgg/workdir/inter/interview/dynamic_programming/topcoder.org" "/home/dgg/workdir/inter/interview/epi/explanation/6.7_buy_and_sell_stock_twice.org" "/home/dgg/workdir/inter/interview/epi/explanation/7.9-convert-from-roman-to-decimal.org" "/home/dgg/workdir/inter/interview/epi/explanation/README_FIRST.org" "/home/dgg/workdir/inter/interview/epi/explanation/ch06.org" "/home/dgg/workdir/inter/interview/epi/src/ch06/ch06.org" "/home/dgg/workdir/inter/interview/googleCodeJam/google-kickstart-2019.org" "/home/dgg/workdir/inter/interview/graph/explanations/103-Binary-Tree-zigzag-level-order-traversal.org" "/home/dgg/workdir/inter/interview/graph/explanations/615-li-course-schedule.org" "/home/dgg/workdir/inter/interview/graph/explanations/Lc-1042-Flower-Planting-With-No-Adjacent.org" "/home/dgg/workdir/inter/interview/graph/explanations/graph-dfs-and-bfs-geeta-notes.org" "/home/dgg/workdir/inter/interview/graph/explanations/graph-representation.org" "/home/dgg/workdir/inter/interview/graph/explanations/remove-invalid-parentheses.org" "/home/dgg/workdir/inter/interview/graph/explanations/topological-sort.org" "/home/dgg/workdir/inter/interview/graph/explanations/word-ladder-li-120-lc-127.org" "/home/dgg/workdir/inter/interview/graph/book_vazhirani.org" "/home/dgg/workdir/inter/interview/html_pages/notes-adam.org" "/home/dgg/workdir/inter/interview/leet/explanation/coding_bat_exercises/2021-06-17_11-57-24_Warmup_1.org" "/home/dgg/workdir/inter/interview/leet/explanation/1504-Count-Submatrices-With-All-Ones.org" "/home/dgg/workdir/inter/interview/leet/explanation/152-max-prod-subarray.org" "/home/dgg/workdir/inter/interview/leet/explanation/153-Find-Minimum-In-Rotate-Sorted-Array.org" "/home/dgg/workdir/inter/interview/leet/explanation/169-Majority-Element.org" "/home/dgg/workdir/inter/interview/leet/explanation/17-Letter-Combinations-of-a-phone-number.org" "/home/dgg/workdir/inter/interview/leet/explanation/186-reverse-words-in-a-string.org" "/home/dgg/workdir/inter/interview/leet/explanation/20-valid-parenthesis.org" "/home/dgg/workdir/inter/interview/leet/explanation/200-number-of-islands-i.org" "/home/dgg/workdir/inter/interview/leet/explanation/21-Merge-Two-Sorted-Lists.org" "/home/dgg/workdir/inter/interview/leet/explanation/23-Merge-K-sortedlist.org" "/home/dgg/workdir/inter/interview/leet/explanation/23_Merge_k_Sorted_Lists.org" "/home/dgg/workdir/inter/interview/leet/explanation/250-count-univalue-subtrees.org" "/home/dgg/workdir/inter/interview/leet/explanation/274_275-H-index-I_II.org" "/home/dgg/workdir/inter/interview/leet/explanation/278-First-Bad-Version.org" "/home/dgg/workdir/inter/interview/leet/explanation/285-inorder-successor-in-bst.org" "/home/dgg/workdir/inter/interview/leet/explanation/31-next-permutations.org" "/home/dgg/workdir/inter/interview/leet/explanation/32-lc-193-lic-longest-valid-parentheses.org" "/home/dgg/workdir/inter/interview/leet/explanation/332-reconstruct-itinerary.org" "/home/dgg/workdir/inter/interview/leet/explanation/33_Search_in_Rotated_Sorted_Array.org" "/home/dgg/workdir/inter/interview/leet/explanation/387-find-unique-chars-in-string.org" "/home/dgg/workdir/inter/interview/leet/explanation/394-decode-string.org" "/home/dgg/workdir/inter/interview/leet/explanation/3sum-2ptr-problem-series.org" "/home/dgg/workdir/inter/interview/leet/explanation/403_Frog_Jump.org" "/home/dgg/workdir/inter/interview/leet/explanation/5-Longest-Palindromic-Substring.org" "/home/dgg/workdir/inter/interview/leet/explanation/52-NQueens.org" "/home/dgg/workdir/inter/interview/leet/explanation/541_ReverseString_ii.org" "/home/dgg/workdir/inter/interview/leet/explanation/583_Delete_Operation_for_Two_Strings.org" "/home/dgg/workdir/inter/interview/leet/explanation/670-maximum-swap.org" "/home/dgg/workdir/inter/interview/leet/explanation/69_Sqrt.org" "/home/dgg/workdir/inter/interview/leet/explanation/763-partition-labels.org" "/home/dgg/workdir/inter/interview/leet/explanation/767-reorganize-string.org" "/home/dgg/workdir/inter/interview/leet/explanation/773-sliding-puzzle.org" "/home/dgg/workdir/inter/interview/leet/explanation/88_Merge_Sorted_Array.org" "/home/dgg/workdir/inter/interview/leet/explanation/937-reorder-data-in-log-file.org" "/home/dgg/workdir/inter/interview/leet/explanation/977_Squares_of_a_Sorted_array.org" "/home/dgg/workdir/inter/interview/leet/explanation/AlienDictionary.org" "/home/dgg/workdir/inter/interview/leet/explanation/CourseSchedule_ii.org" "/home/dgg/workdir/inter/interview/leet/explanation/GasStation.org" "/home/dgg/workdir/inter/interview/leet/explanation/LC-2-LiC-221-AddTwoNumbers-ii.org" "/home/dgg/workdir/inter/interview/leet/explanation/LiC-31-partition-array.org" "/home/dgg/workdir/inter/interview/leet/explanation/Longest_Substring_Without_Repeating_Characters_3.org" "/home/dgg/workdir/inter/interview/leet/explanation/RandomPickwithWeight.org" "/home/dgg/workdir/inter/interview/leet/explanation/dynamic-programming-ik.org" "/home/dgg/workdir/inter/interview/leet/explanation/ik_dp.org" "/home/dgg/workdir/inter/interview/leet/explanation/lc-1185-day-of-the-week.org" "/home/dgg/workdir/inter/interview/leet/explanation/lc-1235-max-profit-in-job-scheduling.org" "/home/dgg/workdir/inter/interview/leet/explanation/lc-155-lic-12-min-stack.org" "/home/dgg/workdir/inter/interview/leet/explanation/lc-169-lc-229-majority-elements.org" "/home/dgg/workdir/inter/interview/leet/explanation/lc-215-lic-5-Kth-largest-number.org" "/home/dgg/workdir/inter/interview/leet/explanation/lc-32-lic-193-longest-valid-parentheses.org" "/home/dgg/workdir/inter/interview/leet/explanation/lc-322-lic-1288-reconstruct-itinerary.org" "/home/dgg/workdir/inter/interview/leet/explanation/lc-371-lic-1-sum-of-two-numbers.org" "/home/dgg/workdir/inter/interview/leet/explanation/lc-75-LiC-148-Sort-Colors.org" "/home/dgg/workdir/inter/interview/leet/explanation/lc-78-lic-17-subsets.org" "/home/dgg/workdir/inter/interview/leet/explanation/lc-LiC-geeta-class-question-numb-of-moves-to-move-people-to-a-location.org" "/home/dgg/workdir/inter/interview/leet/explanation/math.org" "/home/dgg/workdir/inter/interview/leet/explanation/monotonic-queues.org" "/home/dgg/workdir/inter/interview/leet/explanation/n-queens.org" "/home/dgg/workdir/inter/interview/leet/explanation/number-of-islands.org" "/home/dgg/workdir/inter/interview/leet/explanation/permutations.org" "/home/dgg/workdir/inter/interview/leet/explanation/permutations_vs_subsets.org" "/home/dgg/workdir/inter/interview/lint/433-number-of-islands.org" "/home/dgg/workdir/inter/interview/lint/932-friends-within-three-jumps.org" "/home/dgg/workdir/inter/interview/lint/941-sliding-puzzle.org" "/home/dgg/workdir/inter/interview/lint/number-of-islands.org" "/home/dgg/workdir/inter/interview/onsite-interviews/sift-interview-question/README.org" "/home/dgg/workdir/inter/interview/puzzle/bridge-crossing.org" "/home/dgg/workdir/inter/interview/recursion/explanation/count-inversion.org" "/home/dgg/workdir/inter/interview/recursion/fib.org" "/home/dgg/workdir/inter/interview/resume/resume.org" "/home/dgg/workdir/inter/interview/sdi/7 Surprising Steps to Crack the Code Interview — Books on Code __ Reader View.org" "/home/dgg/workdir/inter/interview/sdi/DistributedLocksWithRedis.org" "/home/dgg/workdir/inter/interview/sdi/kafka.org" "/home/dgg/workdir/inter/interview/sdi/prometheus.org" "/home/dgg/workdir/inter/interview/sdi/raft.org" "/home/dgg/workdir/inter/interview/sdi/rate-limiting.org" "/home/dgg/workdir/inter/interview/sdi/sdi.org" "/home/dgg/workdir/inter/interview/sdi/ticket-master-db-design.org" "/home/dgg/workdir/inter/interview/sorting/692-Top-K-Frequent-Words.org" "/home/dgg/workdir/inter/interview/sorting/all-sorting-algos-explanation.org" "/home/dgg/workdir/inter/interview/sorting/bucketsort (Copy 1) (Copy 1).org" "/home/dgg/workdir/inter/interview/sorting/geeta-notes.org" "/home/dgg/workdir/inter/interview/sorting/merge-sort-iterative.org" "/home/dgg/workdir/inter/interview/sorting/nuts-and-bolts-lintcode-399.org" "/home/dgg/workdir/inter/interview/sorting/nuts-and-bolts.org" "/home/dgg/workdir/inter/interview/sorting/quicksort-notes.org" "/home/dgg/workdir/inter/interview/strings/explanation/KMP-pattern-match.org" "/home/dgg/workdir/inter/interview/strings/explanation/LongestSubSequence_With_TwoDistinct_characters.org" "/home/dgg/workdir/inter/interview/strings/explanation/Strings.org" "/home/dgg/workdir/inter/interview/strings/explanation/algo-pro.org" "/home/dgg/workdir/inter/interview/strings/explanation/anagrams.org" "/home/dgg/workdir/inter/interview/strings/explanation/count_number_of_strings_with_exactly_k_distinct_strings.org" "/home/dgg/workdir/inter/interview/strings/explanation/lc-5-longest-palindromic-substring.org" "/home/dgg/workdir/inter/interview/strings/explanation/tries.org" "/home/dgg/workdir/inter/interview/trees/explanation/993-cousins-in-btree.org" "/home/dgg/workdir/inter/interview/trees/explanation/SerializeDeserializeTree.org" "/home/dgg/workdir/inter/interview/trees/explanation/binarySearchTreeDistanceBetweenTwoNodes.org" "/home/dgg/workdir/inter/interview/trees/explanation/maxPathSum.org" "/home/dgg/workdir/inter/interview/twitter-questions/explanation/set-cover-problem.org" "/home/dgg/workdir/inter/interview/IK.org" "/home/dgg/workdir/inter/interview/Incrementer.org" "/home/dgg/workdir/inter/interview/geeta.org" "/home/dgg/workdir/inter/interview/geeta_class_notes.org" "/home/dgg/workdir/inter/interview/katas.org" "/home/dgg/workdir/inter/interview/notes-adam.org"))
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-pomodoro-length 45)
 '(org-pomodoro-long-break-length 25)
 '(org-pomodoro-short-break-length 7)
 '(org-todo-keyword-faces
   '(("REPEAT" . "red")
     ("PROJECT" . "cyan3")
     ("BLOCKED" . "tomato")))
 '(org-todo-keywords
   '((sequence "REPEAT(r)" "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d)")
     (sequence "PROJECT(p)" "AREA(a)" "|" "COMPLETED(c)")
     (sequence "LATER(l)" "WAITING(w)" "FUTURE(f)" "|" "CANCELLED(x)")
     (sequence "|" "NOTE(n)")))
 '(org-treat-S-cursor-todo-selection-as-state-change nil)
 '(org-use-fast-todo-selection t)
 '(package-selected-packages
   '(org-roam-dailies csv-mode zenburn-theme zen-and-art-theme yapfify white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme pyvenv pytest pyenv-mode py-isort purple-haze-theme professional-theme planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme org-noter omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme live-py-mode light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme hy-mode dash-functional heroku-theme hemisu-theme helm-pydoc hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flyspell-correct-helm flyspell-correct flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme company-anaconda color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-dictionary atomic-chrome websocket apropospriate-theme anti-zenburn-theme anaconda-mode pythonic ample-zen-theme ample-theme alect-themes afternoon-theme doom-dracula-theme helm-org yaml-mode xterm-color ws-butler winum which-key web-mode volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tagedit sql-indent spaceline powerline smeargle slim-mode shell-pop scss-mode sass-mode restart-emacs rainbow-delimiters pug-mode popwin persp-mode pcre2el paradox spinner orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-plus-contrib org-mime org-download org-bullets open-junk-file neotree multi-term move-text mmm-mode markdown-toc markdown-mode magit-gitflow magit-popup macrostep lorem-ipsum linum-relative link-hint insert-shebang indent-guide ibuffer-projectile hydra lv hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-gitignore request helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flx-ido flx fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired f evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit git-commit with-editor transient evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav dumb-jump s diminish define-word company-web web-completion-data company-statistics company-shell dash company column-enforce-mode clean-aindent-mode bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(rustic-ansi-faces
   ["#282a36" "#ff5555" "#50fa7b" "#f1fa8c" "#61bfff" "#ff79c6" "#8be9fd" "#f8f8f2"])
 '(vc-annotate-background "#282a36")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50fa7b")
    (cons 40 "#85fa80")
    (cons 60 "#bbf986")
    (cons 80 "#f1fa8c")
    (cons 100 "#f5e381")
    (cons 120 "#face76")
    (cons 140 "#ffb86c")
    (cons 160 "#ffa38a")
    (cons 180 "#ff8ea8")
    (cons 200 "#ff79c6")
    (cons 220 "#ff6da0")
    (cons 240 "#ff617a")
    (cons 260 "#ff5555")
    (cons 280 "#d45558")
    (cons 300 "#aa565a")
    (cons 320 "#80565d")
    (cons 340 "#6272a4")
    (cons 360 "#6272a4")))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil)
 '(writeroom-width 70))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    (quote
;;     (zenburn-theme zen-and-art-theme yapfify white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme pyvenv pytest pyenv-mode py-isort purple-haze-theme professional-theme planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme org-noter omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme live-py-mode light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme hy-mode dash-functional heroku-theme hemisu-theme helm-pydoc hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flyspell-correct-helm flyspell-correct flatui-theme flatland-theme farmhouse-theme exotica-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme company-anaconda color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme auto-dictionary atomic-chrome websocket apropospriate-theme anti-zenburn-theme anaconda-mode pythonic ample-zen-theme ample-theme alect-themes afternoon-theme doom-dracula-theme helm-org yaml-mode xterm-color ws-butler winum which-key web-mode volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tagedit sql-indent spaceline powerline smeargle slim-mode shell-pop scss-mode sass-mode restart-emacs rainbow-delimiters pug-mode popwin persp-mode pcre2el paradox spinner orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-plus-contrib org-mime org-download org-bullets open-junk-file neotree multi-term move-text mmm-mode markdown-toc markdown-mode magit-gitflow magit-popup macrostep lorem-ipsum linum-relative link-hint insert-shebang indent-guide ibuffer-projectile hydra lv hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-gitignore request helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flx-ido flx fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired f evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit git-commit with-editor transient evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav dumb-jump s diminish define-word company-web web-completion-data company-statistics company-shell dash company column-enforce-mode clean-aindent-mode bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
