;; Example Elpaca configuration -*- lexical-binding: t; -*-
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Uncomment for systems which cannot create symlinks:
;; (elpaca-no-symlink-mode)

;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.

;; (elpaca example-package)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use the :wait recipe keyword to block until that package has been installed/configured.
;;For example:
;;(use-package general :ensure (:wait t) :demand t)

;; Expands to: (elpaca evil (use-package evil :demand t))
;; (use-package evil :ensure t :demand t)

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause the declaration to be interpreted immediately (not deferred).
;;Useful for configuring built-in emacs features.
(use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

;; UI --------------------------

(use-package cyberpunk-theme
    :ensure t
    :config
    (load-theme 'cyberpunk t))


(elpaca treemacs (use-package treemacs
		   :ensure t
		   :bind ("<f5>" . treemacs)
		   :custom
		   (treemacs-is-never-other-window t)
		   :hook
		   (treemacs-mode . treemacs-project-follow-mode)))

(elpaca solaire-mode (use-package solaire-mode
		       :ensure t
		       :hook (after-init . solaire-global-mode)))

(elpaca zoom (use-package zoom
	       :ensure t
	       :config
	       (custom-set-variables
		'(zoom-size '(0.618 . 0.618)))
	       (custom-set-variables
		'(zoom-mode t))))

(display-time-mode 1)

(elpaca all-the-icons (use-package all-the-icons
			:ensure t))
(elpaca nerd-icons (use-package nerd-icons
		     :ensure t))

(elpaca dashboard
  (use-package dashboard
    :ensure t
    :config
    (dashboard-setup-startup-hook)))

(elpaca doom-modeline (use-package doom-modeline
			:ensure t
			:init (doom-modeline-mode 1)
			:config (column-number-mode 1)
			:custom
			(doom-modeline-height 30)
			(doom-modeline-width-limit nil)
			(doom-modeline-buffer-file-name-style 'truncate-with-project)
			(doom-modeline-minor-modes nil)
			(doom-modeline-enable-word-count t)
			(doom-modeline-buffer-encoding nil)
			(doom-modeline-buffer-modification-icon t)
			(doom-modeline-time t)
			(doom-modeline-vcs-max-length 50)))

(elpaca spacious-padding (use-package spacious-padding
			   :ensure t
			   :hook (after-init . spacious-padding-mode)))

;; Usability -------------------

(elpaca ws-butler (use-package ws-butler
		    :config
		    (add-hook 'prog-mode-hook #'ws-butler-mode)))

(elpaca helpful (use-package helpful
		  :config
		  ;; Note that the built-in `describe-function' includes both functions
		  ;; and macros. `helpful-function' is functions only, so we provide
		  ;; `helpful-callable' as a drop-in replacement.
		  (global-set-key (kbd "C-h f") #'helpful-callable)
		  (global-set-key (kbd "C-h v") #'helpful-variable)
		  (global-set-key (kbd "C-h k") #'helpful-key)
		  (global-set-key (kbd "C-h x") #'helpful-command)
		  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
		  ;; for this in lisp modes.
		  (global-set-key (kbd "C-c C-d") #'helpful-at-point)

		  ;; Look up *F*unctions (excludes macros).
		  ;;
		  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
		  ;; already links to the manual, if a function is referenced there.
		  (global-set-key (kbd "C-h F") #'helpful-function)))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(elpaca vertico (use-package vertico
		  :init
		  (vertico-mode)))

(elpaca marginalia (use-package marginalia))
(elpaca consult (use-package consult))
(elpaca orderless ;; Optionally use the `orderless' completion style.
  (use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))))
(elpaca embark (use-package embark))
(elpaca embark-consult (use-package embark-consult))
(elpaca swiper (use-package swiper))

;; Development -----------------

(electric-pair-mode 1)
(show-paren-mode 1)

(elpaca transient (use-package transient))
(elpaca git (use-package git))
(elpaca git-commit (use-package git-commit))
(elpaca tree-sitter (use-package tree-sitter))
(elpaca magit (use-package magit))
(elpaca inflections (use-package inflections))

(elpaca projectile (use-package projectile
		     :config
		     (projectile-mode +1)
		     (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(elpaca clojure-mode (use-package clojure-mode))
;; People are working on replacing with clojure-ts-mode. Feature incomplete at the moment.
(elpaca clojure-ts-mode (use-package clojure-ts-mode))
(elpaca clojure-snippets (use-package clojure-snippets))

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(elpaca clj-refactor (use-package clj-refactor
		       :config
		       (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)))
(elpaca cider (use-package cider))

(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)


;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))
