(require 'package)

;; adds melpa as an additional repository for package
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; hides tool and menu bars
(tool-bar-mode -1)
(menu-bar-mode -1)

;; enables context menu on mouse right click (why not default???)
(context-menu-mode t)

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

;; puts backups into tmp/backups
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; puts autosave files into tmp/autosaves
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;; Always use spaces for indentation
(setq-default indent-tabs-mode: nil)
;; Display relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; why setf?
(setf completion-styles '(flex)
      completion-auto-select t
      completion-auto-help 'visible
      ;; completions-format 'one-column
      completions-max-height 15
      completions-sort 'alphabetical
      completion-ignore-case t)

;; Auto close parentheses
(electric-pair-mode 1)

(fido-mode t)
(fido-vertical-mode t)

;; just a nicer looking modeline
(simple-modeline-mode t)


;; allows me to easilly search for all files inside a directory
;; no matter the nesting (either uses a git repo or a manual selection)
(global-set-key (kbd "C-;") 'project-find-file)

;; shortcut to move to the end of the line
(global-set-key (kbd "C-$") 'end-of-line)

;; adds completion and lsp when prog files are loaded
;; eglot displays an error if no lsp server is installed
;; (not a big issue)
(add-hook 'prog-mode-hook 'corfu-mode)
(add-hook 'prog-mode-hook 'eglot-ensure)


;; auto config for compile command; might change it later
(add-hook 'c++-mode-hook
  (lambda ()
    (setq compile-command 
          (concat "g++-14 " (buffer-file-name) " -o " 
                  (file-name-sans-extension (buffer-file-name))))))

;; makes corfu showup automaticallly
(setq corfu-auto t)

;; display identation tabs or spaces
;; this isn't really working
;; note that face must also be in the list
(setq-default whitespace-style '(face indentation indentation::tabs indentation::spaces))


;; don't move point around on scroll
(setq scroll-preserve-screen-position t)


(transient-define-prefix my/compile ()
"Group for compiling related tasks"
["Compilation"
 ("c" "compile" compile)
 ("g" "recompile" recompile)])

(global-set-key (kbd "C-.") 'my/compile)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-night))
 '(custom-safe-themes
   '("7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184"))
 '(doom-tomorrow-night-padded-modeline nil)
 '(eldoc-echo-area-use-multiline-p nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(package-selected-packages
   '(simple-modeline parrot nyan-mode doom-themes ef-themes standard-themes corfu riscv-mode jsonian gruber-darker-theme catppuccin-theme))
 '(pixel-scroll-precision-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono")))))
