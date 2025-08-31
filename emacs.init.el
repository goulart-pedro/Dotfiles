;; testing

(require 'package)

;; adds melpa as an additional repository for package
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; hides tool and menu bars
(tool-bar-mode -1)
(menu-bar-mode -1)

;; enables context menu on mouse right click (why not default???)
(context-menu-mode t)

(defun kill-inside (start end)
  (interactive "r")
  (up-list)
  (backward-char)
  (push-mark)
  (up-list -1)
  (kill-region start end))

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
      completions-max-height 10
      completions-sort 'alphabetical
      completion-ignore-case t)

;; Auto close parentheses
(electric-pair-mode 1)

(fido-mode t)
(fido-vertical-mode t)

;; just a nicer looking modeline
(simple-modeline-mode t)


;; adds completion and lsp when prog files are loaded
;; eglot displays an error if no lsp server is installed
;; (not a big issue)
(add-hook 'prog-mode-hook 'corfu-mode)
(add-hook 'prog-mode-hook 'eglot-ensure)

(add-hook 'c++-mode-hook
  (lambda ()
    (setq compile-command 
          (concat "g++-14 " (buffer-file-name) " -o " 
                  (file-name-sans-extension (buffer-file-name))))))

;; makes corfu showup automaticallly
(setq corfu-auto t)

;; display identation tabs or spaces
;; note that face must also be in the list
(setq-default whitespace-style '(face indentation indentation::tabs indentation::spaces))


;; don't move point around on scroll
(setq scroll-preserve-screen-position t)

(setq mode-line-format '("%e" mode-line-front-space
 (:propertize
  ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
  display
  (min-width
   (5.0)))
 mode-line-frame-identification mode-line-buffer-identification " "
 (vc-mode vc-mode)
 " " mode-line-modes mode-line-misc-info mode-line-end-spaces)
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-use-fuzzy t)
 '(custom-enabled-themes '(doom-tomorrow-night))
 '(custom-safe-themes
   '("7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184"))
 '(doom-tomorrow-night-padded-modeline nil)
 '(eldoc-echo-area-use-multiline-p nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(package-selected-packages
   '(simple-modeline parrot nyan-mode doom-themes ef-themes standard-themes corfu riscv-mode jsonian gruber-darker-theme catppuccin-theme))
 '(pixel-scroll-precision-mode t)
 '(windmove-default-keybindings '([ignore] meta)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono")))))
