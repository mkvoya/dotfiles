(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

;;; Library
(use-package dash
  :defer t)

;;; Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;(setq delete-old-versiojns -1)
;(setq version-control t)
;(setq vc-make-backup-files t)
;(setq auto-save-file-name-transforms '((".*" "~/emacs.d/auto-save-list/" t)))

;;; History
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring search-ring regex-search-ring))

; Windows
(tool-bar-mode -1) ; close tool bar
(menu-bar-mode -1) ; close menu bar

; Winner mode
(use-package winner :defer t)

; Sentence
(setq sentence-end-double-space nil)

; Mode line format
(use-package smart-mode-line
  :defer t)

; lazy answer
(fset 'yes-or-no-p 'y-or-n-p)

;;minibuffer editing
;(use-package miniedit
;  :ensure t
;  :defer t
;  :commands minibuffer-edit
;  :init (miniedit-install))

;; light-on-dark color scheme
;(defadvice color-theme-alist (around sacha activate)
;  (if (ad-get-arg 0)
;      ad-do-it
;    nil))
;(use-package color-theme :ensure t)
;(use-package color-theme-solarized :ensure t)
;(defun my/setup-color-theme ()
;  (interactive)
;  (color-theme-solarized-dark)
;  (set-face-foreground 'secondary-selection "darkblue")
;  (set-face-background 'secondary-selection "lightblue")
;  (set-face-background 'font-lock-doc-face "black")
;  (set-face-foreground 'font-lock-doc-face "wheat")
;  (set-face-background 'font-lock-string-face "black")
;  (set-face-foreground 'org-todo "green")
;  (set-face-background 'org-todo "black"))
;
;;(eval-after-load 'color-theme (my/setup-color-theme))


(use-package undo-tree
  :ensure t
  :defer t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package guide-key
  :ensure t
  :defer t
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
    (guide-key-mode 1))) ; Enable guide-key-mode

; UTF-8
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPUND_TEXT TEXT STRING)))


; Unicode
(defmacro my/insert-unicode (unicode-name)
  `(lambda () (interactive)
     (insert-char (cdr (assoc-string, unicode-name (ucs-names))))))
(bind-key "C-x 8 s" (my/insert-unicode "ZERO WIDTH SPACE"))
(bind-key "C-x 8 S" (my/insert-unicode "SNOWMAN"))
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

; Clean up spaces
(bind-key "M-SPC" 'cycle-spacing)

; Show column number
(column-number-mode 1)

; Autocomplete
(use-package company
  :ensure t
  :defer t
  :config (add-hook 'prog-mode-hook 'company-mode))


;;; EVIL
(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)

;;; change mode-line color by evil state
;(lexical-let ((default-color (cons (face-background 'mode-line)
;				   (face-foreground 'mode-line))))
;  (add-hook 'post-command-hook
;	    (lambda ()
;	      (let ((color (cond ((minibufferp) default-color)
;				 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
;				 ((evil-emacs-state-p) '("#444488" . "#ffffff"))
;				 ((buffer-modified-p) '("#006fa0" . "#ffffff"))
;				 (t default-color))))
;		(set-face-background 'mode-line (car color))
;		(set-face-foreground 'mode-line (cdr color))))))


; Powerline
(use-package powerline
  :ensure t
  :config
  (powerline-center-evil-theme))

(setq line-number-mode t)
(setq linum-format "%d ")
(global-linum-mode t)

(load-theme 'manoj-dark)

(setq c-default-style "linux"
      c-basic-offset 8)
(setq-default c-basic-offset 8
	      tab-width 8
	      indent-tabs-mode t)

; Whitespace, check: http://ergoemacs.org/emacs/whitespace-mode.html
(use-package whitespace
  :ensure t)
(setq whitespace-style '(trailing tabs newline tab-mark newline-mark))
(setq whitespace-display-mappings
      '(
	(newline-mark 10 [8617 10])
	(tab-mark 9 [8594 9] [92 9])))
(global-whitespace-mode t)


(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


; style I want to use in c++ mode
(c-add-style "my-style"
	     '("stroustrup"
	       (c-basic-offset . 4)            ; indent by four spaces
	       (tab-width . 4)
	       (indent-tabs-mode . t)        ; use tabs
	       (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
				   (brace-list-open . 0)
				   (innamespace . [0])
				   (statement-case-open . +)))))
;(defun my-c-setup ()
;  (c-set-offset 'innamespace [0]))
;(add-hook 'c++-mode-hook 'my-c-setup)

(defun my-c++-mode-hook ()
  (c-set-style "my-style"))        ; use my-style defined above
  ;(auto-fill-mode)
  ;(c-toggle-auto-hungry-state 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)


(use-package monokai-theme
  :ensure t)
(load-theme 'monokai t)

;(use-package spaceline-config
;  :ensure t
;  :config
;  (spaceline-spacemacs-theme))

(use-package airline-themes
  :ensure t)
(load-theme 'airline-light t)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "962dacd99e5a99801ca7257f25be7be0cebc333ad07be97efd6ff59755e6148f" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" default)))
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (powerline use-package miniedit guide-key evil company color-theme-solarized)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; FONT
;;;;;;;;; DO NOT WORK FOR CLI
;;;;; INSTALL FILES
;(call-process "git" nil t nil "clone" "https://github.com/powerline/fonts.git" "/tmp/fonts")
;(call-process "ls" nil t nil "/tmp/fonts")
;(call-process "/tmp/fonts/install.sh" nil t)

;;;;;;(add-to-list 'default-frame-alist '(font . "3270"))
;;;;;;(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono for Powerline"))
;;;;;;
;;;;;;(set-face-attribute 'default nil :family "DejaVu Sans Mono for Powerline")
;;;;;;;(set-face-attribute 'default nil :height 94)
;;;;;;(set-fontset-font "fontset-default" 'unicode "DejaVu Sans Mono for Powerline")
;;;;;;
;;;;;;;;
;;;;;;
;;;;;;(set-face-attribute 'mode-line nil :family "DejaVu Sans Mono for Powerline")
;;;;;;(set-face-attribute 'mode-line-inactive nil :family "DejaVu Sans Mono for Powerline")
;;;;;;(set-face-attribute 'powerline-inactive1 nil :family "DejaVu Sans Mono for Powerline")
;;;;;;(set-face-attribute 'powerline-active1 nil :family "DejaVu Sans Mono for Powerline")


(setq powerline-default-separator 'utf-8)
(setq powerline-utf-8-separator-left #x27bd)
(setq powerline-utf-8-separator-right #x2b05)

(use-package vimish-fold
  :ensure t
  :defer t)
;(vimish-fold-global-mode 1)
(use-package evil-vimish-fold
  :ensure t
  :defer t)
;(evil-vimish-fold-mode 1)

(add-hook 'prog-mode-hook #'hs-minor-mode)
