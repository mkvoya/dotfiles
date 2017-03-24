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

;minibuffer editing
(use-package miniedit
  :ensure t
  :defer t
  :commands minibuffer-edit
  :init (miniedit-install))

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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (powerline use-package miniedit guide-key evil company color-theme-solarized))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
