;; global keys (not sure if I need to rebind ESC)
  (define-key key-translation-map (kbd "ESC") (kbd "C-g"))

  ;; hooks
  (defvar clavis-command-mode-activate-hook nil "Hook for `clavis-command-mode-activate'")
  (defvar clavis-insert-mode-activate-hook nil "Hook for `clavis-insert-mode-activate'")


  ;; keymaps
  (defvar clavis-key-map (make-sparse-keymap) "Keybinding for `clavis' minor mode.")
  (defvar clavis-insert-mode-keymap (make-sparse-keymap) "Keybinding for `clavis' command mode.")
  (defvar clavis-command-mode-keymap (make-sparse-keymap) "Keybinding for `clavis' insert mode.")

  (defvar clavis-leader-keymap (make-sparse-keymap) "Keybinding for `clavis' leader map.")

  ;; Another option just in case
  (define-key clavis-key-map (kbd "<f7>") clavis-leader-keymap)
  (define-key clavis-key-map (kbd "<f7>") clavis-leader-keymap)
  (define-key clavis-key-map (kbd "<f8>") 'clavis-command-mode-activate)
  (define-key clavis-key-map (kbd "<f9>") 'clavis-insert-mode-activate)

  ;; For system settings
  (define-key clavis-key-map (kbd "<f13>") clavis-leader-keymap)  ;; set CapsLock to f13
  (define-key clavis-key-map (kbd "<f14>") 'clavis-command-mode-activate)  ;; set Alt to f14
  ;; does not work with karabiner on Mac (looks like emacs does not see f14)

  ;; Convinient bindings when CapsLock and Alt is not set in the system bindings
  (define-key clavis-command-mode-keymap (kbd "SPC") 'clavis-insert-mode-activate)
  (define-key clavis-insert-mode-keymap (kbd "M-SPC") 'clavis-command-mode-activate)

  ;; (define-key clavis-command-mode-keymap (kbd "a") clavis-leader-keymap)
  ;; (define-key clavis-command-mode-keymap (kbd "a") nil)


  (defun clavis-command-mode-activate ()
    "Activate command mode."
    (interactive)
    (modify-all-frames-parameters (list (cons 'cursor-type 'box)))
    (global-hl-line-mode 1)  ;; highligh lines when in command mode
    ;; (setq overriding-local-map clavis-command-mode-keymap)
    (set-keymap-parent clavis-key-map clavis-command-mode-keymap)
    (run-hooks 'clavis-command-mode-activate-hook)
    (setq clavis-insert-state-q nil))


  (defun clavis-insert-mode-activate ()
    "Activate insertion mode."
    (interactive)
    (modify-all-frames-parameters (list (cons 'cursor-type 'bar)))
    (global-hl-line-mode 0)  ;; do not highlight lines it insert mode
    ;; (setq overriding-local-map nil)
    (set-keymap-parent clavis-key-map clavis-insert-mode-keymap)
    (run-hooks 'clavis-insert-mode-activate-hook)
    (setq clavis-insert-state-q t))



;; when going into minibuffer, switch to insertion mode.
(add-hook 'minibuffer-setup-hook 'clavis-insert-mode-activate)
(add-hook 'minibuffer-setup-hook 'clavis-deactivate-input-method t)
(add-hook 'minibuffer-exit-hook 'clavis-command-mode-activate)

;; when in shell mode, switch to insertion mode.
(add-hook 'shell-mode-hook 'clavis-deactivate-input-method t)
(add-hook 'shell-mode-hook 'clavis-insert-mode-activate)




  ;; Preserve input method when switching to command mode
  (setq clavis-insert-mode-input-method nil)  ;; assume default input method at begining
  (setq clavis-input-method-lighter " ξ") ;; this is for the minor mode lighter

  (defun clavis-insert-mode-toggle-input-method ()
    (activate-input-method clavis-insert-mode-input-method))

  (defun clavis-command-mode-preserve-input-method ()
    (when clavis-insert-state-q
      (setq clavis-insert-mode-input-method current-input-method))
    (deactivate-input-method))


  (add-hook 'clavis-command-mode-activate-hook  'clavis-command-mode-preserve-input-method)
  (add-hook 'clavis-insert-mode-activate-hook  'clavis-insert-mode-toggle-input-method)


  (defun clavis-toggle-input-method ()
    (interactive)
    ;; change to input mode when toggling input method
    (unless clavis-insert-state-q
      (clavis-insert-mode-activate))
    ;; change language spelling and lighter
    (if current-input-method
        (clavis-deactivate-input-method)
      (clavis-activate-input-method)))

(defun clavis-activate-input-method ()
  (interactive)
  (clavis-set-ispell-russian)
  (activate-input-method default-input-method)
  (setq current-input-method-title nil)
  (setq clavis-input-method-lighter #(" ѣ" 1 2  (face org-formula))))

(defun clavis-deactivate-input-method ()
  (interactive)
  (clavis-set-ispell-english)
          (deactivate-input-method)
          (setq current-input-method-title nil)
          (setq clavis-input-method-lighter #(" ξ" 1 2  (face org-formula))))



  ;; minor-mode-alist

  ;; Define Clavis (minor) Mode
  (define-minor-mode clavis
    "A personalized modal keybinding set, like vim, but based on ergonomic principles, like Dvorak layout and personal preferences. Modified version of xah-fly-keys (URL `http://ergoemacs.org/misc/ergoemacs_vi_mode.html')"
    :init-value nil
    :global t
    :lighter (:eval clavis-input-method-lighter) ;; ξѣѢѮ£₽⦾
    :keymap clavis-key-map
    (add-to-list 'emulation-mode-map-alists
                 '((cons 'clavis-keys clavis-key-map)))
    (clavis-insert-mode-activate))

  (provide 'clavis)
  (clavis t)
