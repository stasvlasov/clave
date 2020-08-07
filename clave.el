;; define hooks
(defvar clave-on-hook nil
  "Hook for `clave-on'")
(defvar clave-off-hook nil
  "Hook for `clave-off'")

;; define keymaps
(defvar clave-minor-mode-map (make-sparse-keymap)
  "Keybinding for `clave' minor mode.")

(defvar clave-map (make-sparse-keymap)
  "Keybinding for `clave' command mode.")

;; (setq clave-map (make-sparse-keymap))

;; define variables
(defvar clave--off-func '(lambda ())
  "Function to remove map.")

(defvar clave-on-p nil "Clave state.")

(defvar clave-lighter " λ"  ;; ξѣѢѮ£₽⦾λ
  "Default mode line lighter for minor mode `clave'")

(defvar clave-map-init-standard-keys
  '(
    ;; Numbers are Workman's difficulty of keys. The higher number the worse.
    ;; https://web.archive.org/web/20170311215135/http://www.workmanlayout.com/blog/wp-content/uploads/2010/10/keyboard_graded1.png
    ;; Numbers row 
    "`" "~"
    "1" "!"
    "2" "@"
    "3" "#"
    "4" "$"
    "5" "%"
    "6" "^"
    "7" "&"
    "8" "*"
    "9" "("
    "0" ")"
    "-" "_"
    "=" "+"
    ;; LEFT HAND 
    ;; - Top row
    "q" "Q"  ; 4
    "w" "W"  ; 2
    "e" "E"  ; 2
    "r" "R"  ; 3
    "t" "T"  ; 4
    ;; - Home row
    "a" "A"  ; 1.5
    "s" "S"  ; 1
    "d" "D"  ; 1
    "f" "F"  ; 1
    "g" "G"  ; 3
    ;; - Bottom row
    "z" "Z"  ; 4
    "x" "X"  ; 4
    "c" "C"  ; 3
    "v" "V"  ; 2
    "b" "B"  ; 5
    ;; RIGHT HAND
    ;; - Top row
    "y" "Y"  ; 5
    "u" "U"  ; 3
    "i" "I"  ; 2
    "o" "O"  ; 2
    "p" "P"  ; 4
    "[" "{"  
    "]" "}"  
    "\\" "|" 
    ;; - Home row
    "h" "H"  ; 3
    "j" "J"  ; 1
    "k" "K"  ; 1
    "l" "L"  ; 1
    ";" ":"  ; 1.5
    "'" "\""
    ;; - Bottom row
    "n" "N"  ; 3
    "m" "M"  ; 2
    "," "<"  ; 3
    "." ">"  ; 4
    "/" "?"  ; 4
    ))

(defvar clave-map-init-standard-extra-keys
  '("TAB"
    "RET"
    "SPC"
    "DEL"))

;;;###autoload
(defmacro clave-map-init (map-name &optional extra-keys keys)
  "Creates MAP-NAME keymap if is does not exist and binds dummy commands to KEYS. If KEYS is nil use `clave-map-init-standard-keys' list of keys instead. If EXTRA-KEYS are set it creates extra dummy functions and binds it to the end of MAP-NAME keymap. If EXTRA-KEYS is set to t it uses `clave-map-init-standard-keys' list as extra keys. Both KEYS and EXTRA-KEYS should be list of valid `kbd' arguments.
Examples:
    ;; binds `clave-map-init-standard-keys' to my-map
    (clave-map-init 'my-map)
    ;; binds `clave-map-init-standard-keys' and `clave-map-init-standard-extra-keys'
    (clave-map-init 'my-map t)
    ;; add one extra key
    (clave-map-init 'my-map '(\"ESC\"))
    ;; binds one key
    (clave-map-init 'my-map nil '(\"ESC\"))"
  `(progn
     ;; check keymap
     (unless (boundp ,(eval map-name))
       (defvar ,(eval map-name) (make-sparse-keymap)))
     ;; define dummy functions
     ,@(mapcar
        (lambda (key)
          (let ((func-name
                 (make-symbol (concat (symbol-name (eval map-name)) "-" key))))
            `(progn
               (defun ,func-name ()
                 "This is clave dummy command meant for rebinding."
                 (interactive))
               (define-key ,(eval map-name) (kbd ,key) (quote ,func-name)))))
        (append (or (eval keys)
                    clave-map-init-standard-keys)
                (when extra-keys
                  (if (equal extra-keys t)
                      clave-map-init-standard-extra-keys
                    (eval extra-keys)))))))

;; populate clave-map with dummy commands
(clave-map-init 'clave-map)

;; functions
(defun clave-on-indicate ()
  "Indicate clave on state."
  (modify-all-frames-parameters (list (cons 'cursor-type 'box)))
  (global-hl-line-mode 1))

(defun clave-off-indicate ()
  "Indicate clave on state."
  (modify-all-frames-parameters (list (cons 'cursor-type 'bar)))
  (global-hl-line-mode 0))

;;;###autoload
(defun clave-on ()
  "Activate `clave' command mode."
  (interactive)
  (setq clave--off-func
        (set-transient-map clave-map (lambda () t)))
  (setq clave-on-p t)
  (clave-on-indicate)
  (run-hooks 'clave-on-hook))

;;;###autoload
(defun clave-off ()
  "Activate `clave' insertion mode."
  (interactive)
  (funcall clave--off-func)
  (setq clave-on-p nil)
  (clave-off-indicate)
  (run-hooks 'clave-off-hook))

;; we need an escape from clave-on
(define-key clave-map (kbd "SPC") 'clave-off)

;; clave minor mode
(defun clave-set-hooks ()
  "Sets hooks for `clave' minor mode states"
  (add-hook 'minibuffer-setup-hook 'clave-off)
  (add-hook 'shell-mode-hook 'clave-)
  (add-hook 'minibuffer-exit-hook 'clave-command-mode-activate))

(defun clave-unset-hooks ()
  "Unets hooks for `clave' minor mode states. Used for turning `clave' minor mode of."
  (remove-hook 'minibuffer-setup-hook 'clave-off)
  (remove-hook 'shell-mode-hook 'clave-)
  (remove-hook 'minibuffer-exit-hook 'clave-command-mode-activate))

;;;###autoload
(define-minor-mode clave
  "A personalized modal keybinding set, like vim, but based on ergonomic principles, like Dvorak layout and personal preferences. Inspired by xah-fly-keys (URL `http://ergoemacs.org/misc/ergoemacs_vi_mode.html')"
  :init-value nil
  :global t
  :lighter (:eval clave-lighter) 
  :keymap clave-minor-mode-map
  (if clave
      (progn
        (clave-off-indicate)
        (clave-map-init 'clave-map)
        (clave-set-hooks))
    (progn
      (clave-unset-hooks)
      (clave-off))))

(provide 'clave)

;; (clave t)

;; clave.el ends here
