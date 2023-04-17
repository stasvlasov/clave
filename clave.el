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

(defvar clave-lighter-off nil
  "Directly disable lighter for minor mode `clave'. Other hacks (e.g., blackout.el) might not work.")

(defvar clave-input-method nil "Stores current-input-method in clave-off state.")

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

(defvar clave-keys nil
  "List of keybindings description in form of (ACTIVE-MAP CLAVE-MAP KEY COMMAND TYPE LABEL) defined with `clave-remap-key'.")

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
     (unless (boundp ,map-name)
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

(defmacro clave-remap-key (active-map clave-map key command &optional type label)
  "Remaps dummy CLAVE-MAP-KEY function to COMMAND in ACTIVE-MAP and appends (ACTIVE-MAP CLAVE-MAP KEY COMMAND TYPE LABEL) to `clave-keys' list.

If CLAVE-MAP does not exist at evaluation then it is initialized by `clave-init-map' with  `clave-map-init-standard-extra-keys'. If command is unquoted symbol then it is assumed to be a keymap which is bind directly to key (without remapping) as there is no known mechanism to remap command to keymap."
  (let* ((clave-map-name (if clave-map (symbol-name clave-map) "clave-map"))
         (clave-func (make-symbol (concat clave-map-name "-" key)))
         ;; the below I learned from xah-fly-keys and bind-key.el 
         ;; it is meant to pass keymap symbol to define-key and not the map itself
         (clave-map-var (make-symbol "clave-map-name"))
         (active-map-var (make-symbol "active-map-name"))
         (command-var (make-symbol "command-name"))
         type-keymap)
    `(let ((,active-map-var ,active-map))
       (unless (boundp (quote ,clave-map)) 
         (clave-map-init (quote ,clave-map) t))
       ,(if (symbolp command)
            (if active-map
                (error "Clave: Cannot bind `%s' prefix map to non clave map `%s'! If it is command and not prefix map then quote it." (symbol-name command) (symbol-name active-map))
              `(progn 
                 (unless (boundp (quote ,command))
                   (clave-map-init (quote ,command) t))
                 (let ((,command-var ,command)
                       (,clave-map-var ,clave-map))
                   (define-key ,clave-map-var ,key ,command-var))))
          (if active-map
              `(define-key ,active-map-var [remap ,clave-func] ,command)
            `(global-set-key [remap ,clave-func] ,command)))
       (add-to-list 'clave-keys '(,(if active-map
                                       (symbol-name active-map)
                                     "global-map")
                                  ,clave-map-name
                                  ,key
                                  ;; get command as string
                                  ,(if (symbolp command)
                                       ;; if not 'command then it must be keymap
                                       (progn (setq type-keymap "keymap")
                                              (symbol-name command))
                                     ;; if 'command it is command
                                     (symbol-name (eval command)))
                                  ,(or type type-keymap)
                                  ,label)))))

;; test
;; (clave-remap-key (package-map . package) nil "a" 'a-func)
;; (clave-remap-key package-map nil "a" 'a-func)
;; (clave-remap-key nil clave-other-map "a" 'a-func)
;; (clave-remap-key nil clave-other-map "a" a-func)
;; (clave-remap-key package-map clave-other-map "a" a-func)
;; (clave-remap-key org-map nil "RET" 'a-func "edit" "✖")

(defun clave-remap-normalize-args (args &optional for-use-package)
  "Checks if the ARGS are fine and normalize them into list of bindings descriptions for `clave-remap-key' macro as follows (ACTIVE-MAP CLAVE-MAP KEY COMMAND TYPE LABEL)."
  ;; harmonize between (("a" b)) and ("a" b) args
  (unless (cdr args) (setq args (car args)))
  (let (param-active-map
        param-clave-map
        param-bind-after
        return-args)
    (while args
      (let ((x (car args)))
        (pcase x
          ((or 
            ;; (KEY BINDING)
            `(,(pred stringp) ,_)
            ;; (KEY BINDING TYPE)
            `(,(pred stringp) ,_ ,_)
            ;; (KEY BINDING TYPE LABEL)
            `(,(pred stringp) ,_ ,_ ,_))
           ;; return list of (MAP CLAVE-MAP KEY BINDING &optional TYPE LABEL)
           ;; for use-package return list of (BIND-AFTER MAP CLAVE-MAP KEY BINDING &optional TYPE LABEL)
           (setq return-args
                 (append return-args
                         (list 
                          (append
                           (when for-use-package
                             (list param-bind-after))
                           (list param-active-map)
                           (list param-clave-map)
                           x))))
           (setq args (cdr args)))
          ;; keywords
          (':active-map
           (setq param-active-map (cadr args))
           ;; reset param-clave-map to default map
           (setq param-clave-map nil)
           (setq args (cddr args)))
          (':bind-after
           (setq param-bind-after (cadr args))
           (setq args (cddr args)))
          (':clave-map
           (setq param-clave-map (cadr args))
           (setq args (cddr args)))
          ;; skip value
          (_ 
           (warn "clave-remap-normalize-args: Do not know how to process '%s' keyword." x)
           (setq args (cdr args))))))
    ;; return list
    return-args))

;; (clave-remap-normalize-args
;;  '(("a" 'a-func) ;; remaps clave-map-a to a-func in global-map
;;    ("b" 'b-func) ;; remaps clave-map-b to b-func in global-map
;;    :active-map my-map
;;    ("c" 'c-func) ;; remaps clave-map-b to b-func in global-map
;;    :clave-map clave-a-map
;;    ("b" 'b-func)
;;    :bind-after c
;;    :active-map c-map
;;    ("c" 'c-func)) t)



(defmacro clave-remap (&rest args)
  "Remaps clave keys (clave dummy functions) to commands. The ARGS should be a list of following elements:
  - binding description (KEY COMMAND &optional TYPE LABEL)
  - :active-map keyword followed by symbol (unquoted)
  - :clave-map keyword followed by symbol (unquoted)
  - :bind-after keyword followed by symbol (unquoted) - (similar to eval-after-load for bindings to keymaps that are not nessesary loaded)

  Order matters: First it remaps dummy-functions clave-map-KEY from default `clave-map' to `global-map'. Everything after :active-map specification binds to that map until next :active-map specification. Similar for :clave-map specification albeit it tells from which clave map which clave dummy function to bind (see `clave-map-init' for details).

  Example:
  (clave-remap
    (\"a\" 'a-func) ;; remaps clave-map-a to a-func in global-map
    (\"b\" 'b-func) ;; remaps clave-map-b to b-func in global-map
    :active-map my-map
    (\"c\" 'c-func) ;; remaps clave-map-b to b-func in global-map
    :clave-map clave-a-map
    (\"b\" 'b-func)
    :active-map c-map
    (\"c\" 'c-func))

  The each remap specification when processed passed to `clave-remap-key' macro."
  (macroexp-progn
   (mapcar
    (lambda (arg) `(clave-remap-key ,@arg))
    (clave-remap-normalize-args args))))

;; (clave-remap
;; ( ("a" 'a-func) ;; remaps clave-map-a to a-func in global-map
;;  ("b" 'b-func) ;; remaps clave-map-b to b-func in global-map
;;  :active-map my-map
;;  ("c" 'c-func) ;; remaps clave-map-b to b-func in global-map
;;  :clave-map clave-a-map
;;  ("b" 'b-func)
;;  :active-map c-map
;;  ("c" 'c-func)))




;; functions

(defun clave-change-terminal-cursor-to-box ()
  "Change terminal cursor to box. Same as typing echo -e '\e[2 q' in the terminal"
  (send-string-to-terminal "\033[2 q"))

(defun clave-change-terminal-cursor-to-bar ()
  "Change terminal cursor to bar. Same as typing echo -e '\e[6 q' in the terminal"
  (send-string-to-terminal "\033[6 q"))


(defvar clave-on-indicate-cursor 'box)
(defvar clave-off-indicate-cursor 'bar)

(defun clave-on-indicate ()
  "Indicate clave on state."
  (if (display-graphic-p)
      (modify-all-frames-parameters
       (list (cons 'cursor-type clave-on-indicate-cursor)))
    (clave-change-terminal-cursor-to-box))
  (global-hl-line-mode 1))

(defun clave-off-indicate ()
  "Indicate clave off state."
  (if (display-graphic-p)
      (modify-all-frames-parameters
       (list (cons 'cursor-type clave-off-indicate-cursor)))
    (clave-change-terminal-cursor-to-bar))
  (global-hl-line-mode 0))

(defun clave-on ()
  "Activate `clave' command mode."
  (interactive)
  ;; preserve input method
  (setq clave-input-method current-input-method)
  (deactivate-input-method)
  ;; activate clave-map
  (setq clave--off-func
        (set-transient-map clave-map (lambda () t)))
  (setq clave-on-p t)
  (clave-on-indicate)
  (run-hooks 'clave-on-hook))

(defvar clave-toggle-input-method-if-on-off t
  "Whether to toggle input method if clave was turned off right after it was turned on")

(defun clave-off ()
  "Activate `clave' insertion mode."
  (interactive)
  (funcall clave--off-func)
  (setq clave-on-p nil)
  ;; restore input method
  (when clave-input-method
    (activate-input-method clave-input-method))
  ;; switch input method on clave on-off
  (when (and
         clave-toggle-input-method-if-on-off
         (equal last-command 'clave-on))
    ;; (toggle-input-method)
    (sv-toggle-input-method))
  (clave-off-indicate)
  (run-hooks 'clave-off-hook))  

;; we need an escape from clave-on
(define-key clave-map (kbd "SPC") 'clave-off)

;; clave minor mode
(defun clave-set-hooks ()
  "Sets hooks for `clave' minor mode states"
  (add-hook 'minibuffer-setup-hook 'clave-off)
  (add-hook 'shell-mode-hook 'clave-off)
  (add-hook 'minibuffer-exit-hook 'clave-on)
  (add-hook 'isearch-mode-end-hook 'clave-on))

(defun clave-unset-hooks ()
  "Unets hooks for `clave' minor mode states. Used for turning `clave' minor mode of."
  (remove-hook 'minibuffer-setup-hook 'clave-off)
  (remove-hook 'shell-mode-hook 'clave-off)
  (remove-hook 'minibuffer-exit-hook 'clave-on)
  (remove-hook 'isearch-mode-end-hook 'clave-on))

       ;;;###autoload
(define-minor-mode clave
  "A personalized modal keybinding set, like vim, but based on ergonomic principles, like Dvorak layout and personal preferences. Inspired by xah-fly-keys (URL `http://ergoemacs.org/misc/ergoemacs_vi_mode.html')"
  :init-value nil
  :global t
  :lighter (:eval (unless clave-lighter-off clave-lighter))
  :keymap clave-minor-mode-map
  (if clave
      (progn (clave-off-indicate)
             (clave-set-hooks))
    (progn (clave-unset-hooks)
           (clave-off))))

;; clave use-package integration

;;add :remap keyword
(require 'seq)
(setq use-package-keywords 
      (append
       (seq-take-while (lambda (el) (not (equal el :bind))) use-package-keywords)
       '(:remap)
       (seq-drop-while (lambda (el) (not (equal el :bind))) use-package-keywords)))

;; (add-to-list 'use-package-keywords :remap)
;; (setq use-package-keywords (remove ':remap use-package-keywords))


(defun use-package-normalize/:remap (name keyword args)
  "Checks if the argumets are fine. See `clave-remap' for expected ARGS and how it is processed."
  (clave-remap-normalize-args args 'for-use-package))

    ;;;; test

;; (use-package-normalize/:remap nil nil '(("a" 'sdf "asdf")
;;                                         :active-map aaa
;;                                         :clave-map clave-org
;;                                         ("a" 'sdf "asdf" "sadf")
;;                                         ("a" 'sdf "asdf")
;;                                         :bind-after bbb
;;                                         :active-map bbb-map
;;                                         ("a" 'sdf "asdf" "sadf")))


;; (use-package-normalize/:remap nil nil '((("a" 'sdf "asdf")
;; 					 :clave-map clave-org-zero
;; 					("a" 'sdf "asdf" "saf")
;;                                         ("a" 'sdf "asdf")
;;                                          :active-map aaa
;;                                         :clave-map clave-org
;;                                         ("a" 'sdf "asdf" "sadf")
;;                                         ("a" 'sdf "asdf")
;;                                         :active-map bbb
;;                                         ("a" 'sdf "asdf" "sadf"))))

(defun use-package-handler/:remap (name _keyword args rest state)
  (use-package-concat
   (use-package-process-keywords name rest state)
   `(,@(mapcar #'(lambda (clave-remap-args)
                   (pcase-let
                       ((`(,bind-after ,active-map ,clave-map ,key ,command ,type ,label)
                         clave-remap-args))
                     (if active-map
                         (if bind-after
                             `(eval-after-load (quote ,bind-after)
                                '(progn
                                  (unless (or (keymapp ,command)
                                              (fboundp ,command))
                                    (autoload ,command ,(symbol-name name) nil t))
                                  (clave-remap-key ,@(cdr clave-remap-args))))
                           `(eval-after-load (quote ,name)
                              '(clave-remap-key ,@(cdr clave-remap-args))))
                       `(progn
                          (unless (or (keymapp ,command)
                                      (fboundp ,command))
                            (autoload ,command ,(symbol-name name) nil t))
                          (clave-remap-key ,@(cdr clave-remap-args))))))
               args))))

;; (use-package pack
;;   :remap
;;   (:clave-map clave-map
;;    ("a" 'a-func)
;;    ("b" 'b-func)
;;    ("f" clave-files-map)
;;    :active-map a-map
;;    ("c" 'c-func)
;;    :clave-map clave-a-map
;;    ("d" 'd-func)
;;    ("d" d-map)
;;    :bind-after b-mode
;;    :active-map b-map
;;    ("c" 'c-func)
;;    :clave-map clave-a-map
;;    ("d" 'd-func)
;;    ("d" d-map)
;;    ))

;; clave visualizatoin with KLE

(defvar clave-keys-colors
      (seq-reverse '(
"#B55762"
"#BB5D5B"
"#C16E60"
"#C77F64"
"#CD9168"
"#D2A36C"
"#D8B570"
"#DEC875"
"#E3DB79"
"#E4E97E"
"#DBEE82"
"#CCEC8B"
"#C0EA94"
"#B7E89D"
"#B2E7A5"
"#AFE6AD"
"#B5E5BA"
"#BCE5C7"
"#C3E5D1"
"#C9E5DA"
"#D0E6E0"
)))

(defun clave-clm-count-commands (logs-regex commands)
  ;; here I can get a dates diapason from user
  (when-let* ((sv-clm/logging-dir-p (boundp 'sv-clm/logging-dir))
              ;; get all files in form YYYY-MM-DD
              (files (directory-files-recursively sv-clm/logging-dir logs-regex)))
    (defun count-command (command)
      (beginning-of-buffer)
      (setq count 0)
      (while (search-forward (concat " " command "\n") nil t)
        (setq count (1+ count)))
      count)
    (with-temp-buffer
      ;; insert all files
      (mapcar 'insert-file-contents files)
      (mapcar 'count-command commands))))

;; test
;; (clave-clm-count-commands "2020-[0-9\\\\-]+" '("next-line" "org-clock-goto"))

(defun clave-kle-make-key-labels (clave-keys
                                  logs-regex
                                  clave-map-filter
                                  active-map-filter
                                  &optional
                                  log-counts)
  (defun maps-match-p (key-description)
    (pcase-let ((`(,active-map ,clave-map) key-description))
      (and (string= clave-map clave-map-filter)
           (string= active-map active-map-filter))))
  (defun get-key-color (count)
    (let*  ((step (/ (- (seq-max keys-counts) (seq-min keys-counts))
                     (- (length clave-keys-colors) 1)))
            (color-index (round (/ count step))))
      (nth color-index clave-keys-colors)))
  (defun log+ (count) (log (1+ count)))
  ;; set defaults
  (let* (;; filter keymaps
         (keys (seq-filter 'maps-match-p clave-keys))
         (keys-commands
          (mapcar (lambda (x) (nth 3 x)) keys))
         (keys-counts
          (clave-clm-count-commands logs-regex keys-commands))
         (keys-counts
          (if log-counts (mapcar 'log+ keys-counts) keys-counts))
         (keys-colors
          (mapcar 'get-key-color keys-counts)))
    (defun make-key-label (key-description key-color)
      (pcase-let
          ((`(,active-map ,clave-map ,key ,command ,type ,label)
            key-description))
        (list key
              (concat
               (when key-color
                 ;; # is %23
                 (concat "&_c=%23" (substring key-color 1) "%3B"))
               "&="
               (url-encode-url
                (concat
                 (clave-kle-encode-url (if label label command))
                 "\n\n\n\n\n\n\n\n\n\n\n"))))))
    (seq-mapn 'make-key-label
              keys
              keys-colors)))

;; test
;; (clave-kle-make-key-labels clave-keys "2020-08")

(defvar clave-kle-encode-url-chars
      '(("/"   "%2F%2F")
        ("="   "%2F=")
        (";"   "%2F%3B")
        ("`"   "%60" )
        ("#"   "%23")
        ("\""  "%22" )
        ("["   "%5B" )
        ("]"   "%5D" )
        ("\\"  "%5C" )))

(defun clave-kle-encode-url (str &optional chars)
  (let ((chars (if chars chars
                 (when (boundp 'clave-kle-encode-url-chars)
                   clave-kle-encode-url-chars))))
    (while (setq char (pop chars))
      (setq str
            (replace-regexp-in-string
             (regexp-quote (car char)) (cadr char) str nil 'literal)))
    str))

;; test
;; (clave-kle-encode-url  "/asdf=")



(defun clave-kle-decode-url (str &optional chars)
  (let ((chars (if chars chars
                 (when (boundp 'clave-kle-encode-url-chars)
                   clave-kle-encode-url-chars))))
    (while (setq char (pop chars))
      (setq str
            (replace-regexp-in-string
             (regexp-quote (cadr char)) (car char) str nil 'literal)))
    str))


;; (clave-kle-decode-url "%22")

(defvar clave-kle-url
      "http://www.keyboard-layout-editor.com/##@@_f:1&a:3%3B&=ESC&=F1&=F2&=F3&=F4&=F5&=F6&=F7&=F8&=F9&=F10&=F11&=F12&=NmLk&=ScrLk&=Insert%3B&@=%60&=1&=2&=3&=4&=5&=6&=7&=8&=9&=0&=-&=%2F=&_w:2%3B&=DEL&=Home%3B&@_w:1.5%3B&=TAB&=q&=w&=e&=r&=t&=y&=u&=i&=o&=p&=%5B&=%5D&_w:1.5%3B&=%5C&=Page%20Up%3B&@_w:1.75%3B&=Caps%20Lock&=a&=s&=d&=f&=g&=h&=j&=k&=l&=%2F%3B&='&_w:2.25%3B&=RET&=Page%20Down%3B&@_w:2.25%3B&=Shift&=z&=x&=c&=v&=b&=n&=m&=,&=.&=%2F%2F&_w:1.75%3B&=Shift&=%E2%86%91&=End%3B&@_w:1.25%3B&=Ctrl&_w:1.25%3B&=Win&_w:1.25%3B&=Alt&_w:6.25%3B&=SPC&=Alt&=Fn&=Ctrl&=%E2%86%90&=%E2%86%93&=%E2%86%92")


;; http://www.keyboard-layout-editor.com
;; @_w:1.5 - properties and ends with ; (%3B)
;; &@=%60 - new line
;; search for keys between "&=" and "%3B"
;; For each matched KEY I need to insert my commad and add "\n\n\n\n\nKEY"

(defun clave-kle-make-url (&optional clave-map-filter active-map-filter)
  (let ((clave-map-filter (if clave-map-filter
                              clave-map-filter
                            "clave-map"))
        (active-map-filter (if active-map-filter
                               active-map-filter
                             "global-map")))
    (with-temp-buffer
      (insert clave-kle-url)
      (beginning-of-buffer)
      (while (search-forward "&=" nil t)
        (mapcar
         (lambda (key-description)
           (pcase-let
               ((`(,active-map ,clave-map ,key ,command ,type ,label)
                 key-description))
             (when (and (string= active-map active-map-filter)
                        (string= clave-map clave-map-filter)
                        (or (looking-at
                             (regexp-quote
                              (concat key "&=")))
                            (looking-at
                             (regexp-quote
                              (concat key "%3B")))))
               (insert (url-encode-url
                        (concat
                         (if label label command)
                         "\n\n\n\n\n"))))))
         clave-keys))
      (buffer-string))))

(defvar clave-kle-default-key-color "#cccccc")

;; new
(defun clave-kle-make-url
    (&optional logs-regex clave-map-filter active-map-filter log-counts)
  (let ((colorful-commands
         (clave-kle-make-key-labels
          clave-keys logs-regex clave-map-filter active-map-filter log-counts))
        (clave-kle-default-key-color
         (clave-kle-encode-url clave-kle-default-key-color))
        ;; toggle case sensitive search
        (case-fold-search nil)
        colorful-command)
    (with-temp-buffer
      (insert clave-kle-url)
      ;; TODO: insert title (maps names)
      (beginning-of-buffer)
      (search-forward "/##@")
      (insert (url-encode-url (concat "_name=" clave-map-filter
                      " from " active-map-filter
                      "%3B&")))
      (while (setq colorful-command (pop colorful-commands))
        (beginning-of-buffer)
        (when (re-search-forward
               (concat "&=\\("
                       (regexp-quote (clave-kle-encode-url (car colorful-command)))
                       "\\)\\(&=\\|%3B\\|&_\\)")
               ;; "&=" starts key label
               ;; "%3B" starts new line
               ;; "&_" starts property (w is width)
               nil t)
          ;; the easiest is to wrap into colors!
          ;; first insert default color as next
          (goto-char (match-beginning 2))
          (insert (concat "&_c=" clave-kle-default-key-color "%3B"))
          ;; then go to begginning
          (goto-char (match-beginning 0))
          ;; remove &= as it will be in colorful-commands
          (delete-char 2)
          ;; insert key description and color
          (insert (cadr colorful-command))))
      (buffer-string))))


(defun clave-kle-show (&optional logs-regex clave-map-filter active-map-filter log-counts)
  (interactive
   (list (read-regexp "Filter log files by regex:" "2020-")
         (completing-read
          "Chose clave keymap to visualize:"
          (delete-dups (mapcar 'cadr clave-keys)))
         (completing-read
          "Chose context keymap to visualize:"
          (delete-dups (mapcar 'car clave-keys)))
         (y-or-n-p "Log the counts")))
  (browse-url
   (clave-kle-make-url logs-regex clave-map-filter active-map-filter log-counts)))

;; (clave-kle-make-url)

(provide 'clave)

;; clave.el ends here
