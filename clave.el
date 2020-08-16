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
  "Remaps dummy CLAVE-MAP-KEY function to COMMAND in ACTIVE-MAP and appends (ACTIVE-MAP CLAVE-MAP KEY COMMAND TYPE LABEL) to `clave-keys' list. If CLAVE-MAP does not exist at evaluation then it is initialized by `clave-init-map' with  `clave-map-init-standard-extra-keys'. If command is unquoted symbol then it is assumed to be a keymap which is bind directly to key (without remapping) as there is no known mechanism to remap command to keymap."
  (let* ((clave-map-name (if clave-map (symbol-name clave-map) "clave-map"))
	 (clave-func (make-symbol (concat clave-map-name "-" key)))
	 (clave-map-var (make-symbol "clave-map-name"))
	 (active-map-var (make-symbol "active-map-name"))
	 (command-var (make-symbol "command-name"))
	 type-keymap)
    `(let ((,active-map-var ,active-map))
       (unless (boundp (quote ,clave-map)) 
	 (clave-map-init (quote ,clave-map) t))
       ,(if (symbolp command)
	    (if active-map (error "Clave: Cannot bind `%s' prefix map to non clave map `%s'! If it is command and not prefix map then quote it."
				  (symbol-name command)
				  (symbol-name active-map))
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
;; (clave-remap-key package-map nil "a" a-func)
;; (clave-remap-key nil clave-other-map "a" 'a-func)
;; (clave-remap-key nil clave-other-map "a" a-func)
;; (clave-remap-key package-map clave-other-map "a" a-func)
;; (clave-remap-key org-map nil "RET" 'a-func "edit" "✖")

(defun clave-remap-normalize-args (args)
  "Checks if the ARGS are fine and normalize them into list of bindings descriptions for `clave-remap-key' macro as follows (ACTIVE-MAP CLAVE-MAP KEY COMMAND TYPE LABEL)."
  ;; harmonize between (("a" b)) and ("a" b) args
  (unless (cdr args) (setq args (car args)))
  (let (param-map
	param-clave-map
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
	   (setq return-args
		 (append return-args
			 (list (append (list param-map) (list param-clave-map) x))))
	   (setq args (cdr args)))
	  ;; keywords
	  ((or ':map ':active-map)
	   (setq param-map (cadr args))
	   ;; reset param-clave-map to default map
	   (setq param-clave-map nil)
	   (setq args (cddr args)))
	  (':clave-map
	   (setq param-clave-map (cadr args))
	   (setq args (cddr args)))
	  ;; skip value
	  (_ (setq args (cdr args))))))
    ;; return list
    return-args))

;; (clave-remap-normalize-args
;;  '(("a" 'a-func) ;; remaps clave-map-a to a-func in global-map
;;    ("b" 'b-func) ;; remaps clave-map-b to b-func in global-map
;;    :active-map my-map
;;    ("c" 'c-func) ;; remaps clave-map-b to b-func in global-map
;;    :clave-map clave-a-map
;;    ("b" 'b-func)
;;    :active-map c-map
;;    ("c" 'c-func)))

(defmacro clave-remap (&rest args)
  "Remaps clave keys (clave dummy functions) to commands. The ARGS should be a list of following elements:
  - binding description (KEY COMMAND &optional TYPE LABEL)
  - :active-map keyword followed by symbol (unquoted)
  - :clave-map keyword followed by symbol (unquoted)

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
(defun clave-on-indicate ()
  "Indicate clave on state."
  (modify-all-frames-parameters (list (cons 'cursor-type 'box)))
  (global-hl-line-mode 1))

(defun clave-off-indicate ()
  "Indicate clave on state."
  (modify-all-frames-parameters (list (cons 'cursor-type 'bar)))
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

(defun clave-off ()
  "Activate `clave' insertion mode."
  (interactive)
  (funcall clave--off-func)
  (setq clave-on-p nil)
  ;; restore input method
  (when clave-input-method
    (activate-input-method clave-input-method))
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
  :lighter (:eval clave-lighter) 
  :keymap clave-minor-mode-map
  (if clave
      (progn (clave-off-indicate)
	     (clave-set-hooks))
    (progn (clave-unset-hooks)
	   (clave-off))))

(provide 'clave)

;; clave.el ends here

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
      (clave-remap-normalize-args args))

    ;;;; test

    ;; (use-package-normalize/:remap nil nil '(("a" 'sdf "asdf")
    ;;                                         :active-map aaa
    ;;                                         :clave-map clave-org
    ;;                                         ("a" 'sdf "asdf" "sadf")
    ;;                                         ("a" 'sdf "asdf")
    ;;                                         :active-map bbb
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
			   ((`(,active-map ,clave-map ,key ,command ,type ,label)
			     clave-remap-args))
			 (if active-map
			     (progn
			       `(eval-after-load (quote ,name)
				  '(clave-remap-key ,@clave-remap-args)))
			   `(progn
			      (unless (or (keymapp ,command)
					  (fboundp ,command))
				(autoload ,command ,(symbol-name name) nil t))
			      (clave-remap-key ,@clave-remap-args)))))
		   args))))

    ;; (use-package pack
    ;;     :remap (:clave-map clave-map
    ;; 	    ("a" 'a-func)
    ;; 	    ("b" 'b-func)
    ;; 	     ("f" clave-files-map)
    ;; 	    :map a-map
    ;; 	    ("c" 'c-func)
    ;; 	    :clave-map clave-a-map
    ;; 	    ("d" 'd-func)
    ;; 	     ("d" d-map)
    ;; 	    )
    ;;   :bind ("a" . a-func)
    ;;   :config (lala)
    ;;   )

(defvar clave-kle-url "http://www.keyboard-layout-editor.com/##@@_f:1&a:3%3B&=Esc%0A%0A%0A%0A%0A1&=F1&=F2&=F3&=F4&=F5&=F6&=F7&=F8&=F9&=F10&=F11&=F12&=PrtSNmLk&=PausScrLk&=DeletInsert%3B&@=%60&=1&=2&=3&=4&=5&=6&=7&=8&=9&=0&=-&=%2F=&_w:2%3B&=DEL&=Home%3B&@_w:1.5%3B&=Tab&=Q&=W&=E&=R&=T&=Y&=U&=I&=O&=P&=%5B&=%5D&_w:1.5%3B&=%5C&=Page%20Up%3B&@_w:1.75%3B&=Caps%20Lock&=A&=S&=D&=F&=G&=H&=J&=K&=L&=%2F%3B&=%22&_w:2.25%3B&=RET&=Page%20Down%3B&@_w:2.25%3B&=Shift&=Z&=X&=C&=V&=B&=N&=M&=,&=.&=%2F%2F&_w:1.75%3B&=Shift&=%E2%86%91&=End%3B&@_w:1.25%3B&=Ctrl&_w:1.25%3B&=Win&_w:1.25%3B&=Alt&_w:6.25%3B&=SPC&=Alt&=Fn&=Ctrl&=%E2%86%90&=%E2%86%93&=%E2%86%92")


;; http://www.keyboard-layout-editor.com
;; @_w:1.5 - properties and ends with ;
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
	     (when (and (string= clave-map clave-map-filter)
			(string= active-map active-map-filter)
			(or (looking-at
			     (regexp-quote
			      (concat key "&=")))
			    (looking-at
			     (regexp-quote
			      (concat key "%3B")))))
	       (insert (url-encode-url
			(concat command "\n\n\n\n\n"))))))
	 clave-keys))
      (buffer-string))))

(clave-kle-make-url)

(defun clave-kle-show (&optional clave-map-filter active-map-filter)
  (interactive)
  (browse-url (clave-kle-make-url clave-map-filter active-map-filter)))
