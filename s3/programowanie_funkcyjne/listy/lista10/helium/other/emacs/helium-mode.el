(defvar helium-mode-hook nil)

;; Customisation

(defgroup helium ()
  "Editing helium code"
  :group 'languages)

(defcustom helium-indent-level 2
  "Basic indentation step for helium"
  :group 'helium :type 'integer)

(defcustom helium-path-to-bin nil
  "Path to the helium interpreter's directory"
  :group 'helium :type 'directory)

(defgroup helium-faces ()
  "Special faces for helium"
  :group 'helium)

(defface font-lock-effop-face
  '((t (:inherit (font-lock-variable-name-face) :slant italic)))
  "Font Lock face used for highlighting effect operations."
  :group 'helium-faces)
(defvar font-lock-effop-face 'font-lock-effop-face
  "Face name to use for effect operations")
(defface font-lock-constr-face
  '((t (:inherit (font-lock-variable-name-face) :slant italic)))
  "Font Lock face used for highlighting constructors."
  :group 'helium-faces)
(defvar font-lock-constr-face 'font-lock-constr-face
  "Face name to use for constructors")

;; Keymap for the major mode
(defvar helium-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    (define-key map "\C-c\C-c" 'helium-eval-buffer)
    map)
  "The keymap for the `helium-mode'")

(defvar helium-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\* ". 23"  st)
    (modify-syntax-entry ?\( "()1n"   st)
    (modify-syntax-entry ?\) ")(4n"   st)
;;    (modify-syntax-entry ?/  ". 12b" st) ; not sure this works properly, check!
;;    (modify-syntax-entry ?\n "> b"   st)
    (mapc (lambda (c) (modify-syntax-entry c "_")) "_'")
    (mapc (lambda (c) (modify-syntax-entry c ".")) ".,|+-*<>=:")
    st)
  "The syntax table for the `helium-mode'")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[s]?he\\'" . helium-mode))

;; Regexps
(defun syms-wrapper-re (syms)
  (concat "\\_<" (regexp-opt syms t) "\\_>"))

(defconst helium-keywords-regexp
  (syms-wrapper-re
   '("let" "type" "effect" "rec" "of" "and" "in"
     "fn" "if" "then" "elif" "else" "open" "module" "functor" "sig" "struct" "val"
     "match" "handle" "with" "end" "return" "resume")))

(eval-and-compile
  (defconst helium-id-re "\\_<[a-z_]\\(?:\\sw\\|\\s_\\)*\\_>"))

(eval-and-compile
  (defconst helium-type-re "\\_<[A-Z]\\(?:\\sw\\|\\s_\\)*\\_>"))

(defconst helium-synops-regexp
  (regexp-opt '("=" "=>" "|" ":" "[" "]" "->" "." "{" "}" ";") t))

;; Syntax highlighting
(defconst helium-font-lock-keywords
  `(;;("(\\*\\*\\(?:[^*]+\\(?:\\*[^\)]\\)?\\)*\\*)" 1 font-lock-doc-face)
    ;;("(\\*\\(?:[^*]+\\(?:\\*[^\)]\\)?\\)*\\*)" . font-lock-comment-face)
    (,(concat "\\_<\\(let\\(?:\\s-+rec\\)?\\|and\\)\\s-+\\(" helium-id-re "\\)\\s-+")
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    (,(concat "\\_<\\(\\(?:type\\|effect\\)\\(?:\\s-+rec\\)?\\|and\\)\\(\\(?:\\s-+"
              helium-type-re "\\)+\\)\\s-*")
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))
    (,(concat "\\(?:let\\||\\|=\\)\\s-+\\(" helium-type-re "\\)\\s-*")
     (1 font-lock-constr-face))
    (,(concat "\\_<\\(val\\)\\s-+\\(" helium-id-re "\\)\\s-+:")
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    (,(concat "\\_<\\(?:type\\|effect\\)\\s-+" helium-type-re "\\s-+:")
     (1 font-lock-type-face))

    (,helium-synops-regexp 1 font-lock-builtin-face keep)
    (,helium-keywords-regexp 0 font-lock-keyword-face)
    (,(concat "\\(?:\{\\|;\\|\|\\)\\s-*\\(" helium-id-re
              "\\)\\s-*\\(?:[:=]\\|[^=:]*=>\\)")
     (1 font-lock-effop-face))
    ))

(defun helium-font-lock-syntactic-face-function (state)
  (if (nth 3 state)
      font-lock-string-face
    (let ((start (nth 8 state)))
      (if (and (> (point-max) (+ start 2))
               (eq (char-after (+ start 2)) ?*)
               (not (eq (char-after (+ start 3)) ?\)))
               (not (eq (char-after (+ start 3)) ?*)))
          ;; This is a documentation comment
          font-lock-doc-face
        font-lock-comment-face))))

;; Evaluation

(defun helium-eval-buffer (&optional BUFFER)
  "Evaluate the current buffer's file"
  (interactive)
  (save-buffer)
  (let ((helium-command
         (if (or (null helium-path-to-bin)
                 (string= helium-path-to-bin ""))
             (locate-file "helium" exec-path)
           (locate-file "helium" (list helium-path-to-bin))))
        (file-name
         (buffer-file-name BUFFER)))
    (when (null helium-command)
      (error "Helium interpreter not found! Make sure it is in exec-path or that `helium-path-to-bin' is set appropriately."))
    ;;  (let ((helium-lib (substring (shell-command-to-string (concat helium-command " -where")) 0 -1)))) ; this doesn't work since -where gives lib dir locally, and even after expansion prelude is not found
    ;; XXX: the following is a nasty hack, since I can't get helium to run from anywhere else then its home dir
    (let ((olddir default-directory))
      (cd (substring helium-command 0 -10))
      (compile (concat helium-command " " file-name) t)
      (minibuffer-message olddir)
      (cd olddir))))


(defun helium-mode ()
  "Major mode for editing helium code"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table helium-syntax-table)
  (use-local-map helium-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(helium-font-lock-keywords
         nil nil nil nil
         (font-lock-syntactic-face-function
           . helium-font-lock-syntactic-face-function)))
  (setq major-mode 'helium-mode)
  (setq mode-name "helium")
  (run-hooks 'helium-mode-hook))

(provide 'helium-mode)
