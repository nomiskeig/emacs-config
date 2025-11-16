(defvar my/c-ts-rules
(treesit-font-lock-rules
 :language 'c
 :override t
 :feature 'my-preproc
 '(["#include" "#ifdef" "#endif" "#elif" "#define" "#ifndef" "#else" (preproc_directive)] @my-font-lock-preproc-face)
 :language 'c
 :override t
 :feature 'my-keyword
 '(["break" "case" "const"
 "continue"
 "default"
 "do"
 "else"
 "enum"
 "extern"
 "for"
 "goto"
 "if"
 "inline"
 "return"
 "sizeof"
 "static"
 "struct"
 "switch"
 "typedef"
 "union"
 "volatile"
 "while"
 "..."]  @my-font-lock-keyword-face)

 :language 'C
 :override t
 :feature 'my-string
 '([(string_literal) (system_lib_string)] @my-font-lock-string-face)

:language 'c
:override t
:feature 'my-number
'([(number_literal) (char_literal)] @my-font-lock-number-face)

:language 'c
:override t
:feature 'my-constant
'(((identifier) @my-font-lock-constant-face
 (:match "^[A-Z][A-Z_]*" @my-font-lock-constant-face )))
:language 'c
:override t
:feature 'my-function
'(((function_declarator
 declarator: [(identifier) @my-font-lock-function-face
              (parenthesized_declarator
               (pointer_declarator (field_identifier) @my-font-lock-fuction-face))])))

:language 'c
:override t
:feature 'my-function
'((preproc_function_def
 name: (identifier) @my-font-lock-function-face))


:language 'c
:override t
:feature 'my-function-call
'((call_expression
 function: [(identifier) @my-font-lock-function-call-face
            (field_expression field: (_) @my-font-lock-method-call-face)]))
))
(defun my/c-ts-mode-extra-highlights ()
  (setq-local treesit-font-lock-settings
              (append treesit-font-lock-settings my/c-ts-rules))
  (setq-local treesit-font-lock-feature-list
              (append treesit-font-lock-feature-list '((my-preproc my-string my-number my-constant my-keyword my-function my-function-call))))
  ;; Neu kompilieren/aktivieren
  (treesit-font-lock-recompute-features)
  (font-lock-flush))
  
(add-hook 'c-ts-mode-hook #'my/c-ts-mode-extra-highlights)
