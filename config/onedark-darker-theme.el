;;; package --- color-scheme
;;; Commentary:

; The structure for this is based on https://github.com/emacsmirror/timu-spacegrey-theme/blob/master/timu-spacegrey-theme.el
; The colors are strongly based on  https://github.com/navarasu/onedark.nvim
(defgroup my-code-faces-c nil
  "Faces for highlighting c code.")
(defface my-font-lock-preproc-face
  '((default :inherit default :slant oblique))
  "Face for creating new instances."
  :group 'my-code-faces-c)
(defface my-font-lock-string-face
  '((default :inherit default :slant oblique))
  "Face for strings"
  :group 'my-code-faces-c)
(defface my-font-lock-number-face
  '((default :inherit default :slant oblique))
  "Face for number and char literals"
  :group 'my-code-faces-c)

(defface my-font-lock-constant-face
  '((default :inherit default :slant oblique))
  "Face for number and char literals"
  :group 'my-code-faces-c)

(defface my-font-lock-keyword-face
  '((default :inherit default :slant oblique))
  "Face for number and char literals"
  :group 'my-code-faces-c)
(defface my-font-lock-function-face

  '((default :inherit default :slant oblique))
  "Face for number and char literals"
  :group 'my-code-faces-c)
(deftheme onedark-darker "A theme based on onedark-darker.")
(defface my-font-lock-method-call-face

  '((default :inherit default :slant oblique))
  "Face for number and char literals"
  :group 'my-code-faces-c)
(deftheme onedark-darker "A theme based on onedark-darker.")
(defface my-font-lock-function-call-face

  '((default :inherit default :slant oblique))
  "Face for number and char literals"
  :group 'my-code-faces-c)
(deftheme onedark-darker "A theme based on onedark-darker.")
;;; Code:
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
(let (
      (color-black "#0e1013")
      (fg "#a0a8b7")
      (bg "#1f2329")
      (purple "#bf68d9")
      (green "#8ebd6b")
      (orange "#cc9057")
      (zyan "#48b0bd")
      (blue "#4fa6ed")
      (light-blue "#75bef9")
      )
  (custom-theme-set-faces 'onedark-darker
			  `(default ((t (:foreground ,fg :background ,bg))))
			  `(my-font-lock-preproc-face ((t (:foreground, purple))))
			  `(my-font-lock-string-face ((t (:foreground, green))))
			  `(my-font-lock-number-face ((t (:foreground, orange))))
			  `(my-font-lock-constant-face ((t (:foreground, zyan))))
			  `(my-font-lock-keyword-face ((t (:foreground, purple))))
			  `(my-font-lock-function-face ((t (:foreground,blue))))
			  `(my-font-lock-function-call-face ((t (:foreground, light-blue))))

			  ;`(hl-line-face ((t (:background, zyan))))
 ))
(provide-theme 'onedark-darker)
