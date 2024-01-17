;;; Code:
(require 'treesit)

(setq combobulate-rules-calyx-inverted '())

(defgroup calyx-mode-faces nil
  "Faces for highlighting Calyx code."
  :group 'calyx-mode)

(defface calyx-mode-face-annotation
  '((default :inherit font-lock-preprocessor-face
             :slant italic
             ))
  "Todo"
  :group 'calyx-mode)

(defface calyx-mode-face-cell-instantiation
  '((default :inherit (link font-lock-function-name-face)
             :underline nil))
  "Todo"
  :group 'calyx-mode)

(defface calyx-mode-face-attribute
  '((default :inherit (font-lock-string-face)
             :slant italic))
  "Todo"
  :group 'calyx-mode)

(defface calyx-mode-face-group-name
  '((default :inherit (font-lock-type-face)))
  "Todo"
  :group 'calyx-mode)

(defface calyx-mode-face-cell-name
  '((default))
  "Todo"
  :group 'calyx-mode)

(defface calyx-mode-face-cell-use
  '((default))
  "Todo"
  :group 'calyx-mode)

(defface calyx-mode-face-cell-access
  '((default :inherit (font-lock-variable-name-face)
             :slant italic))
  "Todo"
  :group 'calyx-mode)

(defface calyx-mode-face-hole
  '((default :inherit (font-lock-variable-name-face)
             :bold t))
  "Todo"
  :group 'calyx-mode)

(defvar calyx-font-lock-rules
  '(:language calyx
              :override t
              :feature comment
              ((comment) @font-lock-comment-face)

              :language calyx
              :override t
              :feature keyword
              (["import" "component" "ref" "comb" "group" "static"] @font-lock-keyword-face)

              :language calyx
              :override t
              :feature keyword
              (["invoke" "seq" "par" "if" "while" "repeat" "with"] @font-lock-keyword-face)

              :language calyx
              :override t
              :feature keyword
              (["cells" "wires" "control"] @font-lock-function-name-face)

              :language calyx
              :override t
              :feature toplevel
              ((import (string) @font-lock-string-face))

              :language calyx
              :override t
              :feature toplevel
              ((component (ident) @font-lock-type-face))

              :language calyx
              :override t
              :feature toplevel
              ((at_attribute "@" @calyx-mode-face-annotation
                             (ident) @calyx-mode-face-annotation))

              :language calyx
              :override t
              :feature toplevel
              ((cell_assignment (ident) @calyx-mode-face-cell-name
                                (instantiation (ident) @calyx-mode-face-cell-instantiation)))

              :language calyx
              :override t
              :feature toplevel
              ((attributes) @font-lock-string-face)

              :language calyx
              :override t
              :feature toplevel
              ((attribute (string)) @calyx-mode-face-attribute)

              :language calyx
              :override t
              :feature toplevel
              ((group (ident) @calyx-mode-face-group-name))

              :language calyx
              :override t
              :feature toplevel
              ((port (ident) @calyx-mode-face-cell-use
                     (ident) @calyx-mode-face-cell-access))

              :language calyx
              :override t
              :feature toplevel
              ((hole (ident) @calyx-mode-face-group-name
                     (ident) @calyx-mode-face-hole))
              
              ;; highest precedence
              :language calyx
              :override t
              :feature toplevel
              ((number) @font-lock-builtin-face)

              :language calyx
              :override t
              :feature toplevel
              ((literal) @font-lock-builtin-face)))

(defun calyx-mode-setup ()
  "Setup treesit for calyx-mode"

  ;; setup font-locking
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules calyx-font-lock-rules))
  (setq-local font-lock-defaults nil)
  (setq-local treesit-font-lock-feature-list
              '((comment)
                (keyword)
                (toplevel)
                (annotations)
                ))

  ;; setup indentation
  ;; (setq-local treesit-simple-indent-rules calyx-indent-rules)

  (treesit-major-mode-setup)
  )

(defun calyx-mode-update-tree-sitter ()
  "Update the tree-sitter parser for Calyx."
  (interactive)

  (let ((treesit-language-source-alist '((calyx "https://github.com/sgpthomas/tree-sitter-calyx.git"))))
    (treesit-install-language-grammar 'calyx)
    (calyx-mode-setup))

  (when (eq major-mode 'calyx-mode)
    (treesit-parser-delete (car (treesit-parser-list)))
    (treesit-parser-create 'calyx)
    (calyx-mode-setup)
    (font-lock-update)))

;;;###autoload
(define-derived-mode calyx-mode prog-mode "Calyx"
  "Major mode for editing Calyx with tree-sitter."

  (when (treesit-available-p)
    (if (treesit-ready-p 'calyx)
        (progn (treesit-parser-create 'calyx)
               (calyx-mode-setup))
      (when (y-or-n-p "Install language grammar?")
        (calyx-mode-update-tree-sitter)))))

