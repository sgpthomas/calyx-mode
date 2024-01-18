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

(defvar calyx-indent-level 2)

(defvar calyx-indent-rules
  `((calyx
     ((node-is "comment") no-indent 0)

     ((parent-is "source_file") column-0 0)
     ((query ((component "}" @query))) parent-bol 0)
     ((node-is "component") parent 0)
     ((parent-is "component") parent ,calyx-indent-level)

     ((node-is "io_port_list") prev-sibling 0)
     ((node-is "io_port") prev-sibling 0)
     ((query ((io_port_list ")" @q))) column-0 0)
     ((parent-is "io_port") parent ,calyx-indent-level)
     ((parent-is "signature") parent 0)

     ;; cells
     ((parent-is "cell_assignment") parent-bol ,calyx-indent-level)
     ((node-is "cells_inner") parent ,calyx-indent-level)
     ((parent-is "cells_inner") parent 0)
     ((parent-is "cells") parent 0)
     ((query ((arg_list ")" @query))) parent-bol 0)
     ((parent-is "arg_list") parent-bol ,calyx-indent-level)

     ;; wires block
     ((node-is "wires_inner") parent ,calyx-indent-level)
     ((parent-is "wires_inner") parent 0)
     ((parent-is "wires") parent 0)

     ;; groups
     ((query ((group "}" @query))) parent-bol 0)
     ((parent-is "group") parent ,calyx-indent-level)
     ((parent-is "wire_assignment") parent-bol ,calyx-indent-level)
     ((parent-is "switch") parent-bol ,calyx-indent-level)

     ;; control block
     ((node-is "control_inner") parent ,calyx-indent-level)
     ((node-is "stmt") parent-bol ,calyx-indent-level)
     ((parent-is "control") parent 0)

     ((query ((seq "}" @q))) parent 0)
     ((parent-is "seq") parent-bol ,calyx-indent-level)

     ((query ((par "}" @q))) parent 0)
     ((parent-is "par") parent-bol ,calyx-indent-level)

     ((query ((block "}" @q))) parent-bol 0)
     ((parent-is "block") parent-bol ,calyx-indent-level)

     ((node-is "invoke_args") prev-sibling 0)
     ((node-is "invoke_arg") prev-sibling 0)
     ((parent-is "invoke_arg") parent ,calyx-indent-level)

     (catch-all parent-bol ,calyx-indent-level))))

(with-current-buffer (get-buffer "pass-in-register.futil")
  (calyx-mode-setup)
  (indent-region (point-min) (point-max))
  )

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
  (setq-local treesit--indent-verbose t)
  (setq-local treesit-simple-indent-rules calyx-indent-rules)

  (setq combobulate-rules-calyx '())
  (setq combobulate-rules-calyx-inverted '())

  (treesit-major-mode-setup))

(defun calyx-mode-update-tree-sitter ()
  "Update the tree-sitter parser for Calyx."
  (interactive)

  (let ((treesit-language-source-alist '((calyx "https://github.com/sgpthomas/tree-sitter-calyx.git"))))
    (treesit-install-language-grammar 'calyx)
    (calyx-mode-setup))

  (when (eq major-mode 'calyx-mode)
    (treesit-parser-create 'calyx)
    (calyx-mode-setup)
    (font-lock-update)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.futil\\'" . calyx-mode))

;;;###autoload
(define-derived-mode calyx-mode prog-mode "Calyx"
  "Major mode for editing Calyx with tree-sitter."

  (when (treesit-available-p)
    (if (treesit-ready-p 'calyx)
        (progn (treesit-parser-create 'calyx)
               (calyx-mode-setup))
      (when (y-or-n-p "Install language grammar?")
        (calyx-mode-update-tree-sitter)))))

(provide 'calyx-mode)
