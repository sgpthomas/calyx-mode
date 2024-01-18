;;; Code:
(require 'treesit)
(require 'dash)

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

(defface calyx-mode-face-parse-error
  '((default :inherit flyspell-incorrect))
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
              (["import" "component" "ref" "comb" "group" "static" "primitive"] @font-lock-keyword-face)

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
              ((primitive (ident) @font-lock-type-face))

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
              ((literal) @font-lock-builtin-face)

              :language calyx
              :override t
              :feature toplevel
              ((any_line) @font-lock-string-face)

              :language calyx
              :override t
              :feature toplevel
              ((ERROR) @calyx-mode-face-parse-error)
              ))

(defvar calyx-indent-level 2)

(defvar calyx-indent-rules
  `((calyx
     ((node-is "comment") no-indent 0)

     ((parent-is "source_file") column-0 0)

     ((query ((component "}" @q))) parent 0)
     ((node-is "component") parent 0)
     ((parent-is "component") parent ,calyx-indent-level)

     ;; primitive
     ((query ((primitive_blob "}" @q))) parent-bol 0)
     ((node-is "primitive") parent 0)
     ((parent-is "primitive_blob") parent-bol ,calyx-indent-level)

     ((node-is "io_port_list") prev-sibling 0)
     ((node-is "io_port") prev-sibling 0)
     ((query ((io_port_list ")" @q))) column-0 0)
     ((parent-is "io_port") parent ,calyx-indent-level)
     ((parent-is "signature") parent 0)

     ;; cells
     ((parent-is "cell_assignment") parent-bol ,calyx-indent-level)
     ((node-is "cells_inner") parent ,calyx-indent-level)
     ((parent-is "cells_inner") parent 0)
     ((query ((cells "}" @q))) parent 0)
     ((parent-is "cells") parent ,calyx-indent-level)
     ((query ((arg_list ")" @query))) parent-bol 0)
     ((parent-is "arg_list") parent-bol ,calyx-indent-level)

     ;; wires block
     ((node-is "wires_inner") parent ,calyx-indent-level)
     ((parent-is "wires_inner") parent 0)
     ((query ((wires "}" @q))) parent 0)
     ((parent-is "wires") parent ,calyx-indent-level)

     ;; groups
     ((query ((group "}" @query))) parent-bol 0)
     ((parent-is "group") parent ,calyx-indent-level)
     ((parent-is "wire_assignment") parent-bol ,calyx-indent-level)
     ((parent-is "switch") parent-bol ,calyx-indent-level)

     ;; control block
     ((node-is "control_inner") parent ,calyx-indent-level)
     ((node-is "stmt") parent-bol ,calyx-indent-level)
     ((query ((control "}" @q))) parent 0)
     ((parent-is "control") parent ,calyx-indent-level)

     ((query ((seq "}" @q))) parent 0)
     ((parent-is "seq") parent-bol ,calyx-indent-level)

     ((query ((par "}" @q))) parent 0)
     ((parent-is "par") parent-bol ,calyx-indent-level)

     ((query ((block "}" @q))) parent-bol 0)
     ((parent-is "block") parent-bol ,calyx-indent-level)

     ((node-is "invoke_args") prev-sibling 0)
     ((node-is "invoke_arg") prev-sibling 0)
     ((parent-is "invoke_arg") parent ,calyx-indent-level)

     (catch-all parent-bol 0))))

(defvar calyx-mode-imenu
  `(("Component"
     ,(lambda (node)
        (or (equal (treesit-node-text node) "component")
            (equal (treesit-node-text node) "group")))
     ,(lambda (node)
        (pcase (treesit-node-text node)
          ("component" (treesit-node-text (treesit-node-next-sibling node)))
          ("group" (concat "group " (treesit-node-text (treesit-node-next-sibling node))))
          (_ (treesit-node-text node)))))))

;; Xref
(defun calyx-mode-xref-backend () 'calyx)

(cl-defstruct (calyx-xref-location
               (:constructor calyx-xref-make-location (symbol file node)))
  "doc"
  symbol file node)

(cl-defmethod xref-location-marker ((l calyx-xref-location))
  (save-excursion
    (goto-char (treesit-node-start (calyx-xref-location-node l)))
    (point-marker)))

(cl-defmethod xref-location-group ((l calyx-xref-location))
  (calyx-xref-location-file l))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql 'calyx)))
  (-let* (((beg . end) (bounds-of-thing-at-point 'symbol))
          (node (treesit-node-at beg)))
    (if (equal (treesit-node-type node) "ident")
        (propertize (treesit-node-text node)
                    'calyx-ts-node node)
      nil)))

(defun calyx-mode-find-parent (node parent)
  (if (equal (treesit-node-type node) parent)
      node
    (calyx-mode-find-parent (treesit-node-parent node) parent)))

(defun calyx-mode-has-parent-p (node parent-name)
  (cond
   ((equal (treesit-node-type node) "source_file") nil)
   ((equal (treesit-node-type node) parent-name) t)
   (t (calyx-mode-has-parent-p (treesit-node-parent node) parent-name))))

(defvar calyx-mode-obj-queries
  '((cell "component" ((cells (cells_inner (cell_assignment (ident) @cell)))))
    (group "component" ((group (ident) @group)))
    (component "source_file" ((component (ident) @group)))))

(defun calyx-mode-find-objs (node type)
  (-if-let* (((_ parent query) (assoc type calyx-mode-obj-queries))
             (enclosing (calyx-mode-find-parent node parent))
             (nodes (treesit-query-capture enclosing query)))
      (--map (cons (treesit-node-text (cdr it)) (cdr it)) nodes)))

(defun calyx-mode-make-location-list (lst ident)
  (--map (xref-make "" (calyx-xref-make-location (car it) "" (cdr it)))
         (--filter (equal ident (car it)) lst)))

(defun calyx-mode-ident-type (node)
  (cond
   ((calyx-mode-has-parent-p node "invoke") 'cell)
   ((calyx-mode-has-parent-p node "port") 'cell)
   ((calyx-mode-has-parent-p node "hole") 'group)
   ((calyx-mode-has-parent-p node "enable") 'group)
   ((calyx-mode-has-parent-p node "port_with") 'group)
   ((calyx-mode-has-parent-p node "instantiation") 'component)
   (t nil)))

(cl-defmethod xref-backend-definitions ((_backend (eql 'calyx)) identifier)
  (let* ((node (get-text-property 0 'calyx-ts-node identifier))
         (type (calyx-mode-ident-type node)))
    (calyx-mode-make-location-list
     (calyx-mode-find-objs node type)
     identifier)))

(defun calyx-mode-setup ()
  "Setup treesit for calyx-mode"
  (interactive)

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

  ;; so that we can use the combobulate query builder in calyx modes
  (setq combobulate-rules-calyx '())
  (setq combobulate-rules-calyx-inverted '())

  ;; setup imenu
  (setq-local treesit-simple-imenu-settings
              (--map (-let (((name node-fn name-fn) it))
                       `(,name ,node-fn nil ,name-fn))
                     calyx-mode-imenu))

  ;; setup xref
  (add-hook 'xref-backend-functions #'calyx-mode-xref-backend nil t)
  ;; only use xref for goto definition
  (when (featurep 'evil)
    (setq-local evil-goto-definition-functions '(evil-goto-definition-xref)))

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
