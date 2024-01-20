;;; calyx-mode.el --- Major mode for editing Calyx files. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Samuel Thomas

;; Author: Samuel Thomas <sgt@cs.utexas.edu>
;; Package-Requires: (treesit dash s f xref)

;;; Code:
(require 'treesit)
(require 'dash)
(require 's)
(require 'f)
(require 'xref)

(defgroup calyx-mode-faces nil
  "Faces for highlighting Calyx code."
  :group 'calyx-mode)

(defface calyx-mode-face-comment
  '((default :inherit font-lock-comment-face))
  "Doc"
  :group 'calyx-mode)

(defface calyx-mode-face-keyword
  '((default :inherit font-lock-keyword-face))
  "Doc"
  :group 'calyx-mode)

(defface calyx-mode-face-literal
  '((default :inherit font-lock-builtin-face))
  "Doc"
  :group 'calyx-mode)

(defface calyx-mode-face-annotation
  '((default :inherit font-lock-preprocessor-face
             :slant italic))
  "Todo"
  :group 'calyx-mode)

(defface calyx-mode-face-attribute
  '((default :inherit (font-lock-string-face)
             :slant italic))
  "Todo"
  :group 'calyx-mode)

(defface calyx-mode-face-param
  '((default :inherit font-lock-variable-name-face))
  "Todo"
  :group 'calyx-mode)

(defface calyx-mode-face-string
  '((default :inherit font-lock-string-face))
  "Todo"
  :group 'calyx-mode)

(defface calyx-mode-face-section
  '((default :inherit font-lock-function-name-face))
  "Todo"
  :group 'calyx-mode)

(defface calyx-mode-face-component-name
  '((default :inherit font-lock-type-face))
  "Todo"
  :group 'calyx-mode)

(defface calyx-mode-face-group-name
  '((default :inherit (font-lock-type-face)))
  "Todo"
  :group 'calyx-mode)

(defface calyx-mode-face-group-hole
  '((default :inherit (font-lock-variable-name-face)
             :bold t))
  "Todo"
  :group 'calyx-mode)

(defface calyx-mode-face-cell-instantiation
  '((default :inherit (link font-lock-function-name-face)
             :underline nil))
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

(defface calyx-mode-face-parse-error
  '((default :inherit flyspell-incorrect))
  "Todo"
  :group 'calyx-mode)

(defvar calyx-mode-face-abbrevs
  '((comment . @calyx-mode-face-comment)
    (keyword . @calyx-mode-face-keyword)
    (literal . @calyx-mode-face-literal)
    (annotation . @calyx-mode-face-annotation)
    (attribute . @calyx-mode-face-attribute)
    (param . @calyx-mode-face-param)
    (string . @calyx-mode-face-string)
    (section . @calyx-mode-face-section)
    (component . ((name . @calyx-mode-face-component-name)))
    (group . ((name . @calyx-mode-face-group-name)
              (hole . @calyx-mode-face-group-hole)))
    (cell . ((instantiation . @calyx-mode-face-cell-instantiation)
             (name . @calyx-mode-face-cell-name)
             (use . @calyx-mode-face-cell-use)
             (access . @calyx-mode-face-cell-access)))
    (parse-error . @calyx-mode-face-parse-error)
    )
  )

(defvar calyx-font-lock-rules
  (let-alist calyx-mode-face-abbrevs
    `(:language calyx
                :override t
                :feature comment
                ((comment) ,.comment)

                :language calyx
                :override t
                :feature keyword
                (["import" "component" "ref" "comb" "group" "static" "primitive" "extern"] ,.keyword)

                :language calyx
                :override t
                :feature keyword
                (["invoke" "seq" "par" "if" "while" "repeat" "with"] ,.keyword)

                :language calyx
                :override t
                :feature keyword
                (["cells" "wires" "control"] ,.section)

                :language calyx
                :override t
                :feature toplevel
                ((import (string) ,.string))

                :language calyx
                :override t
                :feature toplevel
                ((extern (string) ,.string))

                :language calyx
                :override t
                :feature toplevel
                ((component (ident) ,.component.name))

                :language calyx
                :override t
                :feature toplevel
                ((primitive (ident) ,.component.name
                            (params (ident) ,.param)
                            (signature (io_port_list (io_port (ident) (ident) ,.param)))))

                :language calyx
                :override t
                :feature toplevel
                ((at_attribute "@" ,.annotation
                               (ident) ,.annotation))

                :language calyx
                :override t
                :feature toplevel
                ((cell_assignment (ident) ,.cell.name
                                  (instantiation (ident) ,.cell.instantiation)))

                :language calyx
                :override t
                :feature toplevel
                ((attributes) ,.string)

                :language calyx
                :override t
                :feature toplevel
                ((attribute (string)) ,.attribute)

                :language calyx
                :override t
                :feature toplevel
                ((group (ident) ,.group.name))

                :language calyx
                :override t
                :feature toplevel
                ((port (ident) ,.cell.use
                       (ident) ,.cell.access))

                :language calyx
                :override t
                :feature toplevel
                ((hole (ident) ,.group.name
                       (ident) ,.group.hole))
                
                ;; highest precedence
                :language calyx
                :override t
                :feature toplevel
                ((number) ,.literal)

                :language calyx
                :override t
                :feature toplevel
                ((literal) ,.literal)

                :language calyx
                :override t
                :feature toplevel
                ((any_line) ,.string)

                :language calyx
                :override t
                :feature toplevel
                ((ERROR) ,.parse-error))))

(defvar calyx-indent-level 2)

(defvar calyx-indent-rules
  `((calyx
     ((node-is "comment") no-indent 0)

     ((parent-is "source_file") column-0 0)

     ((query ((component "}" @q))) parent 0)
     ((node-is "component") parent 0)
     ((parent-is "component") parent ,calyx-indent-level)

     ((query ((extern "}" @q))) parent-bol 0)
     ((node-is "extern") parent 0)
     ((parent-is "extern") parent-bol ,calyx-indent-level)

     ;; primitive
     ((query ((primitive_blob "}" @q))) parent-bol 0)
     ((node-is "primitive") parent 0)
     ((parent-is "primitive_blob") parent-bol ,calyx-indent-level)

     ((node-is "io_port_list") prev-sibling 0)
     ((node-is "io_port") prev-sibling 0)
     ((query ((io_port_list ")" @q))) parent-bol 0)
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

;; analysis functions
(defun calyx-mode-ts-node-at-point ()
  (interactive)
  (-if-let* (((beg . end) (bounds-of-thing-at-point 'symbol))
             (node (treesit-node-at beg)))
    node))

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

(defun calyx-mode-make-location-list (lst)
  (--map (xref-make "" (calyx-xref-make-location (car it) "" (cdr it))) lst))

(defun calyx-mode-ident-type (node)
  (cond
   ((calyx-mode-has-parent-p node "invoke") 'cell)
   ;; we only want the left hand side ident of a port
   ;; making sure that we have a next sibling ensures this
   ((and (calyx-mode-has-parent-p node "port")
         (treesit-node-next-sibling node))
    'cell)
   ((calyx-mode-has-parent-p node "hole") 'group)
   ((calyx-mode-has-parent-p node "enable") 'group)
   ((calyx-mode-has-parent-p node "port_with") 'group)
   ((calyx-mode-has-parent-p node "instantiation") 'component)
   (t nil)))

;; eldoc integration
(defun calyx-mode-component-signature (comp-node)
  (let-alist (treesit-query-capture comp-node
                                    '((component (ident) @name (signature) @sig)
                                      (primitive (ident) @name (signature) @sig)))
    (cons (treesit-node-text .name) .sig)))

(defun calyx-mode-gather-signatures ()
  (--map (calyx-mode-component-signature (cdr it))
         (treesit-query-capture (treesit-buffer-root-node)
                                '((component) @comp
                                  (primitive) @prim))))

(defun calyx-mode-resolve-import-path (path)
  (cond
   ((file-exists-p path) path)
   ((file-exists-p (f-join "~" ".calyx" path)) (f-join "~" ".calyx" path))
   (t nil)))

(defun calyx-mode-search-for-signature (imports cell-name)
  (-when-let (path (car imports))
    (-when-let (path (calyx-mode-resolve-import-path path))
      (with-current-buffer (find-file-noselect path)
        (-if-let* ((sig (calyx-mode-gather-signatures))
                   (match-sig (assoc cell-name sig)))
            (cdr match-sig)
          (calyx-mode-search-for-signature (append (calyx-mode-file-import-strings) (cdr imports))
                                           cell-name))))))

(defun calyx-mode-render-signature (name sig)
  (when sig
    (concat name (s-join "" (-map #'s-trim (s-lines (treesit-node-text sig)))))))

(defun calyx-mode-file-import-strings ()
  (let* ((raw-imports (treesit-query-capture (treesit-buffer-root-node)
                                             '((import (string) @q))))
         (file-imports (--map (substring (treesit-node-text (cdr it)) 1 -1)
                              raw-imports)))
    file-imports))

(defun calyx-mode-cell-sig-at-point ()
  (when-let (node (calyx-mode-ts-node-at-point))
    (pcase (calyx-mode-ident-type node)
      ('cell (-let* ((name (treesit-node-text node))
                     (cells (calyx-mode-find-objs node 'cell))
                     ((_ . instance-node) (--find (equal (car it) name) cells))
                     (cell-name (treesit-node-text
                                 (cdar (treesit-query-capture (treesit-node-parent instance-node)
                                                              '((instantiation (ident) @q))))))
                     (local-sig (calyx-mode-gather-signatures)))
               
               (-if-let (match-sig (assoc cell-name local-sig))
                   (calyx-mode-render-signature cell-name (cdr match-sig))
                 (calyx-mode-render-signature cell-name
                                              (calyx-mode-search-for-signature (calyx-mode-file-import-strings) cell-name)))))
      ('component (-if-let* ((name (treesit-node-text node))
                             (local-sig (calyx-mode-gather-signatures))
                             (match-sig (assoc name local-sig)))
                      (calyx-mode-render-signature name (cdr match-sig))
                    (calyx-mode-render-signature name
                                                 (calyx-mode-search-for-signature (calyx-mode-file-import-strings) name)))))))

(defun calyx-mode-eldoc (callback &rest _args)
  (funcall callback (calyx-mode-cell-sig-at-point)))

;; Xref
(defun calyx-mode-xref-backend () 'calyx)

(cl-defstruct (calyx-xref-location
               (:constructor calyx-xref-make-location (symbol file node)))
  "doc"
  symbol file node)

(cl-defmethod xref-location-marker ((l calyx-xref-location))
  (let ((node (calyx-xref-location-node l)))
    (with-current-buffer (treesit-node-buffer node)
      (save-excursion
        (goto-char (treesit-node-start node))
        (point-marker)))))

(cl-defmethod xref-location-group ((l calyx-xref-location))
  (calyx-xref-location-file l))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql 'calyx)))
  (let ((node (calyx-mode-ts-node-at-point)))
    (if (equal (treesit-node-type node) "ident")
        (propertize (treesit-node-text node)
                    'calyx-ts-node node)
      nil)))

(cl-defmethod xref-backend-definitions ((_backend (eql 'calyx)) ident)
  (let* ((node (get-text-property 0 'calyx-ts-node ident))
         (type (calyx-mode-ident-type node)))
    (if-let (objs (--filter (equal ident (car it)) (calyx-mode-find-objs node type)))
        (calyx-mode-make-location-list objs)
      (progn
        (when-let* ((sig (calyx-mode-search-for-signature (calyx-mode-file-import-strings) ident))
                    (node (cdar (treesit-query-capture (treesit-node-parent sig)
                                                       '((primitive (ident) @q))))))
          (calyx-mode-make-location-list `((,ident . ,node))))))))

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
                (annotations)))

  ;; setup indentation
  ;; (setq-local treesit--indent-verbose t)
  (setq-local treesit-simple-indent-rules calyx-indent-rules)

  ;; so that we can use the combobulate query builder in calyx modes
  (setq-local combobulate-rules-calyx '())
  (setq-local combobulate-rules-calyx-inverted '())

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

  ;; eldoc
  (add-hook 'eldoc-documentation-functions #'calyx-mode-eldoc nil t)

  (treesit-major-mode-setup))

(defun calyx-mode-reload ()
  (interactive)
  (fundamental-mode)
  (calyx-mode))

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
