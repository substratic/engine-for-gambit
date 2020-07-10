;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine components component)
  (import (gambit)
          (node)
          (alist)
          (state)
          (macros))
  (export make-component
          make-component-new
          make-component-lambda
          add-component-methods
          define-component
          remove-component
          merge-alists) ;; This won't be needed anymore

  (begin

    (define-macro (make-component state-key . initial-fields)
      ;; Takes the list of update-pairs and filters them so
      ;; that all component state fields are separated from
      ;; "method" fields that are applied to the top-level
      ;; game object state (handlers, updaters, renderers)
      (define (filter-component-fields update-pairs)
        (fold (lambda (field-pair field-lists)
                (let ((state-fields (car field-lists))
                      (method-fields (cadr field-lists)))
                  (case (car field-pair)
                    ((handlers updaters listeners renderers)
                     (set! method-fields (append method-fields (list (cons (car field-pair)
                                                                           (cadr field-pair))))))
                    (else (set! state-fields (append state-fields (list (cons (car field-pair)
                                                                              (cadr field-pair)))))))
                  (list state-fields method-fields)))
              '(() ())
              update-pairs))

      (let* ((filtered-fields  (filter-component-fields initial-fields))
             ;; TODO: There must be a way to pre-create these in filter-component-fields
             (component-fields (cons `(cons '__converted #t)
                                     (map (lambda (f) `(cons ',(car f) ,(cdr f)))
                                          (car filtered-fields))))
             (object-fields    (map (lambda (f) `(cons ',(car f) ,(cdr f)))
                                    (cadr filtered-fields))))
        `(lambda (node #!optional (state-overlay '()))
           (let* ((component-overlay (assoc ',state-key state-overlay))
                  (default-values    (list ,@component-fields))
                  (field-values      (merge-alists default-values (if component-overlay
                                                                      (cdr component-overlay)
                                                                      '())))
                  (method-fields     (list ,@object-fields)))
             (update-state-with-alist node (append method-fields
                                                   (list (cons ',state-key field-values))))))))

    (define (remove-component state component-name)
      ;; TODO: How do we remove *all* methods that a
      ;; component added?  How can we know which ones?
      ;; Store a 'components' list with an index?

      (update-state state
                    (updaters  (remove-method component-name))
                    (handlers  (remove-method component-name))
                    (renderers (remove-method component-name))))

      ;; TODO: Implement 'define-component': it will define a component with
      ;; a function that wraps the output of 'make-component' so that it
      ;; can be invoked with the same type of parameter list.  The function
      ;; that is generated will have keyword parameters that correspond
      ;; with the fields in the definition.

    (define (make-component-lambda component-symbol field-alist)
      `(lambda (node #!optional (state-overlay '()))
         (let* ((component-overlay (assoc ',component-symbol state-overlay)))
           (update-state-with-alist node (append (list (cons ',component-symbol ,field-alist))
                                                 (list (cons ',component-symbol component-overlay)))))))


    (define-macro (make-component-new component-symbol . define-forms)

      (define (make-component-lambda component-symbol field-alist)
        `(lambda (node #!optional (state-overlay '()))
           (let* ((component-overlay (assoc ',component-symbol state-overlay)))
             (update-state-with-alist node (append (list (cons ',component-symbol ,field-alist))
                                                   (list (cons ',component-symbol component-overlay)))))))

      (define (field-alist field-forms)
        ;; For now it's (field-name: default-value)
        (map (lambda (field-form)
               (let ((sym (string->symbol (keyword->string (car field-form)))))
                 `(cons ',sym ,(cdr field-form))))
            field-forms))

      ;; TODO: Ensure that the symbol is correct before using list
      (let ((fields  (cdar define-forms))
            (methods (cdadr define-forms)))
        (make-component-lambda component-symbol (field-alist fields))))

        ;; Use fields to generate #!key and initial state alist to be merged with state-overlay
        ;; Use methods to generate
        ;; Also create state update call for 'components' to register (or update) this component

        ;; `(list ,@(map (lambda (m) (##procedure-name fps-renderer)) (cdar methods)))))))
        ;; `(list (##procedure-name fps-renderer) ',fields  (map (lambda (m) (##procedure-name m)) (list ,@(cdar methods))))
        ;; `(lambda ,(append '(node #!optional (state-overlay '())) '())
        ;;    (lambda ,(append '(node #!optional (state-overlay '())) '()
        ;;                      (if (pair? fields) (cons '#!key (keyword-args fields)) '()))
        ;;      (let* ((component-overlay (assoc ',component-name state-overlay)))
        ;;        (update-state-with-alist node (append (list (cons ',component-name ,(default-alist fields)))
        ;;                                              (list (cons ',component-name component-overlay)))))))))

    ;; Reusing the inner macro:
    ;; - The main difference is in the beginning of the declaration, whether it's a lambda
    ;;   or not.  make-component needs '(lambda (' where define-component needs (define ())
    ;; - Actually maybe they both produce a lambda, the define just sets up the define?  It's
    ;;   more a matter of passing in the arguments correctly


    ;; HERE'S THE DIFFERENCE:
    ;; - define-component creates a lambda with keyword parameters to set default values.  This
    ;;   lambda wraps the real component lambda which takes a node as parameter
    ;; - make-component creates a raw component lambda which takes
    ;;
    ;; The main difference is in how the default values are baked into the component lambda.
    ;; make-component bakes the default values in directly where define-component bakes in the
    ;; keyword argument values.
    ;;
    ;; The helper function that generates the raw lambda needs to take the list of defaults
    ;; so that they can be included directly.

    (define-macro (eval-in-macro-environment . exprs)
      (eval (if (null? (cdr exprs)) (car exprs) (cons 'begin exprs))
            (interaction-environment)))

    (define-macro (define^ . args)
      `(eval-in-macro-environment
        (define ,(car args) ,@(cdr args))))

    (define^ (eval^ . exprs)
      (eval (if (null? (cdr exprs)) (car exprs) (cons 'begin exprs))
            (interaction-environment)))

    (define-macro (test-me method sym)
      (import (github.com/substratic engine methods))
      (let ((method-params (method-table-ref method)))
        (pp method-params)
        `(lambda ,method-params
          (,sym ,@method-params))))

    ;; (define-method editor-renderer engine/renderer (node context renderer)
    ;;   #t)

    (define-macro (define-component component-symbol . define-forms)
      ;; Import methods module for method table access
      (import (github.com/substratic engine methods))

      (define (make-component-lambda component-symbol field-alist method-sets)
        `(lambda (node #!optional (state-overlay '()))
           (let* ((component-overlay (assoc ',component-symbol state-overlay)))
             (update-state-with-alist
              node
              (append (list (cons ',component-symbol
                                   (cons (cons '__converted #t) (list ,@field-alist))))
                      (if (list? component-overlay)
                          (list (cons ',component-symbol component-overlay)))
                      (list (cons 'methods (add-component-methods ,component-symbol ,@method-sets))))))))

      (define (field-alist field-forms)
        ;; For now it's (field-name: default-value)
        (map (lambda (field-form)
               (let ((sym (string->symbol (keyword->string (car field-form)))))
                 `(cons ',sym ,sym)))
            field-forms))

      (define (keyword-args field-forms)
        ;; For now it's (field-name: default-value)
        (map (lambda (field-form)
               (let ((sym (string->symbol (keyword->string (car field-form)))))
                 `(,sym ,(cadr field-form))))
            field-forms))

      (define (method-sets method-forms)
        (map (lambda (method-form)
               (let* ((method-type (string->symbol (keyword->string (car method-form))))
                      (method-params (method-table-ref method-type)))

                 ;; For each method, verify the method argument list and generate wrapper
                 `(list
                   ',method-type
                   (list ,@(map (lambda (method-symbol)
                                  `(cons
                                    (##procedure-name method-symbol)
                                    (lambda ,method-params
                                      (,method-symbol ,@method-params))))
                                (cdr method-form))))))
             method-forms))

      ;; To get the argument list of a procedure:
      ;; (cadr (##decompile (eval 'proc-name)))

      ;; TODO: Ensure that the symbol is correct before using list
      (let ((fields  (cdar define-forms))
            (methods (method-sets (cdadr define-forms)))
            (component-name (string->symbol (string-append (symbol->string component-symbol)
                                                           "-component"))))
        `(define (,component-name ,@(if (pair? fields) (cons '#!key (keyword-args fields)) '()))
           ;; (list ,@methods)
           ,(make-component-lambda component-symbol
                                   (field-alist fields)
                                   methods))))

    ;; (define-component editor
    ;;   (fields  (visible:       #t)
    ;;            (mode:          'editor) ;; Or 'playback
    ;;            (cursor-pos:    '(0 0. 0.))
    ;;            (can-place?:    #t)
    ;;            (stack-file:    stack-file)
    ;;            (layer-opacity: #f))

    ;;   (methods (engine/updater:  editor-updater)
    ;;            (engine/handler:  editor-handler)
    ;;            (engine/renderer: editor-renderer)))

    ;; node:
    ;; - components: '((#t editor) (#f debug))
    ;; - methods:
    ;;   - engine/renderer: '((#t crash/components/editor#editor-renderer editor) (#f substratic/engine/debug#debug-renderer debug))
    ;;   - engine/updater: '((#t crash/components/editor#editor-updater editor)
    ;; - editor:
    ;;   ... state ...

    (define (add-component-method component methods method-info)
      (let ((method (list #t (car method-info) (cdr method-info) component)))
        (let next-method ((methods methods))
          (if (null? methods)
              (list method)
              (if (equal? (car methods))
                  (cons method (cdr methods))
                  (cons (car methods) (next-method (cdr methods))))))))

    (define (add-component-methods component . method-sets)
      (lambda (methods)
        (fold (lambda (method-set methods)
                (update-in methods (car method-set) (add-component-method component (cdr))))
              methods
              method-sets)))

    (define editor-component
      (lambda (#!optional (state-overlay '())
               #!key (visible #t) (mode 'editor)) ;etc
        (let* ((defaults (list (cons 'visible visible)
                               (cons 'editor editor)))
               (methods  (add-component-methods 'editor
                                                (list 'engine/renderer (lambda (node context renderer)
                                                                         (editor-renderer node context renderer)))))
               (add-component (lambda (components)
                                (append components (list (list 'editor #t))))))
          (update-state-with-alist node (append (list (cons 'editor defaults))
                                                (list (cons 'editor state-overlay))
                                                (list (cons 'methods methods))
                                                (list (cons 'components add-component)))))))))
