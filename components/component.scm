;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine components component)
  (import (gambit)
          (substratic engine state)
          (substratic engine macros))
  (export make-component
          remove-component)

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

    (define (remove-component component-name state)
      ;; TODO: How do we remove *all* methods that a
      ;; component added?  How can we know which ones?
      ;; Store a 'components' list with an index?

      (update-state state (updaters  (remove-method component-name))
                    (handlers  (remove-method component-name))
                    (renderers (remove-method component-name))))))

      ;; TODO: Implement 'define-component': it will define a component with
      ;; a function that wraps the output of 'make-component' so that it
      ;; can be invoked with the same type of parameter list.  The function
      ;; that is generated will have keyword parameters that correspond
      ;; with the fields in the definition.

      ;; (define-macro (define-component component-name . initial-fields)
      ;;   (let* ((filtered-fields  (filter-component-fields initial-fields))
      ;;          ;; TODO: There must be a way to pre-create these in filter-component-fields
      ;;          (component-fields (map (lambda (f) `(cons ',(car f) ,(cdr f)))
      ;;                                 (car filtered-fields)))
      ;;          (object-fields    (map (lambda (f) `(cons ',(car f) ,(cdr f)))
      ;;                                 (cadr filtered-fields))))
      ;;     `(define (,(format-symbol-name component-name (lambda (name) (string-append name "-component")))
      ;;               state
      ;;               #!optional (overlay '()))
      ;;        (let* ((default-values (list ,@component-fields))
      ;;               (field-values   (merge-alists default-values overlay))
      ;;               (method-fields  (list ,@object-fields)))
      ;;          (update-state-with-alist state (append method-fields
      ;;                                                 (list (cons ',component-name field-values))))))))

      ;; (define-component first-test
      ;;   a: 5
      ;;   b: 'thing)
