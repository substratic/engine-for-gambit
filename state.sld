;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine state)
  (import (gambit)
          (substratic engine alist))
  (export add-method
          remove-method
          state?
          state-ref
          state-set!
          state-fields
          format-symbol-name
          make-state
          update-in
          update-state
          with-state
          print-state
          update-state-with-alist
          resolve-procedure
          partial
          remove
          remp
          contains?
          insert-sorted
          for-each-while)

  (begin

    (define-macro (with-state state field-list . body)

      (define (make-field-ref state-name field-name real-name)
        `(,field-name (table-ref ,state-name (quote ,real-name))))

      (define (make-field-bindings state-name field-list)
        (fold (lambda (field-spec field-forms)
                (append field-forms
                        (cond
                         ((symbol? field-spec)
                          (list (make-field-ref state-name field-spec field-spec)))
                         ((list? field-spec)
                          ;; Is a field being renamed with 'as:'?
                          (if (equal? (cadr field-spec) as:)
                              (list (make-field-ref state-name (caddr field-spec) (car field-spec)))
                              (let* ((field-name (car field-spec))
                                     (binding-name (gensym field-name)))
                                (append (list (make-field-ref state-name binding-name field-name))
                                        (make-field-bindings binding-name (cdr field-spec)))))))))
              '()
              field-list))

      `(let* ,(make-field-bindings state field-list)
         ,@body))

    (define-macro (make-state . initial-fields)

      (define (_symbol-or-binding symbol)
        (let ((symbol-name (symbol->string symbol)))
          (if (equal? #\! (string-ref symbol-name 0))
              (string->symbol (substring symbol-name
                                         1 (string-length symbol-name)))
              `(quote ,(string->symbol symbol-name)))))

      (define (_make-state-table initial-fields)
        (let ((initial-size (max (* 2 (length initial-fields))
                                 5)))
          `(make-table size: ,initial-size init: #f)))

      (define (_make-state state initial-fields)
        (map (lambda (field-pair)
               (let ((field-sym (_symbol-or-binding (car field-pair))))
                 (if (and (list? (cadr field-pair))
                          (> (length (cadr field-pair)) 1)
                          (equal? (caadr field-pair) '>))
                     (let ((temp-sym (gensym (car field-pair)))
                           (nested-fields (cdadr field-pair)))
                       `(let ((,temp-sym ,(_make-state-table nested-fields)))
                          ,@(apply _make-state `(,temp-sym ,nested-fields))
                          (state-set! ,state ,field-sym ,temp-sym)))
                     `(state-set! ,state ,field-sym ,(cadr field-pair)))))
             initial-fields))

      (let ((state-sym    (gensym 'new-state-))
            (initial-size (max (* 2 (length initial-fields))
                               5)))
        `(let ((,state-sym ,(_make-state-table initial-fields)))
           ,@(apply _make-state `(,state-sym ,initial-fields))
           ,state-sym)))

    (define-macro (update-state state . update-pairs)

      (define (_make-state-table initial-fields)
        (let ((initial-size (max (* 2 (length initial-fields))
                                 5)))
          `(make-table size: ,initial-size init: #f)))

      (define (_symbol-or-binding symbol)
        (let ((symbol-name (symbol->string symbol)))
          (if (equal? #\! (string-ref symbol-name 0))
              (string->symbol (substring symbol-name
                                         1 (string-length symbol-name)))
              `(quote ,(string->symbol symbol-name)))))

      (define (_update-state state . update-pairs)
        (map (lambda (update-pair)
               (let ((field-sym (_symbol-or-binding (car update-pair))))
                 (if (and (list? (cadr update-pair))
                          (equal? (caadr update-pair) '>))
                     (let ((temp-sym (gensym (car update-pair))))
                       `(let ((,temp-sym (or (table-ref ,state ,field-sym)
                                             ,(_make-state-table '()))))
                          ,@(apply _update-state `(,temp-sym ,@(cdadr update-pair)))
                          (state-set! ,state ,field-sym ,temp-sym)))
                     `(state-set! ,state ,field-sym ,(cadr update-pair)))))
             update-pairs))

      (let ((state-sym (gensym 'state-)))
        `(let ((,state-sym ,state))
           ,@(apply _update-state `(,state-sym ,@update-pairs))
           ,state-sym)))

    (define (remove value list)
      (fold-right (lambda (item result)
                    (if (equal? item value)
                        result
                        (cons item result)))
                  '()
                  list))

    (define (remp proc list)
      (fold-right (lambda (item result)
                    (if (proc item)
                        result
                        (cons item result)))
                  '()
                  list))

    (define (contains? value items)
      (let ((found #f))
        (let check-next ((next-items items))
          (when (pair? items)
            (if (equal? value (car next-items))
                (set! found #t)
                (check-next (cdr items)))))
        found))

    (define (insert-sorted item insert-test list)
      (if (pair? list)
          (if (insert-test item (car list))
              (cons item list)
              (cons (car list) (insert-sorted item insert-test (cdr list))))
          (cons item '())))

    (define (for-each-while while-test action list)
      (let ((remaining-items list))
        (let next-item ()
          (when (pair? remaining-items)
            (if (while-test (car remaining-items))
                (begin
                  (action (car remaining-items))
                  (set! remaining-items (cdr remaining-items))
                  (next-item)))))
        remaining-items))

    (define (update-at list index updater)
      (let ((i 0))
        (map (lambda (item)
               (set! i (+ i 1))
               (if (equal? index (- i 1))
                   (updater item)
                   item))
             list)))

    (define (pairs flat-list)
      (let ((pair-list '()))
        (let find-pair ((remaining flat-list))
          (when (pair? remaining)
            (set! pair-list (append pair-list (list (cons (car remaining)
                                                          (cadr remaining)))))
            (find-pair (cddr remaining))))
        pair-list))

    ;; This is also defined for macros in state-macros.scm
    (define (format-symbol-name symbol format-proc)
      (string->symbol (format-proc (symbol->string symbol))))

    (define (partial proc . args)
      (lambda (final-arg)
        (apply proc (append args (list final-arg)))))

    (define (add-method method-pair)
      (let ((method-pairs (if (list? method-pair)
                              method-pair
                              (list method-pair))))
        (lambda (method-list)
          (fold (lambda (method methods)
                  (update-in methods (car method) (lambda (v) (cdr method))))
                method-list
                method-pairs))))

    (define (remove-method method-name)
      (lambda (method-list)
        (if method-list
            (remp (lambda (method-pair)
                    (equal? (car method-pair) method-name))
                  method-list)
            method-list)))

    (define (resolve-proc-or-value proc-or-value . params)
      (if (procedure? proc-or-value)
          (apply proc-or-value params)
          proc-or-value))

    ;; TODO: Add recursive object updates!
    (define update-in
      (case-lambda
        ((alist key proc-or-value)
         (update-in alist `((,key . ,proc-or-value))))
        ((alist update-pairs)
         (fold (lambda (update-pair current-alist)
                 (let* ((key (car update-pair))
                        (proc-or-value (cdr update-pair))
                        (found #f)
                        (new-alist (map (lambda (pair)
                                          (if (equal? (car pair) key)
                                              (begin
                                                (set! found #t)
                                                (cons key (resolve-proc-or-value proc-or-value (cdr pair))))
                                              pair))
                                        current-alist)))
                   (if (equal? found #t)
                       new-alist
                       (append current-alist
                               (list (cons key (resolve-proc-or-value proc-or-value #f)))))))
               alist
               update-pairs))))

    (define (update-state-with-alist state alist)
      (fold (lambda (pair state)
              (let ((existing-value (state-ref state (car pair))))
                (state-set! state
                            (car pair)
                            (if (alist? (cdr pair))
                                ;; Can we merge the new value into state?
                                (cond
                                 ((state? existing-value)
                                  (update-state-with-alist existing-value (cdr pair)))
                                 ((and (equal? existing-value #f)
                                       (alist-was-converted? (cdr pair)))
                                  (update-state-with-alist (make-state) (cddr pair)))
                                 ((alist? existing-value)
                                  (merge-alists existing-value (cdr pair)))
                                 (else (cdr pair)))
                                (cdr pair)))))
            state
            alist))

    ;; NOTE: This currently doesn't aggregate sizes so that multiple
    ;; lists with entirely different values cause a size including all
    (define (initial-state-size initial-pair-lists)
      (let ((initial-size 5))
        (let list-loop ((pair-lists initial-pair-lists))
          (when (pair? pair-lists)
            (let* ((pair-list (car pair-lists))
                   (new-size (if (list? pair-list)
                                 (* 2 (length pair-list))
                                 0)))
              (when (> new-size initial-size)
                (set! initial-size new-size))

              (list-loop (cdr pair-lists)))))

        initial-size))

    (define state? table?)

    ;; TODO: Accept a port
    (define (print-state table #!optional (indent 0))
      (let ((padding (make-string (* indent 2) #\ )))
        (table-for-each (lambda (key value)
                          (if (table? value)
                              (begin
                                (println padding key ":")
                                (print-state value (+ indent 1)))
                              (println padding key ": " (with-output-to-string (lambda () (write value))))))
                        table)
        table))

    ;; TODO: Accept a port
    ;; (define (print-state state #!optional (indent 0))
    ;;   (pp state))

    (define (state-ref state key-or-path #!optional (default-value #!void))
      (if (list? key-or-path)
          (fold (lambda (key state-or-value)
                  (table-ref state-or-value key))
                state
                key-or-path)
          (table-ref state key-or-path)))

    ;; (define (state-ref state key-or-path #!optional (default-value #f))
    ;;   ;; TODO: Support key paths
    ;;   (let ((value-pair (assoc key-or-path state)))
    ;;     (if value-pair
    ;;       (cdr value-pair)
    ;;       default-value)))

    (define (state-set! state key proc-or-value)
      ;; TODO: Support key paths
      (table-set! state
                  key
                  (if (procedure? proc-or-value)
                      (proc-or-value (table-ref state key))
                      proc-or-value))
      state)

    (define (state-fields state)
      (table->list state))

    ;; (define (state-set! state key proc-or-value)
    ;;   ;; TODO: Support key paths
    ;;   (assoc-in key
    ;;             (if (procedure? proc-or-value)
    ;;               (proc-or-value (state-ref state key))
    ;;               proc-or-value)
    ;;             state))

    (define (resolve-procedure proc-or-symbol)
      ;; TODO: In release builds, use macro to pass proc
      ;;       through so it can be inlined?
      (if (symbol? proc-or-symbol)
          (eval proc-or-symbol)
          proc-or-symbol))))

    ;; (define^ (_keyword-or-binding keyword)
    ;;   (let ((keyword-name (keyword->string keyword)))
    ;;     (if (equal? #\! (string-ref keyword-name 0))
    ;;         (string->symbol (substring keyword-name
    ;;                                    1 (string-length keyword-name)))
    ;;         `(quote ,(string->symbol keyword-name)))))

    ;; (define^ (_keyword->symbol keyword)
    ;;   (string->symbol (keyword->string keyword)))

    ;; (define^ (_update-state-new state . update-pairs)
    ;;   (let ((forms '()))
    ;;     (let next-pair ((remaining-forms update-pairs))
    ;;       (when (pair? remaining-forms)
    ;;         (let ((key (car remaining-forms)))
    ;;           (cond
    ;;            ((keyword? key)
    ;;             ;; Assume next form is value?
    ;;             (let ((value (cadr remaining-forms))
    ;;                   (field-sym (_keyword-or-binding key)))
    ;;               (set! forms
    ;;                 (append forms
    ;;                   (if (and (list? value)
    ;;                           (keyword? (car value)))
    ;;                       (let ((temp-sym (gensym (_keyword->symbol (car value)))))
    ;;                         `((let ((,temp-sym (or (table-ref ,state ,field-sym)
    ;;                                               ,(_make-state-table '()))))
    ;;                             ,@(apply _update-state-new `(,temp-sym ,@value))
    ;;                             (state-set! ,state ,field-sym ,temp-sym))))
    ;;                       `((state-set! ,state ,field-sym ,value))))))
    ;;             (set! remaining-forms (cddr remaining-forms)))

    ;;            ((symbol? key)
    ;;             (set! forms (append forms `((state-set! ,state ,(_symbol-or-binding key) ,key))))
    ;;             (set! remaining-forms (cdr remaining-forms)))
    ;;            (else (raise (string-append "Key is not a symbol or keyword: " key))))

    ;;           (next-pair remaining-forms))))

    ;;     forms))

    ;; (define-macro (update-state-new state . update-pairs)
    ;;   (let ((state-sym (gensym 'state-)))
    ;;     `(let ((,state-sym ,state))
    ;;           ,@(apply _update-state-new `(,state-sym ,@update-pairs))
    ;;           ,state-sym)))

    ;; ;; (pp (lambda (local)
    ;; ;;       (update-state-new state
    ;; ;;         local
    ;; ;;         thing:  'value
    ;; ;;         nested: (doo: 42
    ;; ;;                  boo: "stuff")
    ;; ;;         oof:    '(1 2 3))))

    ;; (define^ (format-symbol-name symbol format-proc)
    ;;   (string->symbol (format-proc (symbol->string symbol))))
