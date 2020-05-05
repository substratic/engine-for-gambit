;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
