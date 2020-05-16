;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

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
      proc-or-symbol))
