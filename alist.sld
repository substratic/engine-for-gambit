;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine alist)
  (import (gambit))
  (export alist?
          alist-was-converted?
          merge-alists
          alist->plist
          plist->alist
          pairs)
  (begin

    (define (plist->alist plist #!key (mark-lists #f))
      (let ((alist (map (lambda (pair)
                          (cons (string->symbol (keyword->string (car pair)))
                                (if (and (list? (cdr pair))
                                         (keyword? (cadr pair)))
                                    (plist->alist (cdr pair) mark-lists: mark-lists)
                                    (cdr pair))))
                        (pairs plist))))
        (if mark-lists
            (cons (cons '__plist #t) alist)
            alist)))

    (define (alist->plist alist)
      (fold (lambda (pair plist)
              (append plist (list (string->keyword (symbol->string (car pair)))
                                  (if (and (list? (cdr pair))
                                           (pair? (cadr pair)))
                                      (alist->plist (cdr pair))
                                      (cdr pair)))))
            '()
            alist))

    (define (alist? list)
      (and list
           (pair? list)
           (pair? (car list))))

    (define (merge-alists . alists)
      (fold (lambda (alist merged)
              (if alist
                  (fold (lambda (pair new-alist)
                          (let ((existing-value (assoc (car pair) new-alist)))
                            (assoc-in (car pair)
                                      (if (and (alist? (cdr pair))
                                               existing-value
                                               (alist? (cdr existing-value)))
                                          (merge-alists (cdr existing-value) (cdr pair))
                                          (cdr pair))
                                      new-alist)))
                        merged
                        alist)
                  merged))
            (car alists)
            (cdr alists)))

    (define (alist-was-converted? alist)
      (and (alist? alist)
          (equal? (caar alist) '__converted)))

    (define (assoc-in key value alist)
      (let* ((found #f)
             (result (map (lambda (pair)
                            (if (equal? (car pair) key)
                                (begin
                                  (set! found #t)
                                  (cons key value))
                                pair))
                          alist)))
        (if found
            result
            (append result (list (cons key value))))))

    (define (remove-in key value alist)
      (fold-right (lambda (pair result)
                    (if (equal? (car pair) result)
                        result
                        (cons pair result)))
                  '()
                  alist))

    (define (pairs flat-list)
      (let ((pair-list '()))
        (let find-pair ((remaining flat-list))
          (when (pair? remaining)
            (set! pair-list (append pair-list (list (cons (car remaining)
                                                          (cadr remaining)))))
            (find-pair (cddr remaining))))
        pair-list))))
