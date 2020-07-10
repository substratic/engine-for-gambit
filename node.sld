;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine node)
  (import (gambit)
          (substratic engine state)
          (substratic engine events))
  (export make-node
          node-id
          node-type
          node-components
          add-method
          add-method2
          remove-method
          update-node
          render-node
          dispatch-events)

  (begin

    (define *next-node-id* 0)

    (define (get-next-node-id!)
      (set! *next-node-id* (+ *next-node-id* 1))
      *next-node-id*)

    (define (make-node type-symbol #!key (tag #f) (component-values '()) . component-procs)
      (fold (lambda (component-proc node)
              ;; Allow component procedures to be conditionally omitted
              (if (equal? component-proc #!void)
                  node
                  (component-proc node component-values)))
            (make-state
             (id        (get-next-node-id!))
             (type      type-symbol)
             (tag       tag)
             (handlers  '())
             (updaters  '())
             (listeners '())
             (renderers '()))
            component-procs))

    (define (node-id node)
      (state-ref node 'id))

    (define (node-type node)
      (state-ref node 'type))

    (define (node-tag node)
      (state-ref node 'tag))

    (define (node-components node)
      (fold (lambda (field filtered-fields)
              (case (car field)
                ((id tag type handlers listeners updaters renderers)
                 ;; Skip these
                 filtered-fields)
                (else (append filtered-fields (list field)))))
            '()
            (state-fields node)))

    (define-macro (add-method2 component method-name)
      (pp `(add-method (cons ',component (lambda (node context renderer)
                                          (,method-name node context renderer)))))
      `(add-method (cons ',component (lambda (node context renderer)
                                      (,method-name node context renderer)))))

    (define (add-method method-pair #!key (component #f))
      (let ((method-pairs (if (list? method-pair)
                              method-pair
                              (list method-pair))))
        (lambda (method-list)
          (fold (lambda (method methods)
                  (update-in methods (car method) (lambda (v) (cdr method))))
                method-list
                method-pairs))))

    (define (remove-method method)
      (lambda (method-list)
        (if method-list
            (remp (lambda (method-pair)
                    (equal? (car method-pair) method-name))
                  method-list)
            method-list)))

    (define (remove-component-methods node component)
      ;; Loop through method lists and remove all for the named component
      node)

    (define (dispatch-events node context event-sink)
      (let* ((event-send (car event-sink))
             (event-receive (cdr event-sink)))
        (let process-events ((events (event-receive #t)))
          (when (pair? events)
            (let* ((current-event (car events)))
              (set! node (apply-event-handlers node
                                               context
                                               current-event
                                               event-send))
              (process-events (append (cdr events)
                                      (event-receive #t))))))

        node))

    (define (invoke-node-listeners node context event-sink)
      (for-each (lambda (listener)
                  ((resolve-procedure (cdr listener)) node context event-sink))
                (or (state-ref node 'listeners)
                    '())))

    (define (update-node node context time-step parent-event-sink)
      (let ((event-sink (if (pair? parent-event-sink)
                            (car parent-event-sink)
                            parent-event-sink)))
        (set! node (fold (lambda (updater node)
                           ((resolve-procedure (cdr updater)) node context time-step event-sink))
                         node
                         (state-ref node 'updaters)))

        ;; Dispatch to update listeners
        (invoke-node-listeners node context event-sink)

        ;; Dispatch any events raised during the update
        (if (pair? parent-event-sink)
            (dispatch-events node context parent-event-sink)
            node)))

    (define (render-node node context renderer)
      ;; In some cases, a renderer will need to return a new state if
      ;; it needs to cache something that can only be calculated at
      ;; render time (like sizes of UI elements)
      (let ((renderers (state-ref node 'renderers)))
        (fold (lambda (r new-node)
                (let ((next-node ((resolve-procedure (cdr r)) new-node context renderer)))
                  (if (state? next-node)
                      next-node
                      node)))
              node
              renderers)))))
