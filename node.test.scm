;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(import (_test)
        (substratic engine node)
        (substratic engine alist)
        (substratic engine state)
        (substratic engine events)
        (substratic engine components component))

(define (test-component a b)
  (make-component test
    (a        a)
    (b        b)
    (updaters (add-method '(foo . foo)))))

(define (a-handler event state event-sink)
  (case (event-type event)
    ((a/change)
     (event-sink (make-event 'b/change))
     (update-state state (component-a (> (a 2)))))))

(define (b-handler event state event-sink)
  (case (event-type event)
    ((b/change)
     (update-state state (component-b (> (b (lambda (b) (+ b 2)))))))))

(define (b-updater state time-step event-sink)
  (event-sink (make-event 'a/change))
  (update-state state (component-b (> (b 4)))))

(define (make-test-node)
  (make-node 'test
    (make-component component-a
      (a 1)
      (handlers (add-method '(a . a-handler))))
    (make-component component-b
      (b 2)
      (updaters (add-method '(b . b-updater)))
      (handlers (add-method '(b . b-handler))))))

(test-group "Node"
  (let ((node (make-node 'thing (test-component 6 9))))
    (test-group "retrieves id"
      (test-equal 1 (node-id node)))
    (test-group "retrieves type"
      (test-equal 'thing (node-type node)))
    (test-group "contains component state"
      (test-equal 6 (state-ref (state-ref node 'test) 'a)))
    (test-group "lists components"
      (test-equal '(test) (map (lambda (c) (car c)) (node-components node)))))


  (test-group "events circulate within a node's components during update"
    (let ((event-sink (make-event-sink))
          (test-node (make-test-node)))
      (set! test-node (update-node test-node 0.5 event-sink))
      (test-equal 2 (state-ref (state-ref test-node 'component-a) 'a))
      (test-equal 6 (state-ref (state-ref test-node 'component-b) 'b)))))
