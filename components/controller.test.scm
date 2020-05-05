;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(import (substratic sdl2)
        (substratic engine node)
        (substratic engine state)
        (substratic engine events)
        (substratic engine components controller))

(define (make-test-object #!optional (direction-stack '()) (direction-mask 0))
  (make-node 'test
    (lambda (node overlay)
      (update-state node
          (controller (> (direction-stack direction-stack)
                         (direction-mask  direction-mask)
                         (button-mask     0)))))))

(define (test-equal-controller actual expected-stack expected-mask)
  (test-equal (make-state (direction-stack expected-stack)
                          (direction-mask  expected-mask)
                          (button-mask     0))
              (state-ref actual 'controller)))

(define (key-event key direction)
  (make-event 'keyboard data: `((direction . ,direction)
                                (key . ,key))))

(test-group "controller component"
    (test-group "adds and removes a direction"
      (let* ((node (make-test-object))
             (event-sink (make-event-sink))
             (send-event (car event-sink))
             (new-state (controller-handler (key-event SDLK_d 'down) node send-event)))

        (test-equal-controller new-state '(right) 8)

        (test-equal-controller (controller-handler (key-event SDLK_d 'up) new-state send-event)
                           '() 0)

        (test-equal `((entity/move right ,new-state)
                      (entity/move #f ,node))
                    ((cdr event-sink)))))

    (test-group "doesn't add the same direction twice"
      (let* ((node (make-test-object))
             (event-sink (make-event-sink))
             (send-event (car event-sink))
             (new-state (controller-handler (key-event SDLK_d 'down) node send-event)))
        (test-equal-controller (controller-handler (key-event SDLK_d 'down) new-state send-event)
                           '(right) 8)

        (test-equal `((entity/move right ,new-state))
                    ((cdr event-sink)))))

    (test-group "returns to previously held direction key"
      (let* ((node (make-test-object))
             (event-sink (make-event-sink))
             (send-event (car event-sink))
             (right-state (controller-handler (key-event SDLK_d 'down) node send-event))
             (up-state    (controller-handler (key-event SDLK_w 'down) right-state send-event))
             (no-up-state (controller-handler (key-event SDLK_w 'up)   up-state send-event)))
        (test-equal right-state no-up-state)
        (test-equal `((entity/move right ,right-state)
                      (entity/move up ,up-state)
                      (entity/move right ,right-state))
                    ((cdr event-sink)))))

    (test-group "removes earlier key from stack"
      (let* ((node (make-test-object))
             (event-sink (make-event-sink))
             (send-event (car event-sink))
             (right-state (controller-handler (key-event SDLK_d 'down) node send-event))
             (up-state    (controller-handler (key-event SDLK_w 'down) right-state send-event))
             (left-state  (controller-handler (key-event SDLK_a 'down) up-state send-event))
             (no-up-state (controller-handler (key-event SDLK_w 'up)   left-state send-event)))

        (test-equal-controller no-up-state '(left right) 12)

        ;; There shouldn't be a second event for 'left since we're
        ;; still moving in that direction
        (test-equal `((entity/move right ,right-state)
                      (entity/move up ,up-state)
                      (entity/move left ,left-state))
                    ((cdr event-sink))))))
