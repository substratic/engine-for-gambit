;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine components controller)
  (import (gambit)
          (substratic sdl2)
          (substratic engine alist)
          (substratic engine state)
          (substratic engine config)
          (substratic engine events)
          (substratic engine components component))
  (export controller-component
          controller-handler)

  (begin

    (define (controller-handler event node event-sink)
      (with-state node ((controller direction-stack
                                    direction-mask
                                    button-mask))
                  (when (equal? (event-type event) 'keyboard)
                    (let* ((key (event-data event 'key))
                           (key-down? (equal? (event-data event 'direction) 'down))
                           (prev-direction (if (pair? direction-stack)
                                               (car direction-stack)
                                               #f))
                           ;; TODO: How to pick device, like gamepad, etc?
                           ;; TODO: Key remappings
                           (button (cond
                                    ((equal? key SDLK_w) '(up 1))
                                    ((equal? key SDLK_s) '(down 2))
                                    ((equal? key SDLK_a) '(left 4))
                                    ((equal? key SDLK_d) '(right 8))
                                    ;; Non-directional buttons have their own mask (?)
                                    ((equal? key SDLK_e) '(use 1))
                                    ((equal? key SDLK_q) '(sneak 2))
                                    (else #f))))

                      (when button
                        (case (car button)
                          ((up down left right)
                           (let ((direction-name  (car button))
                                 (direction-value (cadr button)))
                             (if (equal? (bitwise-and direction-mask direction-value)
                                         direction-value)
                                 ;; Already pressed, see if it was released
                                 (when (not key-down?)
                                   (begin
                                     (set! direction-stack (remove direction-name direction-stack))
                                     (set! direction-mask (bitwise-and (bitwise-not direction-value)
                                                                       direction-mask))))
                                 ;; Not pressed yet, add to stack and mask
                                 (begin
                                   (set! direction-stack (cons direction-name direction-stack))
                                   (set! direction-mask (bitwise-ior direction-mask direction-value))))

                             (let* ((new-direction (if (pair? direction-stack)
                                                       (car direction-stack)
                                                       #f))
                                    (new-state (update-state node
                                                             (controller (> (direction-stack direction-stack)
                                                                            (direction-mask direction-mask))))))
                               (unless (equal? prev-direction new-direction)
                                 (event-sink `(entity/move ,new-direction ,new-state)))

                               new-state)))

                          (else
                           (let* ((button-update (update-button-mask button key-down? button-mask))
                                  (button-event  (cdr button-update)))
                             (case (car button)
                               ((use)
                                (case button-event
                                  ((down) (event-sink (make-event 'usable/use-start source: node)))
                                  ((up)   (event-sink (make-event 'usable/use-end source: node)))))

                               ((sneak)
                                (case button-event
                                  ((up)   (event-sink (make-event 'player/toggle-sneak source: node))))))

                             (update-state node (controller (> (button-mask (car button-update)))))))))))))

    ;; Returns a cons of the updated button mask and a symbol of up/down or #f
    (define (update-button-mask button pressed button-mask)
      (let ((button-value (cadr button)))
        (if (equal? (bitwise-and button-mask button-value) button-value)
            (if (not pressed)
                (cons (bitwise-and (bitwise-not button-value) button-mask) 'up)
                (cons button-mask #f))
            (cons (bitwise-ior button-mask button-value) 'down))))

    ;; TODO: Consider a parameter that determines whether only
    ;; a single direction can be used at once.  Could also
    ;; wrap the handler with a lambda that bakes in the config
    (define (controller-component)
      (make-component controller
                      (direction-stack '())
                      (direction-mask  0)
                      (button-mask     0)
                      (handlers (add-method `(controller ,@controller-handler)))))))
