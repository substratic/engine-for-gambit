;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define (movement-updater node time-step event-sink)
  (with-state node (position
                    (position pos-x pos-y)
                    (movement vel axis))
    (let* ((axis-values (case axis
                         ((x) `(,pos-x pos-x))
                         ((y) `(,pos-y pos-y))
                         (else '(0 #f))))
           (pos-value (car axis-values))
           (pos-field (cadr axis-values))
           (new-pos (+ pos-value (* vel time-step))))
      (if pos-field
          (let ((new-state (update-state node (position (> (!pos-field new-pos))))))
            (event-sink (make-event 'entity/moved
                                    source: new-state))
            new-state)
          node))))

(define (movement-handler event node event-sink)
  (with-state node (id (movement vel vel-max accel axis))
    (let ((event-type (car event)))
      (when (and (equal? event-type 'entity/move)
                 (equal? (state-ref (caddr event) 'id) id))
        (case (cadr event)
          ((left)
           (set! axis 'x)
           (set! vel (* -1 vel-max)))
          ((right)
           (set! axis 'x)
           (set! vel vel-max))
          ((up)
           (set! axis 'y)
           (set! vel (* -1 vel-max)))
          ((down)
           (set! axis 'y)
           (set! vel vel-max))
          (else
           (set! axis #f)
           (set! vel 0)))

        (update-state node
          (movement (> (vel  vel)
                       (axis axis))))))))

;; For now this component is for movement only in a single direction at a
;; time.  New components may be added latter for different types of movement.
(define (movement-component #!key (vel 0.0) (vel-max 50.0) (accel 1.0) (axis #f))
  (make-component movement
    (vel      vel)
    (vel-max  vel-max)
    (accel    accel)
    (axis     axis) ;; x, y
    (updaters (add-method `(movement ,@movement-updater)))
    (handlers (add-method `(movement ,@movement-handler)))))
