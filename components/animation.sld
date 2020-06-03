;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine components animation)
  (import (gambit)
          (substratic engine state)
          (substratic engine alist)
          (substratic engine components component))
  (export animation-component
          make-animation)

  (begin

    (define (animation-updater node context time-step event-sink)
      (with-state node (sprite
                        animation
                        (animation current-frames
                                   current-animation
                                   frame-time))
                  (let* ((new-frame-time (+ frame-time time-step))
                         (current-frame (car current-frames))
                         (frame-delay (car current-frame)))
                    (if (> new-frame-time frame-delay)
                        (let ((next-frames (if (pair? (cdr current-frames))
                                               (cdr current-frames)
                                               (cdr current-animation))))
                          (update-state node
                                        (sprite    (> (frame-index (cdar current-frames))))
                                        (animation (> (frame-time  (- new-frame-time frame-delay))
                                                      (current-frames next-frames)))))
                        (update-state node
                                      (animation (> (frame-time new-frame-time))))))))

    (define (animation-component initial-animation animations triggers)
      (let ((current-animation (assoc initial-animation animations)))
        (make-component animation
                        (current-animation current-animation)
                        (current-frames    (cdr current-animation))
                        (frame-time        0.0)
                        (animations        animations)
                        (triggers          triggers)
                        (updaters          (add-method `(animation ,@animation-updater)))
                        (handlers          (lambda (h) h)))))

    (define make-animation
      (case-lambda
        ((name frames)
         (make-animation name frames 0.2))
        ((name frames delay)
         (cons name (map (lambda (frame)
                           (cons delay frame))
                         frames)))))))
