;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define *message-duration* 4.0)
(define *message-fade-duration* 1.0)

(define (messages-handler event state event-sink)
  (case (event-type event)
   ((message)
    (update-state state
                  (messages (> (messages (lambda (messages)
                                           (append messages (list (cons (event-data event 'message)
                                                                        0.0)))))))))))
(define (update-messages time-step messages)
  (fold (lambda (message-details updated-messages)
          (let ((new-time (+ (cdr message-details) time-step)))
            (if (> new-time (+ *message-duration*
                               *message-fade-duration*))
                updated-messages
                (append updated-messages (list (cons (car message-details)
                                                     new-time))))))
        '()
        messages))

(define (messages-updater state time-step event-sink)
  (with-state state ((messages messages))
    ;; TODO: Move update-messages here
    (update-state state
      (messages (> (messages (partial update-messages time-step)))))))

(define (messages-renderer renderer node transform)
  (with-state node ((messages messages))
    (let* ((screen-height (transform-height transform))
           (message-count (length messages))
           (message-height 13)
           (message-y (- screen-height (* message-count message-height))))
      (for-each (lambda (message-details)
                  (let ((message-text (car message-details))
                        (message-time (cdr message-details)))
                    (render-text renderer
                                message-text
                                *default-font-small*
                                7 (- message-y 7)
                                alpha: (if (> message-time *message-duration*)
                                           (exact (truncate (* 255 (- 1.0 (/ (- message-time *message-duration*)
                                                                             *message-fade-duration*)))))
                                           255)))
                  (set! message-y (+ message-y message-height)))
                messages))))

(define (messages-component)
  (make-component messages
    (messages '())
    (handlers  (add-method `(messages ,@messages-handler)))
    (updaters  (add-method `(messages ,@messages-updater)))
    (renderers (add-method `(messages ,@messages-renderer)))))
