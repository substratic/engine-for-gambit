;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine components fade)
  (import (gambit))
  (export screen-fade-component)
  (begin

    (define (screen-fade-updater state time-step event-sink)
      (update-state state (screen-fade
                           (> (current-time (lambda (time)
                                              (+ time time-step)))))))

    (define (screen-fade-renderer renderer state transform)
      (with-state state ((screen-fade current-time duration))
        (render-fill-rect renderer
                          `(0 0 ,(transform-width transform) ,(transform-height transform))
                          `(0 0 0 ,(exact (floor (* 255
                                                    (- 1.0
                                                       (ease-in-out (/ current-time
                                                                       duration))))))))))

    (define (screen-fade-component #!key mode duration)
      (make-component screen-fade
        (mode         mode)
        (duration     duration)
        (current-time 0.0)
        (updaters     (add-method `(screen-fade ,@screen-fade-updater)))
        (renderers    (add-method `(screen-fade ,@screen-fade-renderer)))))))
