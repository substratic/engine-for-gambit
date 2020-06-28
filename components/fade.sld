;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine components fade)
  (import (gambit)
          (substratic engine alist)
          (substratic engine state)
          (substratic engine easing)
          (substratic engine renderer)
          (substratic engine components component))
  (export screen-fade-component)

  (begin

    (define (screen-fade-updater node context time-step event-sink)
      (update-state node (screen-fade
                           (> (current-time (lambda (time)
                                              (+ time time-step)))))))

    (define (screen-fade-renderer node context renderer)
      (with-state node ((screen-fade current-time duration))
        (render-fill-rect renderer
                          `(0 0 ,(state-ref context 'screen-width ) ,(state-ref context 'screen-height))
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
