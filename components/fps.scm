;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine components fps)
  (import (gambit)
          (substratic engine alist)
          (substratic engine state)
          (substratic engine assets)
          (substratic engine renderer)
          (substratic engine transform)
          (substratic engine components component))
  (export fps-component)

  (begin

    (define (fps-updater state time-step event-sink)
      (with-state state ((fps frames-per-second))
                  ;; Calculate FPS
                  (set! frames-per-second
                    (if frames-per-second
                        (+ (* frames-per-second 0.95) (* 0.05 (/ 1.0 time-step)))
                        (if (> time-step 0.0)
                            (/ 1.0 time-step)
                            #f)))
                  (update-state state (fps (> (frames-per-second frames-per-second))))))

    (define (fps-renderer renderer state transform)
      (with-state state ((fps frames-per-second))
                  (when frames-per-second
                    (let ((fps-string (number->string frames-per-second)))
                      (render-text renderer
                                   (substring fps-string 0 (min 6 (string-length fps-string)))
                                   *default-font-small*
                                   (- (transform-width transform) 30) 2)))))

    (define (fps-component)
      (make-component fps
                      (frames-per-second #f)
                      (updaters          (add-method `(fps ,@fps-updater)))
                      (renderers         (add-method `(fps ,@fps-renderer)))))))
