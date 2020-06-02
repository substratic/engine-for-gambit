;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine components collider)
  (import (gambit)
          (substratic engine alist)
          (substratic engine state)
          (substratic engine config)
          (substratic engine events)
          (substratic engine components component))
  (export collider-component)

  (begin

    (define (collider-handler event node event-sink)
      (case (event-type event)
        ((entity/collision)
         (with-state node ((collider on-collision))
                     (when on-collision
                       ((resolve-procedure on-collision) event node event-sink))))))

    (define (collider-renderer renderer node transform)
      ;; Check for a global flag for rendering colliders
      (when (render-colliders?)
        (let ((collider-rect (resolve-collider-rect node)))
          (SDL_SetRenderDrawColor renderer 0 255 0 255)
          (SDL_RenderDrawRect renderer (apply rect collider-rect)))))

    (define (collider-component left-top width-height #!key (on-collision #f))
      (let ((corner-points (list left-top
                                 ;; Top-right
                                 (cons (+ (car left-top) (car width-height))
                                       (cdr left-top))
                                 ;; Bottom-right
                                 (cons (+ (car left-top) (car width-height))
                                       (+ (cdr left-top) (cdr width-height)))
                                 ;; Bottom-left
                                 (cons (car left-top)
                                       (+ (cdr left-top) (cdr width-height))))))
        (make-component collider
                        (bounding-rect (list (car left-top)
                                             (cdr left-top)
                                             (car width-height)
                                             (cdr width-height)))
                        (corner-points corner-points)
                        (on-collision  on-collision)
                        (handlers      (add-method `(collider ,@collider-handler)))
                        (renderers     (add-method `(collider ,@collider-renderer))))))))
