;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define (frame-index->frame-coords frame-index frame-columns frame-rows)
  (cons (modulo frame-index frame-columns)
        (truncate (/ frame-index frame-columns))))

(define (resolve-sprite-rect node)
  (with-state node ((position pos-x pos-y scale)
                    (sprite frame-width frame-height))
    `(,(+ pos-x)
      ,(+ pos-y)
      ,(* scale frame-width)
      ,(* scale frame-height))))

(define (sprite-renderer renderer node transform)
  (with-state node ((sprite   sprite-image frame-index
                              frame-width frame-height
                              frame-rows frame-columns)
                    (position pos-x pos-y scale))
    (let* ((frame-coords (frame-index->frame-coords frame-index
                                                    frame-columns
                                                    frame-rows)))
      (render-image-rect renderer
                         sprite-image
                         (* (car frame-coords) frame-width)
                         (* (cdr frame-coords) frame-height)
                         frame-width
                         frame-height
                         pos-x
                         pos-y
                         scale: scale))))

(define (sprite-component sprite-width sprite-height image-path)
  (let ((sprite-image (load-asset image-path)))
    (make-component sprite
      (sprite-image  sprite-image)
      (frame-width   sprite-width)
      (frame-height  sprite-height)
      (frame-columns (/ (image-width sprite-image) sprite-width))
      (frame-rows    (/ (image-height sprite-image) sprite-height))
      (frame-index   0)
      (rotation      0.0)
      (center        (cons (exact (floor (/ sprite-width 2)))
                           (exact (floor (/ sprite-height 2)))))
      (flip-x        #f)
      (flip-y        #f)
      (renderers     (add-method `(sprite ,@sprite-renderer))))))
