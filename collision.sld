;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine collision)
  (import (gambit)
          (substratic engine state))
  (export check-collision
          resolve-collider-rect
          collider-corners-for-motion)
  (begin

    (define (check-collision rect1 rect2)
      (let ((rect1-x (car rect1))
            (rect1-y (cadr rect1))
            (rect1-w (caddr rect1))
            (rect1-h (cadddr rect1))
            (rect2-x (car rect2))
            (rect2-y (cadr rect2))
            (rect2-w (caddr rect2))
            (rect2-h (cadddr rect2)))
        (and (< rect1-x (+ rect2-x rect2-w))
             (> (+ rect1-x rect1-w) rect2-x)
             (< rect1-y (+ rect2-y rect2-h))
             (> (+ rect1-y rect1-h) rect2-y))))

    (define (resolve-collider-rect node)
      (with-state node ((position pos-x pos-y scale)
                        (collider bounding-rect))
                  `(,(+ (car bounding-rect) pos-x)
                    ,(+ (cadr bounding-rect) pos-y)
                    ,(* scale (caddr bounding-rect))
                    ,(* scale (cadddr bounding-rect)))))

    (define (collider-corners-for-motion collider-corners vel axis)
      (let ((corner-accessors (cond
                               ((and (< vel 0) (equal? axis 'x)) (list car cadddr))
                               ((and (> vel 0) (equal? axis 'x)) (list cadr caddr))
                               ((and (< vel 0) (equal? axis 'y)) (list car cadr))
                               ((and (> vel 0) (equal? axis 'y)) (list caddr cadddr)))))
        (fold (lambda (accessor corner-list)
                (append corner-list (list (accessor collider-corners))))
              '()
              corner-accessors)))))
