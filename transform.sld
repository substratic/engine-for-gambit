;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine transform)
  (import (gambit)
          (substratic sdl2))
  (export make-transform
          transform-x
          transform-y
          transform-width
          transform-height)
  (begin

    (define (make-transform x y width height)
      (list x y width height))

    (define (transform-x transform)
      (car transform))

    (define (transform-y transform)
      (cadr transform))

    (define (transform-width transform)
      (caddr transform))

    (define (transform-height transform)
      (cadddr transform))

    (define (transform-add transform transform-amount)
      (map (lambda (a b) (+ a b))
          transform
          transform-amount))))
