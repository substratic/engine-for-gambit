;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (core config)
  (import (gambit))
  (export render-colliders?
          render-colliders-set!)
  (begin
    (define *render-colliders* #f)

    (define (render-colliders?)
      *render-colliders*)
    (define (render-colliders-set! value)
      (set! *render-colliders* value))))
