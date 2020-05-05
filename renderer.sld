;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine renderer)
  (import (gambit)
          (substratic sdl2)
          (substratic engine assets)
          (substratic engine transform))
  (include "renderer.scm")
  (export make-color
          color-r
          color-g
          color-b
          color-a
          render-rect
          render-fill-rect
          render-image-rect
          render-text-to-surface
          render-text))
