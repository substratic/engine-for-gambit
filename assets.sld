;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine assets)
  (import (gambit)
          (substratic sdl2)
          (substratic engine transform))
  (include "assets.scm")
  (export load-asset
          assets-path
          assets-base-path-set!
          image-loader-set!
          image-width
          image-height
          image-texture
          rect
          *default-font*
          *default-font-small*
          load-default-fonts))
