;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine components messages)
  (import (gambit)
          (substratic engine alist)
          (substratic engine state)
          (substratic engine assets)
          (substratic engine events)
          (substratic engine renderer)
          (substratic engine transform)
          (substratic engine components component))
  (include "messages.scm")
  (export messages-component))
