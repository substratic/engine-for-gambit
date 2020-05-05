;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine node)
  (import (gambit)
          (substratic engine state)
          (substratic engine events))
  (include "node.scm")
  (export make-node
          node-id
          node-type
          node-components
          update-node
          render-node
          dispatch-events))
