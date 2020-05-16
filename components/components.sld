;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine components)
  (import (gambit)
          (substratic engine alist)
          (substratic engine components fps)
          (substratic engine components fade)
          (substratic engine components sprite)
          (substratic engine components collider)
          (substratic engine components messages)
          (substratic engine components movement)
          (substratic engine components position)
          (substratic engine components animation)
          (substratic engine components component)
          (substratic engine components controller))
  (export make-component
          remove-component
          merge-alists  ;; This is used by make-component
          fps-component
          sprite-component
          collider-component
          messages-component
          movement-component
          position-component
          animation-component
          controller-component
          screen-fade-component))
