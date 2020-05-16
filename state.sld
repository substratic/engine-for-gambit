;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine state)
  (import (gambit)
          (substratic engine alist))
  (include "state-macros.scm"
           "state.scm")
  (export add-method
          remove-method
          state?
          state-ref
          state-set!
          state-fields
          format-symbol-name
          make-state
          update-in
          update-state
          with-state
          print-state
          update-state-with-alist
          resolve-procedure
          partial
          remove
          remp
          contains?
          insert-sorted
          for-each-while))
