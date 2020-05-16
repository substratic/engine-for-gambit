;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine easing)
  (import (gambit))
  (export ease-in-out)
  (begin

    (define (ease-in-out value)
      (cond
        ((> value 0.75) (max 0.0 (- 1.0 (/ (- value 0.75) 0.25))))
        ((> value 0.25) 1.0)
        (else (min 1.0 (/ value 0.25)))))))
