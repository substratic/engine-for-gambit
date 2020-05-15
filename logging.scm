;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine logging)
  (import (gambit))
  (export log-value)
  (begin

    ;; Logs a message and all of the values provided then returns
    ;; the first value in the values arguments.  The additional
    ;; value arguments are only used for extra context in the
    ;; log statement.
    (define (log-value message . values)
      (println message)
      (for-each (lambda (value) (pp value)) values)
      (println "")
      (car values))))
