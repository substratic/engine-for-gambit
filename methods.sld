;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine methods)
  (import (gambit))
  (export method-table-set!
          method-table-ref
          define-method
          define-method-type)

  (begin

    (define _method-table #f)

    (define (method-table-ref method)
      (table-ref _method-table method))

    (define (method-table-set! method spec)
      (unless _method-table
        (set! _method-table (make-table)))

      (table-set! _method-table method spec))

    (define-macro (define-method-type method-spec)
      `(method-table-set! ',(car method-spec) ',(cdr method-spec)))

    (define-method-type (engine/renderer node context renderer))
    (define-method-type (engine/updater  node context time-step event-sink))
    (define-method-type (engine/handler  node context event event-sink))))
