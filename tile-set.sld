;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine tile-set)
  (import (gambit)
          (substratic engine alist)
          (substratic engine state)
          (substratic engine assets))
  (export load-tile-set)

  (begin

    (define (load-tile-set tile-set-path)
      (let* ((tile-set (load-asset tile-set-path))
             (image-path #f)
             (tile-width #f)
             (tile-height #f)
             (tile-data #f))
        (when (not (equal? (car tile-set) 'tile-set))
          (raise (string-append "load-tile-set: Cannot load data of type " (car tile-set))))

        (for-each (lambda (pair)
                    (case (car pair)
                      ((image-path:)
                       (set! image-path (cdr pair)))
                      ((tile-data:)
                       (set! tile-data (cdr pair)))
                      ((tile-width:)
                       (set! tile-width (cdr pair)))
                      ((tile-height:)
                       (set! tile-height (cdr pair)))))
                  (pairs (cdr tile-set)))

        (make-state
         (file-path   tile-set-path)
         (image-path  image-path)
         (tile-image  (load-asset image-path))
         (tile-width  tile-width)
         (tile-height tile-height)
         (tile-data   tile-data))))))
