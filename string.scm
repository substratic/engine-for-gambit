;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define (string-starts-with? string prefix)
  (let ((prefix-length (string-length prefix)))
    (if (<= prefix-length (string-length string))
      (string=? (substring string 0 prefix-length) prefix)
      #f)))

(define (string-split string delimiter)
  (let ((parts '())
        (start-index 0))
    (let loop ((i 0))
      (if (< i (string-length string))
          (begin
            (when (equal? (string-ref string i) delimiter)
              (set! parts (append parts (list (substring string start-index i))))
              (set! start-index (+ i 1)))
            (loop (+ i 1)))
          (when (< start-index i)
            (set! parts (append parts (list (substring string start-index i)))))))

    parts))
