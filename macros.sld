;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine macros)
  (import (gambit))
  (export -> ->>)
  (begin

    ;; This module contains generally useful macros that might be moved out into
    ;; a separate library (and re-exported here) at a later date.

    (define-macro (-> . forms)
      (let ((result-sym (gensym 'pipeline-)))
        `(let ((,result-sym ,(car forms)))
           ,@(map (lambda (form)
                    `(set! ,result-sym ,(append (list (car form)) (list result-sym) (cdr form))))
                  (cdr forms))
           ,result-sym)))

    (define-macro (->> . forms)
      (let ((result-sym (gensym 'pipeline-)))
        `(let ((,result-sym ,(car forms)))
           ,@(map (lambda (form)
                    `(set! ,result-sym ,(append form (list result-sym))))
                  (cdr forms))
           ,result-sym)))))
