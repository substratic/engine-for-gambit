;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-macro (case-key . cases)
  ;; Include for string-split
  ;; NOTE: Importing with URL prefix seems to be necessary,
  ;; it looks like module aliases aren't (yet) respected when
  ;; import is used in a macro body.
  (import (github.com/substratic engine string))

  (define (key-string->test key)
    (cond
     ((equal? key "a") '(equal? key SDLK_a))
     ((equal? key "b") '(equal? key SDLK_b))
     ((equal? key "c") '(equal? key SDLK_c))
     ((equal? key "d") '(equal? key SDLK_d))
     ((equal? key "e") '(equal? key SDLK_e))
     ((equal? key "f") '(equal? key SDLK_f))
     ((equal? key "g") '(equal? key SDLK_g))
     ((equal? key "h") '(equal? key SDLK_h))
     ((equal? key "i") '(equal? key SDLK_i))
     ((equal? key "j") '(equal? key SDLK_j))
     ((equal? key "k") '(equal? key SDLK_k))
     ((equal? key "l") '(equal? key SDLK_l))
     ((equal? key "m") '(equal? key SDLK_m))
     ((equal? key "n") '(equal? key SDLK_n))
     ((equal? key "o") '(equal? key SDLK_o))
     ((equal? key "p") '(equal? key SDLK_p))
     ((equal? key "q") '(equal? key SDLK_q))
     ((equal? key "r") '(equal? key SDLK_r))
     ((equal? key "s") '(equal? key SDLK_s))
     ((equal? key "t") '(equal? key SDLK_t))
     ((equal? key "u") '(equal? key SDLK_u))
     ((equal? key "v") '(equal? key SDLK_v))
     ((equal? key "w") '(equal? key SDLK_w))
     ((equal? key "x") '(equal? key SDLK_x))
     ((equal? key "y") '(equal? key SDLK_y))
     ((equal? key "z") '(equal? key SDLK_z))
     ((equal? key "TAB") '(equal? key SDLK_TAB))
     ((equal? key "C") '(equal? KMOD_LCTRL (bitwise-and mod KMOD_LCTRL)))
     (else #f)))

  (define (key-binding->test key-binding)
    (let* ((key-parts (string-split key-binding #\-))
           (predicates (map (lambda (key) (key-string->test key))
                            key-parts))
           ;; TODO: This is hacky, need a better way to detect it
           (predicates (if (equal? (length predicates) 1)
                           (cons '(equal? mod 0) predicates)
                           predicates)))
      `(and ,@predicates)))

  `(lambda (key mod)
           (cond
            ,@(map (lambda (binding)
                     `(,(key-binding->test (car binding))
                       ,@(cdr binding)))
                   cases)
            (else 'not-handled))))

(define (handle-key key-event . handlers)
  (when (equal? (event-data key-event 'direction) 'up)
    (let ((key (event-data key-event 'key))
          (mod (event-data key-event 'modifiers)))
      (let next-handler ((remaining-handlers handlers))
        (when (pair? remaining-handlers)
          (when (or (equal? (car remaining-handlers) #!void)
                    (equal? ((car remaining-handlers) key mod) 'not-handled))
            (next-handler (cdr remaining-handlers))))))))
