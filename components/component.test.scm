;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(import (_test)
        (alist)
        (state)
        (components component))

(test-group "Component functions"
  (test-group "apply overlay variables"
    (let ((test-component (make-component test
                            (a 3)
                            (b "stuff"))))
      (test-equal (make-state
                    (test (> (a 5))
                          (b "stuff")
                          (c 'Yep)))
                  (test-component (make-state) '((test . ((a . 5) (c . Yep)))))))))
