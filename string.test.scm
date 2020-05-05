;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(import (_test)
        (substratic engine string))

(test-group "string-split"

  (test-group "splits a string into multiple parts"
    (test-equal '("C" "M" "p") (string-split "C-M-p" #\-)))

  (test-group "returns a string with no matching delimiters"
    (test-equal '("p") (string-split "p" #\-))))
