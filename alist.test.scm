;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(import (_test)
        (substratic engine alist))

(test-group "plist->alist"
  (test-group "converts a nested plist to nested alist"
    (test-equal '((a . 1) (b . ((d . ((e . "two"))))) (c . three))
                (plist->alist '(a: 1 b: (d: (e: "two")) c: three)))))

(test-group "alist->plist"
  (test-group "converts a nested alist to nested plist"
    (test-equal (alist->plist '((a . 1) (b . ((d . ((e . "two"))))) (c . three)))
                '(a: 1 b: (d: (e: "two")) c: three))))

(test-group "merge-alists"
  (test-group "merges nested alists"
    (test-equal (merge-alists '((a . 1)) '((a . 2) (b . ((d . 1) (e . 4)))) '((b . ((d . 3))) (c . 4)))
                '((a . 2) (b . ((d . 3) (e . 4))) (c . 4)))))
