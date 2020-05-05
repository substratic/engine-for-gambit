;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(import (_test)
        (substratic engine state))

(test-group "state-ref"
  (let ((state (make-state
                (id  5)
                (type  'new-thing)
                (thing (> (a 3)
                          (b 2)
                          (c (> (inner "Nice!"))))))))
    (test-group "resolves single key"
      (test-equal (state-ref state 'id) 5)
      (test-equal (state-ref state '(id)) 5))

    (test-group "resolves key path"
      (test-equal (state-ref state '(thing a)) 3)
      (test-equal (state-ref state '(thing c inner)) "Nice!"))))

(test-group "state-fields"
  (let ((state (make-state
                (id  5)
                (type  'new-thing)
                (thing "stuff"))))

    (test-group "lists fields of the state object"
              (test-equal (state-fields state)
                      `((id . 5)
                        (thing . "stuff") ;; Cheating a bit on the order to make it match
                        (type . new-thing))))))

(test-group "update-in"
  (let ((node '((id . 1)
                (type . 'thing)
                (state . ((a . 1) (b . 2))))))

    (test-group "updates multiple fields"
      (test-equal
        (update-in node
                   `((type  . new-thing)
                     (id    . ,(lambda (id) (* 5 id)))
                     (new   . "I'm here")
                     (state . ,(lambda (s) (update-in s 'a 3)))))
        `((id    . 5)
          (type  . new-thing)
          (state . ((a . 3) (b . 2)))
          (new   . "I'm here"))))))

(test-group "update-state"
  (test-group "updates multiple and nested fields"
    (let ((node (make-state
                 (id 1)
                 (type 'thing)
                 (state (> (a 1) (b 2))))))
      (test-equal
        (update-state node
          (type  'new-thing)
          (id    (lambda (id) (* 5 id)))
          (new   "I'm here")
          (state (> (a 3))))

        (make-state
          (id    5)
          (type  'new-thing)
          (state (> (a 3) (b 2)))
          (new   "I'm here")))))

  (test-group "updates with different kinds of updater procs"
    (let ((node (make-state
                 (id 1)
                 (state (> (a  1)))))
          (id-updater   (lambda (x) (* 5 x)))
          (make-updater (lambda (mult)
                          (lambda (x)
                            (* x mult)))))
      (test-equal
        (update-state node
          (id    id-updater)
          (state (> (a (make-updater 4)))))

        (make-state
          (id    5)
          (state (> (a 4))))))))

(test-group "with-state"
  (let ((state (make-state
                 (id    5)
                 (type  'new-thing)
                 (thing (> (a 3)
                           (b 2)
                           (c (> (inner "Nice!"))))))))

    (test-group "resolves fields"
      (with-state state (id type)
        (test-equal id 5)
        (test-equal type 'new-thing)))

    (test-group "resolves nested fields"
      (with-state state (id
                         thing
                         (thing a (c inner)))
        (test-equal id 5)
        (test-equal thing (make-state
                            (a 3)
                            (b 2)
                            (c (> (inner "Nice!")))))
        (test-equal a 3)
        (test-equal inner "Nice!")))

    (test-group "renames fields"
      (with-state state ((id as: object-id)
                         (thing a (c (inner as: dinner))))
        (test-equal object-id 5)
        (test-equal a 3)
        (test-equal dinner "Nice!")))))

(test-group "insert-sorted"
  (test-group "inserts into an empty list"
    (test-equal (insert-sorted 3 <= '())
            '(3)))

  (test-group "inserts at the beginning of the list"
    (test-equal (insert-sorted 3 <= '(3 4 5 6))
            '(3 3 4 5 6)))

  (test-group "inserts inside of the list"
    (test-equal (insert-sorted 3 <= '(1 2 4 5))
            '(1 2 3 4 5)))

  (test-group "inserts at the end of the list"
    (test-equal '(0 1 2 3)
                (insert-sorted 3 <= '(0 1 2)))))


(test-group "for-each-while"
  (test-group "returns an empty list for an empty list"
    (test-equal (for-each-while (lambda (x) #t) (lambda (x) x) '())
            '()))

  (test-group "returns remaining items after performing operation"
    (let ((do-count 0))
      (test-equal '(3 3)
                  (for-each-while (lambda (x) (<= x 2))
                                  (lambda (x) (set! do-count (+ do-count 1)))
                                  '(1 1 2 2 3 3)))

      (test-equal do-count 4))))
