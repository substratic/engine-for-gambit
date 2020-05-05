;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(import (_test)
        (substratic engine tile-map))

(test-group "Tile Map"
  (let* ((fake-image '(160 160 #f))
         (screen-width 160)
         (screen-height 160)
         (tile-width 32)
         (tile-height 32)
         (map-width 5)
         (map-height 5))

   (test-group "calculates tile index to map/tile coords"
     (test-equal '(0 . 0)(map-index->map-coords 0 5 5))
     (test-equal '(0 . 1) (map-index->map-coords 5 5 5))
     (test-equal '(2 . 3) (map-index->map-coords 17 5 5))
     (test-equal '(4 . 4) (map-index->map-coords 24 5 5)))
     ;; This should throw!
     ;; (test-equal (map-index->map-coords 25 5 5) '(4 . 4)))))

   (test-group "calculates screen coordinates to tile index"
     ;; Pos 0/0
     (test-equal 0 (screen-coords->map-index 0 0 160 160 5 6 32 32))
     ;; Pos 1/0
     (test-equal 1 (screen-coords->map-index 32 0 160 160 5 6 32 32))
     ;; Pos 0/1
     (test-equal 5 (screen-coords->map-index 0 32 160 160 5 6 32 32))
     ;; Pos 2/2
     (test-equal 12 (screen-coords->map-index 64 64 160 160 5 6 32 32))
     ;; Bottom right corner
     (test-equal 24 (screen-coords->map-index 138 138 160 160 5 6 32 32)))

   (test-group "maps screen index to map index"
     (test-equal 3 (screen-index->map-index 0 5 3 10))
     (test-equal 13 (screen-index->map-index 5 5 3 10))
     (test-equal 13 (screen-index->map-index 5 5 3 10)))))
