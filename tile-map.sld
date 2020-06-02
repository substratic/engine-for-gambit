;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine tile-map)
  (import (gambit)
          (substratic engine state)
          (substratic engine config)
          (substratic engine assets)
          (substratic engine events)
          (substratic engine renderer)
          (substratic engine collision))
  (export make-tile-map
          render-tile-map
          check-tile-map-collision
          map-index->map-coords
          screen-coords->map-index
          screen-index->map-index)

  (begin

    (define (check-tile-map-collision node tile-map entity event-sink)
      (with-state entity ((position pos-x pos-y)
                          (movement vel (axis as: movement-axis))
                          (collider corner-points))
                  (let ((collider-rect (resolve-collider-rect entity))
                        (collided-rect #f))
                    (let next-corner-point ((movement-corner-points (collider-corners-for-motion corner-points
                                                                                                 vel
                                                                                                 movement-axis)))
                      (let* ((corner-x (exact (truncate (+ (caar movement-corner-points) pos-x))))
                             (corner-y (exact (truncate (+ (cdar movement-corner-points) pos-y))))
                             (map-index (map-coords->map-index tile-map corner-x corner-y))
                             (collision-data (get-tile-data tile-map map-index)))
                        (when (pair? collision-data)
                          (let check-tile-rect ((collision-rects collision-data))
                            (if (check-collision collider-rect (car collision-rects))
                                (set! collided-rect (car collision-rects))
                                (unless (not (pair? (cdr collision-rects)))
                                  (check-tile-rect (cdr collision-rects))))))
                        (if collided-rect
                            (event-sink (make-event 'entity/collision
                                                    source: node
                                                    target: entity))
                            (unless (not (pair? (cdr movement-corner-points)))
                              (next-corner-point (cdr movement-corner-points)))))))))

    (define (get-tile-collision-rect side width offset-x offset-y)
      (case side
        ((left)   (list offset-x offset-y width 32))
        ((right)  (list (+ offset-x (- 32 width)) offset-y width 32))
        ((top)    (list offset-x offset-y 32 width))
        ((bottom) (list offset-x (+ offset-y (- 32 width)) 32 width))))

    (define *tile-collision-rect-width* 5)

    (define (get-tile-collision-data tile-index tile-data world-x world-y)
      (let ((collision-sides (assoc tile-index tile-data)))
        (if collision-sides
            (map (lambda (side)
                   (get-tile-collision-rect side *tile-collision-rect-width*
                                            world-x world-y))
                 (cdr collision-sides))
            '())))

    (define (get-tile-data tile-map map-index)
      (with-state tile-map (map-vector width height
                                       (tile-set tile-width tile-height tile-data))
                  (if (and (>= map-index 0)
                           (< map-index (vector-length map-vector)))
                      (let ((world-coords (map-index->world-coords map-index
                                                                   width height
                                                                   tile-width tile-height)))
                        (get-tile-collision-data (vector-ref map-vector map-index)
                                                 tile-data
                                                 (car world-coords)
                                                 (cdr world-coords)))
                      #f)))

    (define (make-tile-map width height tile-set #!key (map-data '()))
      (let ((map-vector (tile-map-vector-set-from! (make-vector (* width height) 0)
                                                   map-data)))
        (make-state
         (width       width)
         (height      height)
         (tile-set    tile-set)
         (map-vector  map-vector))))

    (define (save-tile-map tile-map)
      (vector->list (state-ref tile-map 'map-vector)))

    (define (tile-map-vector-set-from! map-vector data-list)
      (let ((map-index 0))
        (for-each (lambda (tile-index)
                    (vector-set! map-vector map-index tile-index)
                    (set! map-index (+ map-index 1)))
                  data-list)
        map-vector))

    (define (tile-map-set-tile! tile-map map-x map-y tile-index)
      (with-state tile-map ((tile-map map-vector (width as: map-width)))
                  (vector-set! map-vector (+ map-x (* map-y map-width)) tile-index)))

    (define (map-index->map-coords map-index map-width map-height)
      (cons (modulo map-index map-width)
            (truncate (/ map-index map-width))))

    (define (map-index->world-coords map-index
                                     map-width map-height
                                     tile-width tile-height)
      (let ((map-coords (map-index->map-coords map-index map-width map-height)))
        (cons (* (car map-coords) tile-width)
              (* (cdr map-coords) tile-height))))

    (define (map-coords->map-index tile-map world-x world-y)
      (with-state tile-map ((width as: map-width)
                            (tile-set tile-width tile-height))
                  (let* ((tile-x (max (truncate (/ world-x tile-width)) 0))
                         (tile-y (max (truncate (/ world-y tile-height)) 0)))
                    (+ tile-x (* tile-y map-width)))))

    (define (screen-coords->map-index screen-x screen-y
                                      screen-width screen-height
                                      map-width map-height
                                      tile-width tile-height)
      (let* ((start-x     (max (truncate (/ screen-x tile-width)) 0))
             (start-y     (max (truncate (/ screen-y tile-height)) 0))
             (row-width   (min (truncate (/ screen-width tile-width))
                               (- map-width start-x)))
             (num-rows    (min (truncate (/ screen-height tile-height))
                               (- map-height start-y)))
             (start-index (+ start-x (* start-y map-width))))
        start-index))

    (define (screen-index->map-index screen-index screen-width start-map-index map-width)
      (+ start-map-index
         ;; Offset for column position on screen
         (modulo screen-index screen-width)
         ;; Offset for number of rows on screen
         (* map-width (truncate (/ screen-index screen-width)))))

    (define (render-tile-map renderer tile-map viewport-x viewport-y screen-width screen-height #!key on-row-rendered)
      ;; This let determines the subsection of the map to be rendered on-screen
      ;; start-map-index - the map vector index of the first tile to render at screen 0, 0
      ;; row-width - the number of tiles to render per row
      ;; num-rows - the number of rows to be rendered
      (with-state tile-map ((width as: map-width)
                            (height as: map-height)
                            (tile-set tile-width tile-height tile-image tile-data)
                            map-vector)
                  (let* ((tile-image-tile-width  (/ (image-width tile-image) tile-width))
                         (tile-image-tile-height (/ (image-height tile-image) tile-height))
                         (start-x     (truncate (/ (abs viewport-x) tile-width)))
                         (start-y     (truncate (/ (abs viewport-y) tile-height)))
                         (offset-x    (modulo (abs viewport-x) tile-width)) ;; Pixel offset in the tile
                         (offset-y    (modulo (abs viewport-y) tile-height))
                         (scroll-x    (if (< viewport-x 0)
                                          (+ offset-x (* start-x tile-width))
                                          (* -1 offset-x)))
                         (scroll-y    (if (< viewport-y 0)
                                          (+ offset-y (* start-y tile-height))
                                          (* -1 offset-y)))

                         ;; Constrain the row width to the map size or visible tile area
                         (row-width   (min (- (truncate (/ screen-width tile-width)) start-x)
                                           (- map-width (if (>= viewport-x 0) start-x 0))))
                         (num-rows    (min (- (truncate (/ screen-height tile-height)) start-y)
                                           (- map-height (if (>= viewport-y 0) start-y 0))))
                         (start-map-index (screen-coords->map-index viewport-x viewport-y
                                                                    screen-width screen-height
                                                                    map-width map-height
                                                                    tile-width tile-height)))

                    (unless (or (<= row-width 0) ;; Don't render a map with no visible tiles
                                (<= num-rows 0))

                      (let draw-index ((screen-index 0))
                        (let* ((map-index (screen-index->map-index screen-index row-width
                                                                   start-map-index map-width))
                               (map-coords (map-index->map-coords map-index map-width map-height))
                               (map-x (car map-coords))
                               (map-y (cdr map-coords))
                               (world-x (* tile-width map-x))
                               (world-y (* tile-height map-y))

                               (screen-coords (map-index->map-coords screen-index row-width num-rows))
                               (screen-x (car screen-coords))
                               (screen-y (cdr screen-coords))

                               ;; Look up the tile image coordinates for the map index
                               (tile-index (vector-ref map-vector (+ map-x (* map-y map-width))))
                               (tile-coords (map-index->map-coords tile-index
                                                                   tile-image-tile-width
                                                                   tile-image-tile-height))
                               (tile-image-x (* (car tile-coords) tile-width))
                               (tile-image-y (* (cdr tile-coords) tile-height)))

                          ;; Invoke callback before rendering the row
                          (when (and on-row-rendered
                                     (equal? 0 (modulo screen-index row-width)))
                            ((resolve-procedure on-row-rendered) map-y))

                          (render-image-rect renderer
                                             tile-image
                                             tile-image-x
                                             tile-image-y
                                             tile-width
                                             tile-height
                                             (+ (* tile-width screen-x) scroll-x)
                                             (+ (* tile-height screen-y) scroll-y))

                          (when (render-colliders?)
                            (let ((collision-data (get-tile-collision-data tile-index tile-data world-x world-y)))
                              (when collision-data
                                (SDL_SetRenderDrawColor renderer 0 255 0 255)
                                (for-each (lambda (collision-rect)
                                            (SDL_RenderDrawRect renderer (apply rect collision-rect)))
                                          collision-data))))

                          (unless (equal? screen-index (- (* row-width num-rows) 1))
                            (draw-index (+ screen-index 1)))))))))))
