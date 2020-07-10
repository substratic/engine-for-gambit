;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine loop)
  (import (gambit)
          (substratic sdl2)
          (node)
          (state)
          (events)
          (components))
  (export game-loop)
  (begin

    (define (engine-handler node context event event-sink)
      (case (car event)
        ((engine/change-mode)
         (update-state node (next-node (lambda (unused)
                                         (event-data event 'next-mode)))))
        ((engine/quit)
         (update-state node (quit #t)))))

    (define (prepare-root-node root-node show-fps)
      (when show-fps
        (set! root-node ((fps-component) root-node)))

      ;; Insert the engine handler at the beginning of the list
      ;; so that it can short cut application of remaining handlers
      (update-state root-node
                    (quit      #f)
                    (next-node #f)
                    (handlers (lambda (handlers) (cons `(engine ,@engine-handler) handlers))))

      root-node)

    (define (game-loop renderer
                       root-node
                       screen-width
                       screen-height
                       #!key
                       (event-sink #f)
                       (show-fps #f))
      (let* ((game-event-sink (or event-sink (make-event-sink)))
             (context (make-state (screen-width screen-width)
                                  (screen-height screen-height))))
        (set! root-node (prepare-root-node root-node show-fps))

        (let next-frame ((last-frame-time (SDL_GetTicks)))
          (let* ((current-frame-time (SDL_GetTicks))
                 (time-step (/ (- current-frame-time last-frame-time) 1000.0)))

            ;; Poll for SDL events
            (poll-sdl-events (car game-event-sink))

            ;; Dispatch new events
            (set! root-node (dispatch-events root-node context game-event-sink))

            ;; Any root node changes needed?
            (with-state root-node (quit next-node)
                        (cond
                         (next-node
                          (set! root-node (prepare-root-node (next-node) show-fps)))
                         (quit
                          (set! root-node #f))))

            (when root-node
              ;; Update the game state
              (set! root-node (update-node root-node context time-step game-event-sink))

              ;; Render the screen
              (set! root-node
                (render-node root-node context renderer))

              ;; Update the screen
              (SDL_RenderPresent renderer)

              ;; Print an error if any
              (if (not (equal? "" (SDL_GetError)))
                  (println (SDL_GetError)))

              ;; Run the next frame
              (next-frame current-frame-time))))))))
