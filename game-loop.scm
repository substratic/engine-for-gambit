;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define (engine-handler event state event-sink)
  (case (car event)
   ((engine/change-mode)
    (update-state state (next-node (lambda (unused) (cadr event)))))
   ((engine/quit)
    (update-state state (quit #t)))))

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

(define (game-loop renderer root-node screen-width screen-height #!key (enable-rpc #f) (show-fps #f))
  (let* ((game-event-sink (make-event-sink)))
    (set! root-node (prepare-root-node root-node show-fps))

    (when enable-rpc
      (start-rpc-server 44311 (car game-event-sink)))

    (let next-frame ((last-frame-time (SDL_GetTicks)))
      (let* ((current-frame-time (SDL_GetTicks))
             (time-step (/ (- current-frame-time last-frame-time) 1000.0)))

        ;; Poll for SDL events
        (poll-sdl-events (car game-event-sink))

        ;; Dispatch new events
        (set! root-node (dispatch-events game-event-sink root-node))

        ;; Any root node changes needed?
        (with-state root-node (quit next-node)
          (cond
           (next-node
            (set! root-node (prepare-root-node (next-node renderer) show-fps)))
           (quit
            (set! root-node #f))))

        (when root-node
          ;; Update the game state
          (set! root-node (update-node root-node time-step game-event-sink))

          ;; Clear the screen
          (SDL_SetRenderDrawColor renderer #x00 #x00 #x00 #xFF)
          (SDL_RenderClear renderer)

          ;; Render the screen
          (set! root-node
            (render-node renderer root-node (list 0 0 screen-width screen-height)))

          ;; Update the screen
          (SDL_RenderPresent renderer)

          ;; Print an error if any
          (if (not (equal? "" (SDL_GetError)))
            (println (SDL_GetError)))

          ;; Run the next frame
          (next-frame current-frame-time))))))
