;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define *sdl-event* #f)

(define (make-event type
                    #!key
                    (source #f)
                    (target #f)
                    ;; Possible values:
                    ;; 'circulate - circulates among node components
                    ;; 'bubble    - circulates and then bubbles up
                    ;; 'stop      - stops propagating the event immediately
                    (flow 'circulate)
                    (data '()))

  `(,type ,source ,target ,flow ,data))

(define (event-type event)
  (car event))

(define (event-source event)
  (cadr event))

(define (event-target event)
  (caddr event))

(define (event-flow event)
  (cadddr event))

(define (event-data event key)
  (let ((pair (assoc key (cadr (cdddr event)))))
    (if (pair? pair)
      (cdr pair)
      #f)))

(define (is-event-source? event node)
  (let ((source (event-source event)))
    (and source
         (state? source)
         (equal? (state-ref source 'id)
                 (state-ref node 'id)))))

(define (is-event-target? event node)
  (let ((target (event-target event)))
    (and target
         (or
           (and
             (number? target)
             (equal? target (state-ref node 'id)))
           (and
             (symbol? target)
             (equal? target (state-ref node 'tag)))
           (and
             (state? target)
             (equal? (state-ref target 'id)
                     (state-ref node 'id)))))))

(define make-event-sink
  (case-lambda
   (() (make-event-sink '()))
   ((initial-events)
    (letrec* ((events initial-events)
              (send-event (lambda (event)
                            (set! events (append events (list event)))
                            ;; Always return void in case send-event is
                            ;; used at the end of a handler or updater
                            #!void))
              (receive-events (case-lambda
                                (() (receive-events #f))
                                ((clear-events)
                                 (let ((received-events events))
                                   (when clear-events (set! events '()))
                                   received-events)))))
      (cons send-event receive-events)))))

(define (poll-sdl-events event-sink)
  (unless *sdl-event*
    (set! *sdl-event* (alloc-SDL_Event)))

  (let next-event ()
    (let ((has-event (SDL_PollEvent *sdl-event*)))
      (when (equal? has-event 1)
        (let* ((event-type (SDL_Event-type *sdl-event*))
               (game-event
                 (cond
                   ((equal? event-type SDL_QUIT)    '(engine/quit))
                   ((equal? event-type SDL_KEYDOWN) (make-keyboard-event *sdl-event* 'down))
                   ((equal? event-type SDL_KEYUP)   (make-keyboard-event *sdl-event* 'up))
                   ((equal? event-type SDL_MOUSEBUTTONUP) (make-mouse-event *sdl-event*))
                   (else #f))))
          (unless (equal? game-event #f)
            (event-sink game-event))

          (next-event))))))

(define (make-keyboard-event event event-type)
  (let* ((key-event (SDL_Event-key event))
         (keysym (SDL_KeyboardEvent-keysym key-event))
         (key (SDL_Keysym-sym keysym))
         (modifiers (SDL_Keysym-mod keysym))
         (repeat (equal? 0 (SDL_KeyboardEvent-repeat key-event))))
    (make-event 'keyboard data: `((direction . ,event-type)
                                  (key . ,key)
                                  (modifiers . ,modifiers)))))

;; TODO: Populate details from event
(define (make-mouse-event event)
  `(mouse/click))

(define (quit-event-handler event state event-sink)
  (when (and (equal? (event-type event) 'keyboard)
             (equal? (event-data event 'key) SDLK_ESCAPE))
    (event-sink (make-event 'engine/quit)))
  state)

(define (apply-event-handler event-handler event state event-sink)
  (let ((new-state ((resolve-procedure event-handler) event state event-sink)))
    (if (equal? new-state #!void)
        state
        new-state)))

(define (apply-event-handlers event state event-sink)
  (fold (lambda (event-handler fold-state)
          (apply-event-handler (cdr event-handler) event fold-state event-sink))
        state
        (state-ref state 'handlers)))

(define (send-event event)
  (if (string-starts-with? (symbol->string (car event)) "engine")
    (set! *pending-engine-events* (append *pending-engine-events* (list event)))
    (set! *pending-game-events* (append *pending-game-events*   (list event)))))

(define (message-event source message)
  (make-event 'message
              source: source
              data:   `((message . ,message))))
