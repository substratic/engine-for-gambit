;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine events)
  (import (gambit)
          (state)
          (substratic sdl2))
  (export make-event
          make-event-sink
          make-keyboard-event
          make-mouse-event
          send-event
          message-event
          apply-event-handler
          apply-event-handlers
          quit-event-handler
          poll-sdl-events
          event-type
          event-source
          event-target
          event-data
          is-event-source?
          is-event-target?)
  (begin

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
                     ((equal? event-type SDL_MOUSEBUTTONDOWN) (make-mouse-event *sdl-event* 'down))
                     ((equal? event-type SDL_MOUSEBUTTONUP)   (make-mouse-event *sdl-event* 'up))
                     (else #f))))
              (unless (equal? game-event #f)
                (event-sink game-event))

              (next-event))))))

    (define (make-keyboard-event event event-type)
      (let* ((key-event (SDL_Event-key event))
             (keysym (SDL_KeyboardEvent-keysym key-event))
             (key (SDL_Keysym-sym keysym))
             (modifiers (SDL_Keysym-mod keysym))
             (repeat (SDL_KeyboardEvent-repeat key-event)))
        ;; Avoid double-keypress events but allow future repeats
        (if (equal? repeat 1)
            #f
            (make-event 'keyboard data: `((direction . ,event-type)
                                          (key . ,key)
                                          (repeat . ,(- repeat 1))
                                          (modifiers . ,modifiers))))))

    (define (make-mouse-event event direction)
      (let* ((button-event (SDL_Event-button event))
             (button-value (SDL_MouseButtonEvent-button button-event))
             (button (cond
                      ((equal? button-value SDL_BUTTON_LEFT) 'left)
                      ((equal? button-value SDL_BUTTON_RIGHT) 'right)
                      ((equal? button-value SDL_BUTTON_MIDDLE) 'middle)))
             (pos-x (SDL_MouseButtonEvent-x button-event))
             (pos-y (SDL_MouseButtonEvent-y button-event)))
        (make-event 'mouse/button data: `((direction . ,direction)
                                          (button . ,button)
                                          (pos-x . ,pos-x)
                                          (pos-y . ,pos-y)))))

    (define (quit-event-handler node context event event-sink)
      (when (and (equal? (event-type event) 'keyboard)
                 (equal? (event-data event 'key) SDLK_ESCAPE))
        (event-sink (make-event 'engine/quit)))
      node)

    (define (apply-event-handler node context event-handler event event-sink)
      (let ((new-node ((resolve-procedure event-handler) node context event event-sink)))
        (if (equal? new-node #!void)
            node
            new-node)))

    (define (apply-event-handlers node context event event-sink)
      (fold (lambda (event-handler fold-node)
              (apply-event-handler fold-node context (cdr event-handler) event event-sink))
            node
            (state-ref node 'handlers)))

    (define (send-event event)
      (if (string-starts-with? (symbol->string (car event)) "engine")
          (set! *pending-engine-events* (append *pending-engine-events* (list event)))
          (set! *pending-game-events* (append *pending-game-events*   (list event)))))

    (define (message-event source message)
      (make-event 'message
                  source: source
                  data:   `((message . ,message))))))
