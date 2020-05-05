;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine events)
  (import (gambit)
          (substratic sdl2)
          (substratic engine state))
  (include "events.scm")
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
          is-event-target?))
