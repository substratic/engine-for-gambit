;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine rpc)
  (import (gambit))
  (export start-rpc-server)

  (begin

    (define *rpc-client-receiver* #f)

    (define (start-rpc-message-loop event-sink)
      ;; Store a procedure that can be used to send messages in this context
      (let ((rpc-output-port (current-output-port)))
        (set! *rpc-client-receiver* (lambda (message)
                                      (with-output-to-port rpc-output-port
                                        (lambda ()
                                          (write message)
                                          (newline)
                                          (force-output))))))

      (let next-message ()
        ;; TODO: Verify this is a usable message
        (let ((message (read)))
          ;; TODO: Log when message is invalid format
          (when (list? message)
            (case (car message)
              ((request)
               ;; (pp (string-append "Received request! " (symbol->string (cadr message))))
               ;; TODO: Error checking
               (let* ((args (caddr message))
                      (request-id (cdr (assoc 'request-id args)))
                      (callback (lambda (response)
                                  (*rpc-client-receiver*
                                   `(response ,(cadr message) ,(cons `(request-id . ,request-id)
                                                                     response))))))
                 (event-sink (make-event (cadr message)
                                         data: (cons `(callback . ,callback) args)))))
              ((event)
               (event-sink (make-event (cadr message)
                                       data: (if (list? (caddr message))
                                                 (caddr message)
                                                 '()))))))
          (next-message))))

    (define (start-rpc-server port event-sink)
      (println "Starting RPC server on port " port)
      (tcp-service-register!
       port
       (lambda ()
         (start-rpc-message-loop event-sink))))

    (define (send-rpc-message message)
      (when *rpc-client-receiver*
        (*rpc-client-receiver* message)))

    (define (rpc-client-connected?)
      (not (equal? *rpc-client-receiver #f)))))
