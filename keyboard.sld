;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define-library (substratic engine keyboard)
  (import (gambit)
          (substratic sdl2)
          (substratic engine state)
          (substratic engine events)
          (substratic engine string))
  (export case-key
          handle-key

          ;; Re-export SDL2 key constants so that importers don't
          ;; need to import substratic/sdl2 also
          SDLK_UNKNOWN
          SDLK_RETURN
          SDLK_ESCAPE
          SDLK_BACKSPACE
          SDLK_TAB
          SDLK_SPACE
          SDLK_EXCLAIM
          SDLK_QUOTEDBL
          SDLK_HASH
          SDLK_PERCENT
          SDLK_DOLLAR
          SDLK_AMPERSAND
          SDLK_QUOTE
          SDLK_LEFTPAREN
          SDLK_RIGHTPAREN
          SDLK_ASTERISK
          SDLK_PLUS
          SDLK_COMMA
          SDLK_MINUS
          SDLK_PERIOD
          SDLK_SLASH
          SDLK_0
          SDLK_1
          SDLK_2
          SDLK_3
          SDLK_4
          SDLK_5
          SDLK_6
          SDLK_7
          SDLK_8
          SDLK_9
          SDLK_COLON
          SDLK_SEMICOLON
          SDLK_LESS
          SDLK_EQUALS
          SDLK_GREATER
          SDLK_QUESTION
          SDLK_AT
          SDLK_LEFTBRACKET
          SDLK_BACKSLASH
          SDLK_RIGHTBRACKET
          SDLK_CARET
          SDLK_UNDERSCORE
          SDLK_BACKQUOTE
          SDLK_a
          SDLK_b
          SDLK_c
          SDLK_d
          SDLK_e
          SDLK_f
          SDLK_g
          SDLK_h
          SDLK_i
          SDLK_j
          SDLK_k
          SDLK_l
          SDLK_m
          SDLK_n
          SDLK_o
          SDLK_p
          SDLK_q
          SDLK_r
          SDLK_s
          SDLK_t
          SDLK_u
          SDLK_v
          SDLK_w
          SDLK_x
          SDLK_y
          SDLK_z
          SDLK_CAPSLOCK
          SDLK_F1
          SDLK_F2
          SDLK_F3
          SDLK_F4
          SDLK_F5
          SDLK_F6
          SDLK_F7
          SDLK_F8
          SDLK_F9
          SDLK_F10
          SDLK_F11
          SDLK_F12
          SDLK_PRINTSCREEN
          SDLK_SCROLLLOCK
          SDLK_PAUSE
          SDLK_INSERT
          SDLK_HOME
          SDLK_PAGEUP
          SDLK_DELETE
          SDLK_END
          SDLK_PAGEDOWN
          SDLK_RIGHT
          SDLK_LEFT
          SDLK_DOWN
          SDLK_UP
          SDLK_NUMLOCKCLEAR
          SDLK_KP_DIVIDE
          SDLK_KP_MULTIPLY
          SDLK_KP_MINUS
          SDLK_KP_PLUS
          SDLK_KP_ENTER
          SDLK_KP_1
          SDLK_KP_2
          SDLK_KP_3
          SDLK_KP_4
          SDLK_KP_5
          SDLK_KP_6
          SDLK_KP_7
          SDLK_KP_8
          SDLK_KP_9
          SDLK_KP_0
          SDLK_KP_PERIOD
          SDLK_APPLICATION
          SDLK_POWER
          SDLK_KP_EQUALS
          SDLK_F13
          SDLK_F14
          SDLK_F15
          SDLK_F16
          SDLK_F17
          SDLK_F18
          SDLK_F19
          SDLK_F20
          SDLK_F21
          SDLK_F22
          SDLK_F23
          SDLK_F24
          SDLK_EXECUTE
          SDLK_HELP
          SDLK_MENU
          SDLK_SELECT
          SDLK_STOP
          SDLK_AGAIN
          SDLK_UNDO
          SDLK_CUT
          SDLK_COPY
          SDLK_PASTE
          SDLK_FIND
          SDLK_MUTE
          SDLK_VOLUMEUP
          SDLK_VOLUMEDOWN
          SDLK_KP_COMMA
          SDLK_KP_EQUALSAS400
          SDLK_ALTERASE
          SDLK_SYSREQ
          SDLK_CANCEL
          SDLK_CLEAR
          SDLK_PRIOR
          SDLK_RETURN2
          SDLK_SEPARATOR
          SDLK_OUT
          SDLK_OPER
          SDLK_CLEARAGAIN
          SDLK_CRSEL
          SDLK_EXSEL
          SDLK_KP_00
          SDLK_KP_000
          SDLK_THOUSANDSSEPARATOR
          SDLK_DECIMALSEPARATOR
          SDLK_CURRENCYUNIT
          SDLK_CURRENCYSUBUNIT
          SDLK_KP_LEFTPAREN
          SDLK_KP_RIGHTPAREN
          SDLK_KP_LEFTBRACE
          SDLK_KP_RIGHTBRACE
          SDLK_KP_TAB
          SDLK_KP_BACKSPACE
          SDLK_KP_A
          SDLK_KP_B
          SDLK_KP_C
          SDLK_KP_D
          SDLK_KP_E
          SDLK_KP_F
          SDLK_KP_XOR
          SDLK_KP_POWER
          SDLK_KP_PERCENT
          SDLK_KP_LESS
          SDLK_KP_GREATER
          SDLK_KP_AMPERSAND
          SDLK_KP_DBLAMPERSAND
          SDLK_KP_VERTICALBAR
          SDLK_KP_DBLVERTICALBAR
          SDLK_KP_COLON
          SDLK_KP_HASH
          SDLK_KP_SPACE
          SDLK_KP_AT
          SDLK_KP_EXCLAM
          SDLK_KP_MEMSTORE
          SDLK_KP_MEMRECALL
          SDLK_KP_MEMCLEAR
          SDLK_KP_MEMADD
          SDLK_KP_MEMSUBTRACT
          SDLK_KP_MEMMULTIPLY
          SDLK_KP_MEMDIVIDE
          SDLK_KP_PLUSMINUS
          SDLK_KP_CLEAR
          SDLK_KP_CLEARENTRY
          SDLK_KP_BINARY
          SDLK_KP_OCTAL
          SDLK_KP_DECIMAL
          SDLK_KP_HEXADECIMAL
          SDLK_LCTRL
          SDLK_LSHIFT
          SDLK_LALT
          SDLK_LGUI
          SDLK_RCTRL
          SDLK_RSHIFT
          SDLK_RALT
          SDLK_RGUI
          SDLK_MODE
          SDLK_AUDIONEXT
          SDLK_AUDIOPREV
          SDLK_AUDIOSTOP
          SDLK_AUDIOPLAY
          SDLK_AUDIOMUTE
          SDLK_MEDIASELECT
          SDLK_WWW
          SDLK_MAIL
          SDLK_CALCULATOR
          SDLK_COMPUTER
          SDLK_AC_SEARCH
          SDLK_AC_HOME
          SDLK_AC_BACK
          SDLK_AC_FORWARD
          SDLK_AC_STOP
          SDLK_AC_REFRESH
          SDLK_AC_BOOKMARKS
          SDLK_BRIGHTNESSDOWN
          SDLK_BRIGHTNESSUP
          SDLK_DISPLAYSWITCH
          SDLK_KBDILLUMTOGGLE
          SDLK_KBDILLUMDOWN
          SDLK_KBDILLUMUP
          SDLK_EJECT
          SDLK_SLEEP
          KMOD_NONE
          KMOD_LSHIFT
          KMOD_RSHIFT
          KMOD_LCTRL
          KMOD_RCTRL
          KMOD_LALT
          KMOD_RALT
          KMOD_LGUI
          KMOD_RGUI
          KMOD_NUM
          KMOD_CAPS
          KMOD_MODE
          KMOD_CTRL
          KMOD_SHIFT
          KMOD_ALT
          KMOD_GUI)
  (begin

    (define-macro (case-key . cases)
      ;; Include for string-split
      ;; NOTE: Importing with URL prefix seems to be necessary,
      ;; it looks like module aliases aren't (yet) respected when
      ;; import is used in a macro body.
      (import (github.com/substratic engine string))

      (define (key-string->test key)
        (cond
         ((equal? key "a") '(equal? key SDLK_a))
         ((equal? key "b") '(equal? key SDLK_b))
         ((equal? key "c") '(equal? key SDLK_c))
         ((equal? key "d") '(equal? key SDLK_d))
         ((equal? key "e") '(equal? key SDLK_e))
         ((equal? key "f") '(equal? key SDLK_f))
         ((equal? key "g") '(equal? key SDLK_g))
         ((equal? key "h") '(equal? key SDLK_h))
         ((equal? key "i") '(equal? key SDLK_i))
         ((equal? key "j") '(equal? key SDLK_j))
         ((equal? key "k") '(equal? key SDLK_k))
         ((equal? key "l") '(equal? key SDLK_l))
         ((equal? key "m") '(equal? key SDLK_m))
         ((equal? key "n") '(equal? key SDLK_n))
         ((equal? key "o") '(equal? key SDLK_o))
         ((equal? key "p") '(equal? key SDLK_p))
         ((equal? key "q") '(equal? key SDLK_q))
         ((equal? key "r") '(equal? key SDLK_r))
         ((equal? key "s") '(equal? key SDLK_s))
         ((equal? key "t") '(equal? key SDLK_t))
         ((equal? key "u") '(equal? key SDLK_u))
         ((equal? key "v") '(equal? key SDLK_v))
         ((equal? key "w") '(equal? key SDLK_w))
         ((equal? key "x") '(equal? key SDLK_x))
         ((equal? key "y") '(equal? key SDLK_y))
         ((equal? key "z") '(equal? key SDLK_z))
         ((equal? key "up") '(equal? key SDLK_UP))
         ((equal? key "down") '(equal? key SDLK_DOWN))
         ((equal? key "left") '(equal? key SDLK_LEFT))
         ((equal? key "right") '(equal? key SDLK_RIGHT))
         ((equal? key "S") '(> (bitwise-and mod KMOD_SHIFT) 0))
         ((equal? key "C") '(> (bitwise-and mod KMOD_CTRL) 0))
         ((equal? key "M") '(> (bitwise-and mod KMOD_ALT) 0))
         ((equal? key "TAB") '(equal? key SDLK_TAB))
         ((equal? key "SPC") '(equal? key SDLK_SPACE))
         ((equal? key "ESC") '(equal? key SDLK_ESCAPE))
         ((equal? key "RET") '(equal? key SDLK_RETURN))
         (else #f)))

      (define (key-binding->test key-binding)
        (let* ((key-parts (string-split key-binding #\-))
               (predicates (map (lambda (key) (key-string->test key))
                                key-parts))
               ;; TODO: This is hacky, need a better way to detect it
               (predicates (if (equal? (length predicates) 1)
                               (cons '(equal? mod 0) predicates)
                               predicates)))
          `(and ,@predicates)))

      `(lambda (key mod)
         (cond
          ,@(map (lambda (binding)
                   `(,(key-binding->test (car binding))
                     ,@(cdr binding)))
                 cases)
          (else 'not-handled))))

    (define (handle-key key-event #!key (direction 'down) #!rest handlers)
      (when (equal? (event-data key-event 'direction) direction)
        (let ((key (event-data key-event 'key))
              (mod (event-data key-event 'modifiers)))
          (let next-handler ((remaining-handlers handlers))
            (when (pair? remaining-handlers)
              (when (or (equal? (car remaining-handlers) #!void)
                        (equal? ((car remaining-handlers) key mod) 'not-handled))
                (next-handler (cdr remaining-handlers))))))))))
