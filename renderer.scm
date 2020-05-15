;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define (make-color r g b #!optional (a 255))
  (list r g b a))

(define (color-r color)
  (car color))

(define (color-g color)
  (cadr color))

(define (color-b color)
  (caddr color))

(define (color-a color)
  (cadddr color))

(define (render-clear renderer r g b)
  (SDL_SetRenderDrawColor renderer r g b 0)
  (SDL_RenderClear renderer))

(define (render-line renderer start-pos end-pos color)
  (SDL_SetRenderDrawBlendMode renderer SDL_BLENDMODE_BLEND)
  (apply SDL_SetRenderDrawColor (cons renderer color))
  (SDL_RenderDrawLine renderer (car start-pos) (cdr start-pos)
                               (car end-pos)   (cdr end-pos)))

(define (render-rect renderer draw-rect color)
  (SDL_SetRenderDrawBlendMode renderer SDL_BLENDMODE_BLEND)
  (apply SDL_SetRenderDrawColor (cons renderer color))
  (SDL_RenderDrawRect renderer (rect draw-rect)))

(define (render-fill-rect renderer fill-rect color)
  (SDL_SetRenderDrawBlendMode renderer SDL_BLENDMODE_BLEND)
  (apply SDL_SetRenderDrawColor (cons renderer color))
  (SDL_RenderFillRect renderer (rect fill-rect)))

(define *shared-src-rect* #f)
(define *shared-dest-rect* #f)

(define (render-image renderer image x y #!key (scale 1.0))
  (unless *shared-dest-rect*
    (set! *shared-dest-rect* (alloc-SDL_Rect)))

  (SDL_Rect-x-set! *shared-dest-rect* x)
  (SDL_Rect-y-set! *shared-dest-rect* y)
  (SDL_Rect-w-set! *shared-dest-rect* (exact (floor (* scale (image-width image)))))
  (SDL_Rect-h-set! *shared-dest-rect* (exact (floor (* scale (image-height image)))))
  (SDL_RenderCopy renderer (image-texture image) #f *shared-dest-rect*))

(define (render-image-rect renderer image src-x src-y src-width src-height dest-x dest-y #!key (scale 1.0))
  (unless *shared-src-rect*
    (set! *shared-src-rect* (alloc-SDL_Rect))
    (set! *shared-dest-rect* (alloc-SDL_Rect)))

  (SDL_Rect-x-set! *shared-src-rect* src-x)
  (SDL_Rect-y-set! *shared-src-rect* src-y)
  (SDL_Rect-w-set! *shared-src-rect* src-width)
  (SDL_Rect-h-set! *shared-src-rect* src-height)
  (SDL_Rect-x-set! *shared-dest-rect* (exact (floor dest-x)))
  (SDL_Rect-y-set! *shared-dest-rect* (exact (floor dest-y)))
  (SDL_Rect-w-set! *shared-dest-rect* (exact (floor (* scale src-width))))
  (SDL_Rect-h-set! *shared-dest-rect* (exact (floor (* scale src-height))))
  (SDL_RenderCopy renderer (image-texture image) *shared-src-rect* *shared-dest-rect*))

;; TODO: Need to resolve this after startup because `make-sdl-color`
;;       isn't ready yet in the new module-based approach...
;; (define *color-white* (make-sdl-color 255 255 255 0))

(define (render-text-to-surface text font #!key (color #f))
  (TTF_RenderText_Solid (car font) text (if color
                                            (apply make-sdl-color color)
                                            (make-sdl-color 255 255 255 0))))

(define (render-text renderer text font screen-x screen-y #!key (align 'left) (color #f) (alpha 255) (return-size #f))
  (let* ((font-surface (render-text-to-surface text font color: color))
         (font-texture (SDL_CreateTextureFromSurface renderer font-surface))
         (text-x screen-x)
         (text-y screen-y))
    (case align
      ((center)
       (set! text-x (- screen-x (exact (floor (/ (SDL_Surface-w font-surface) 2.0)))))
       (set! text-y (- screen-y (exact (floor (/ (SDL_Surface-h font-surface) 2.0))))))
      ((right)
       (set! text-x (- screen-x (SDL_Surface-w font-surface)))))
    (SDL_SetTextureAlphaMod font-texture alpha)
    (SDL_RenderCopy renderer font-texture #f (rect text-x text-y
                                                   (SDL_Surface-w font-surface)
                                                   (SDL_Surface-h font-surface)))
    (let ((text-size (cons (SDL_Surface-w font-surface)
                           (SDL_Surface-h font-surface))))
      (SDL_FreeSurface font-surface)
      (SDL_DestroyTexture font-texture)

      (when return-size text-size))))
