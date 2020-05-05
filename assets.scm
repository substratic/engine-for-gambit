;; Copyright (c) 2020 by David Wilson, All Rights Reserved.
;; Substratic Engine - https://github.com/substratic/engine
;;
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.

(define *assets-base-path* "./dist/assets")

(define (assets-base-path-set! assets-base-path)
  (set! *assets-base-path* assets-base-path))

(define (assets-path subpath)
  (path-normalize subpath #f *assets-base-path*))

(define *image-loader* #f)

(define (image-loader-set! image-loader)
  (set! *image-loader* image-loader))

(define (load-asset asset-path #!optional (asset-cache #f))
  (let ((ext (path-extension asset-path)))
    (cond
     ((equal? ext ".png")
      (if *image-loader*
          (*image-loader* (assets-path asset-path))
          (raise (string-append "load-asset: No *image-loader* is registered for file: " asset-path))))
     ((equal? ext ".scm")
      (read (open-file (assets-path asset-path))))
     (else (raise (string-append "load-asset: Unexpected file extension: " asset-path))))))

(define (set-rect! rect x y width height)
  (SDL_Rect-x-set! rect (exact (truncate x)))
  (SDL_Rect-y-set! rect (exact (truncate y)))
  (SDL_Rect-w-set! rect (exact (truncate width)))
  (SDL_Rect-h-set! rect (exact (truncate height)))
  rect)

(define (make-rect x y width height)
  (let ((rect (alloc-SDL_Rect)))
    (set-rect! rect x y width height)))

(define *reusable-rect* #f)

(define rect
  (case-lambda
   ((transform)
    (rect (transform-x transform)
          (transform-y transform)
          (transform-width  transform)
          (transform-height transform)))
   ((x y width height)
    (unless *reusable-rect*
      (set! *reusable-rect* (make-rect 0 0 0 0)))
    (set-rect! *reusable-rect* x y width height))))

(define (load-image renderer image-path)
  (let* ((img (IMG_Load image-path))
         (texture (SDL_CreateTextureFromSurface renderer img)))
    (list (SDL_Surface-w img) (SDL_Surface-h img) texture)))

(define (image-width image)
  (car image))

(define (image-height image)
  (cadr image))

(define (image-texture image)
  (caddr image))

(define (load-font font-path font-size)
  (let* ((font   (TTF_OpenFont font-path font-size))
         (height (TTF_FontHeight font)))
    (cons font height)))

(define (font-height font)
  (cdr font))

;; TODO: Implement font atlases!
(define (generate-font-atlas font output-path)
  #f)

(define *default-font* #f)
(define *default-font-small* #f)

(define (load-default-fonts)
  (set! *default-font* (load-font (assets-path "fonts/Thintel.ttf") 32))
  (set! *default-font-small* (load-font (assets-path "fonts/Thintel.ttf") 16)))
