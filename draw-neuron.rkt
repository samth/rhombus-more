#lang racket/base
(require test-engine/racket-tests
         2htdp/image
         racket/local
         racket/math
         (for-syntax rhombus/macro))

(provide (all-defined-out))

;====== Graphics utilities ======

; place-near-end : Image Number Number Number Number Image -> Image
; rotate and place the given sprite on the given background image,
; near the end of the straight line from x1 y1 to x2 y2
(check-expect (place-near-end (triangle 20 "solid" "red")
                              100 100 200 100
                              (empty-scene 300 200))
              (place-image (triangle 20 "solid" "red")
                           155 100
                           (empty-scene 300 200)))
(check-expect (place-near-end (triangle 20 "solid" "red")
                              100 150 100 50
                              (empty-scene 300 200))
              (place-image (rotate 90 (triangle 20 "solid" "red"))
                           100 95
                           (empty-scene 300 200)))
(define (place-near-end sprite x1 y1 x2 y2 bg)
  (local [(define dx (- x2 x1))
          (define dy (- y2 y1))
          (define len (sqrt (+ (sqr dx) (sqr dy))))
          (define x (- x2 (/ dx len 1/45)))
          (define y (- y2 (/ dy len 1/45)))]
    (place-image (rotate (/ (atan dy dx) pi -1/180) sprite) x y bg)))

; weight->pen : Number -> Pen
; make a pen whose thickness and color depicts the given weight
(check-expect (weight->pen 1000)
              (pen (make-color 255 127 0) 25 "solid" "butt" "bevel"))
(check-expect (weight->pen -1000)
              (pen (make-color 0 127 255) 25 "solid" "butt" "bevel"))
(define (weight->pen w)
  (cond [(positive? w)
         (pen (make-color 255 127 0)
              (round (* (- (/ (+ 1 (exp (- w)))) 1/2) 50))
              "solid" "butt" "bevel")]
        [else
         (pen (make-color 0 127 255)
              (round (* (- 1/2 (/ (+ 1 (exp (- w))))) 50))
              "solid" "butt" "bevel")]))

; draw-weight : Number Number Number Number Number Image -> Image
; draw a weight w of a network connection from x1 y1 to x2 y2 on img
(define minus-sign (rectangle 20 5 "solid" "white"))
(define plus-sign (overlay minus-sign (rectangle 5 20 "solid" "white")))
(check-expect (draw-weight 1000 100 100 200 100 (empty-scene 300 200))
              (place-image plus-sign 155 100
                           (scene+line (empty-scene 300 200)
                                       100 100 200 100
                                       (pen (make-color 255 127 0) 25
                                            "solid" "butt" "bevel"))))
(check-expect (draw-weight -1000 100 150 100 50 (empty-scene 300 200))
              (place-image (rotate 90 minus-sign) 100 95
                           (scene+line (empty-scene 300 200)
                                       100 150 100 50
                                       (pen (make-color 0 127 255) 25
                                            "solid" "butt" "bevel"))))
(define (draw-weight w x1 y1 x2 y2 img)
  (place-near-end
   (if (positive? w) plus-sign minus-sign)
   x1 y1 x2 y2
   (scene+line img x1 y1 x2 y2 (weight->pen w))))

; draw-neuron : Number Number -> Image
; draw a neuron with the given size and activation level
(define (draw-neuron size level)
  (cond [(positive? level)
         (overlay (rectangle (* 2/3 size) (* 1/6 size) "solid" "white")
                  (rectangle (* 1/6 size) (* 2/3 size) "solid" "white")
                  (circle (/ size 2) "solid"
                          (make-color 255
                                      (round (- 255 (* +128 level)))
                                      (round (- 255 (* +255 level))))))]
        [else
         (overlay (rectangle (* 2/3 size) (* 1/6 size) "solid" "white")
                  (circle (/ size 2) "solid"
                          (make-color (round (- 255 (* -255 level)))
                                      (round (- 255 (* -128 level)))
                                      255)))]))

; draw-results : [Number Number -> Number] -> Image
; draw an array of output activation levels for different inputs x and y
(define levels (build-list 21 (lambda (n) (- (/ n 10) 1))))
(define spacer (square 5 "solid" "transparent"))
(define (draw-results f)
  (foldl (lambda (y lesser-rows)
           (above/align
             "right"
             (foldl (lambda (x lesser-cols)
                      (beside
                        lesser-cols
                        (draw-neuron 10 (f x y))))
                    (beside (draw-neuron 10 y) spacer)
                    levels)
             lesser-rows))
         (above spacer
                (foldl (lambda (x lesser-cols)
                         (beside lesser-cols
                                 (draw-neuron 10 x)))
                       empty-image
                       levels))
         levels))

; A Layout is (make-layout Number Number Number Number Formula)
(define-struct layout [x1 y1 x2 y2 formula])

; draw-weights : Environment [ListOf Layout] Image -> Image
; draw weights of network connections, as laid out in the given list
(define (draw-weights env lol background eval)
  (foldl (lambda (l img)
           (draw-weight (eval (layout-formula l) env)
                        (layout-x1 l) (layout-y1 l)
                        (layout-x2 l) (layout-y2 l)
                        img))
         background
         lol))

; A NeuronLayout is (make-neuron-layout Number Number String)
(define-struct neuron-layout [x y name])

; draw-neurons : [ListOf NeuronLayout] Image -> Image
; draw neurons as laid out in the given list
(define (draw-neurons lonl background)
  (foldl (lambda (nl img)
           (place-image (overlay (text (neuron-layout-name nl) 50 "black")
                                 (circle 35 "outline" "black")
                                 (circle 35 "solid" "white"))
                        (neuron-layout-x nl)
                        (neuron-layout-y nl)
                        img))
         background
         lonl))

(define draw_neurons draw-neurons)
(define draw_weights draw-weights)
(define Layout make-layout)
(define draw_results draw-results)
(define NeuronLayout make-neuron-layout)
(require 2htdp/universe)
(define-syntax-rule (big_bang a b c d e)
  (big-bang a [b c] [d e]))

(define-syntax rhombus-if
  (expression-transformer
   #'rhombus-if
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (alts)
       [(form-id test ... (alts alt ...)
                 . tail)
        (syntax-parse #'(alt ...)
          #:datum-literals (block)
          [(((~and tag-thn block) thn ...)
            ((~and tag-els block) els ...))
           (values
            #'(if (rhombus-expression (group test ...))
                  (rhombus-body-at tag-thn thn ...)
                  (rhombus-body-at tag-els els ...))
            #'tail)]
          [_
           (raise-syntax-error #f
                               "expected two alternatives"
                               stx)])]))))

