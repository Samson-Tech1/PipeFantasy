;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |pipe waking up|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Pipe Fantasy 3
(require 2htdp/image)
(require 2htdp/universe)
(require racket/bool)


;; TASK 7
;; run: (pipe-fantasy EX-GAME)


(define-struct pipe [top bot left right])
;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean)
;; Interpretation: a pipe with openings in the given directions. A  #true for 
;; one of top, bot, left, right indicates an opening in that direction.
;; Examples:
(define PIPE-TB (make-pipe #true #true #false #false))
(define PIPE-LR (make-pipe #false #false #true #true))
(define PIPE-TL (make-pipe #true #false #true #false))
(define PIPE-TR (make-pipe #true #false #false #true))
(define PIPE-BL (make-pipe #false #true #true #false))
(define PIPE-BR (make-pipe #false #true #false #true))
(define PIPE-TBLR (make-pipe #true #true #true #true))
(define SPIPE-T (make-pipe #true #false #false #false))
(define SPIPE-B (make-pipe #false #true #false #false))
(define SPIPE-L (make-pipe #false #false #true #false))
(define SPIPE-R (make-pipe #false #false #false #true))
;; Template:
;; pipe-templ : Pipe -> ?
(define (pipe-templ p)
  (... (pipe-top p) ...
       (pipe-bot p) ...
       (pipe-left p) ...
       (piep-right p)))

(define ALL-PIPES (list PIPE-TB PIPE-LR PIPE-TL PIPE-TR PIPE-BL PIPE-BR PIPE-TBLR))

(define TILE-LENGTH 90)
(define PIPE-WIDTH 30)


(define-struct pipe-coords [pipe x y])
;; A PipeCoords is a (make-pipe-coords Pipe PosInt PosInt String)
;; Represents a pipe and its coordinates on a n x n grid where:
;; pipe is the pipe
;; x is the x coord
;; y is the y coord
;; Examples:
(define PC-1 (make-pipe-coords PIPE-LR 2 4)) 
(define PC-2 (make-pipe-coords PIPE-TL 1 2))
(define PC-3 (make-pipe-coords PIPE-TBLR 3 3))
;; Template:
;; pc-templ : PipeCoords -> ?
(define (pc-templ pc)
  (... (pipe-coords-pipe pc) ...
       (pipe-coords-x pc) ...
       (pipe-coords-y pc) ...))



(define-struct grid [n pipes])
;; A Grid is a (make-grid PosInt List)
;; Represents a n x n grid where:
;; n is the size of the grid
;; pipes is a list of placed pipes in which no x or y in a PipeCoords can be larger than n - 1
;; Examples:
(define GRID-1 (make-grid 5 (list PC-1 PC-2 PC-3)))
(define GRID-2 (make-grid 10 '()))
(define GRID-3 (make-grid 2 (list PC-2)))
(define GRID-4 (make-grid 4 (list PC-1 PC-3)))
(define GRID-5 (make-grid 6 (list PC-1)))
;; Template:
;; grid-templ : Grid -> ?
(define (grid-templ g)
  (... (grid-n g) ...
       (grid-pipes g) ...))


(define STARTING-GRID (make-grid 7 '()))



(define-struct goo-flow [pipes direction])
;; A GooFlow is a (make-goo-flow [List-of PipeCoords] [List-of String]
;; Represents the flow of goo through different pipes where:
;; - pipes is all the pipes that the goo has flowed through
;; - direction is the directions the goo is flowing in
;; Examples:
(define GF-1 (make-goo-flow (list (make-pipe-coords PIPE-TB 2 4)
                                  (make-pipe-coords PIPE-TL 2 3)
                                  (make-pipe-coords SPIPE-L 3 3))
                            (list "up" "up" "left")))
(define GF-2 (make-goo-flow (list (make-pipe-coords PIPE-LR 3 6)
                                  (make-pipe-coords PIPE-BR 2 6)
                                  (make-pipe-coords PIPE-TBLR 2 5)
                                  (make-pipe-coords SPIPE-T 2 4))
                            (list "right" "right" "up" "up")))
(define GF-3 (make-goo-flow (list (make-pipe-coords SPIPE-B 7 7))
                            (list "down")))
;; Template:
;; gf-temp : GooFlow -> ?
(define (gf-temp gf)
  (... (goo-flow-pipes gf) ...
       (first (goo-flow-direction gf) ...)))

(define SGF (make-goo-flow '() '()))



(define-struct game-state [grid pipes goo replaced time])
;; A GameState is a (make-game-state Grid List GooFlow Integer Integer)
;; Represents a game state where:
;; - grid is the grid of the game
;; - pipes is the list of incoming pipes
;; - goo is flow of goo
;; - replaced is the number of replaced pipes
;; - times is the number of ticks until the next goo is placed
;; Examples:
(define GS-1 (make-game-state STARTING-GRID (list PIPE-TL PIPE-TR PIPE-BL) SGF 0 140))
(define GS-2 (make-game-state STARTING-GRID (list PIPE-TBLR) SGF 0 140))
(define GS-3 (make-game-state STARTING-GRID (list PIPE-BL PIPE-LR PIPE-BR PIPE-TL PIPE-TBLR PIPE-TR) SGF 0 140))
;; Template:
;; gs-templ : GameState -> ?
(define (gs-templ gs)
  (... (grid-templ (game-state-grid gs) ...
                   (game-state-pipes gs) ...
                   (gs-temp (game-state-goo gs)) ...
                   (game-state-replaced gs) ...
                   (game-state-time gs))))






;; pipe->image: Pipe Integer Integer Boolean -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length.
;; If filled? then draw the pipe with goo.

(define (pipe->image pipe tile-side-length pipe-width filled? flow)
  (local [;; place-pipe-image [List-of String] String
          ;; creates a pipe
          (define (overlay-pipe spots color)
            (cond
              [(empty? spots) (overlay/align "middle" "middle"
                                             (square pipe-width "solid" color)
                                             (square tile-side-length "solid" "grey"))]
              [(cons? spots)
               (cond
                 [(string=? (first spots) "top")
                  (overlay/align "middle" "top" (square pipe-width "solid" color)
                                 (overlay-pipe (rest spots) color))]
                 [(string=? (first spots) "bottom")
                  (overlay/align "middle" "bottom" (square pipe-width "solid" color)
                                 (overlay-pipe (rest spots) color))]
                 [(string=? (first spots) "left")
                  (overlay/align "left" "middle" (square pipe-width "solid" color)
                                 (overlay-pipe (rest spots) color))]
                 [(string=? (first spots) "right")
                  (overlay/align "right" "middle" (square pipe-width "solid" color)
                                 (overlay-pipe (rest spots) color))])]))]
    (cond
      [(and (pipe-top pipe) (pipe-bot pipe) (pipe-left pipe) (pipe-right pipe))
       (cond
         [(string=? "all" flow)
          (overlay/align "left" "middle" (square pipe-width "solid" "green")
                         (overlay/align "right" "middle" (square pipe-width "solid" "green")
                                        (overlay-pipe (list "top" "bottom") (if filled? "green" "black"))))]
         [(or (string=? "up" flow) (string=? "down" flow))
          (overlay/align "left" "middle" (square pipe-width "solid" "black")
                         (overlay/align "right" "middle" (square pipe-width "solid" "black")
                                        (overlay-pipe (list "top" "bottom") (if filled? "green" "black"))))]
         [(or (string=? "left" flow) (string=? "right" flow))
          (overlay/align "middle" "top" (square pipe-width "solid" "black")
                         (overlay/align "middle" "bottom" (square pipe-width "solid" "black")
                                        (overlay-pipe (list "left" "right") (if filled? "green" "black"))))]
         [else (overlay-pipe (list "top" "bottom" "left" "right") (if filled? "green" "black"))])]
      [(and (pipe-top pipe) (pipe-bot pipe))
       (overlay-pipe (list "top" "bottom") (if filled? "green" "black"))]
      [(and (pipe-left pipe) (pipe-right pipe))
       (overlay-pipe (list "left" "right") (if filled? "green" "black"))]
      [(and (pipe-top pipe) (pipe-left pipe))
       (overlay-pipe (list "top" "left") (if filled? "green" "black"))]
      [(and (pipe-top pipe) (pipe-right pipe))
       (overlay-pipe (list "top" "right") (if filled? "green" "black"))]
      [(and (pipe-bot pipe) (pipe-left pipe))
       (overlay-pipe (list "bottom" "left") (if filled? "green" "black"))]
      [(and (pipe-bot pipe) (pipe-right pipe))
       (overlay-pipe (list "bottom" "right") (if filled? "green" "black"))]
      [(pipe-top pipe)
       (overlay-pipe (list "top") (if filled? "green" "black"))]
      [(pipe-bot pipe)
       (overlay-pipe (list "bottom") (if filled? "green" "black"))]
      [(pipe-left pipe)
       (overlay-pipe (list "left") (if filled? "green" "black"))]
      [(pipe-right pipe)
       (overlay-pipe (list "right") (if filled? "green" "black"))])))



;; pipe=? : Pipe Pipe -> Boolean
;; determines if two pipes are equal
(check-expect (pipe=? PIPE-TB PIPE-TB) #t)
(check-expect (pipe=? PIPE-TBLR PIPE-LR) #f)
(check-expect (pipe=? SPIPE-T PIPE-TR) #f)

(define (pipe=? p1 p2)
  (and (not (xor (pipe-top p1) (pipe-top p2)))
       (not (xor (pipe-bot p1) (pipe-bot p2)))
       (not (xor (pipe-left p1) (pipe-left p2)))
       (not (xor (pipe-right p1) (pipe-right p2)))))

(define (pipec=? p1 p2)
  (and (pipe=? (pipe-coords-pipe p1) (pipe-coords-pipe p2))
       (= (pipe-coords-x p1) (pipe-coords-x p2))
       (= (pipe-coords-y p1) (pipe-coords-y p2))))


;; remove-pipe : List PosInt PosInt -> List
;; removes a pipe from the list

(check-expect (remove-pipe (list PC-1 PC-2 PC-3) 2 4) (list PC-2 PC-3))
(check-expect (remove-pipe (list PC-1 PC-2 PC-3) 1 2) (list PC-1 PC-3))
(check-expect (remove-pipe  '() 3 7) '())  


(define (remove-pipe pipes row col)
  (cond
    [(empty? pipes) pipes]
    [(cons? pipes)
     (if (and (= (pipe-coords-x (first pipes)) row) (= (pipe-coords-y (first pipes))))
         (rest pipes)
         (cons (first pipes) (remove-pipe (rest pipes) row col)))]))


;; pipe-at: Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.
(check-expect (pipe-at GRID-1 2 4) PIPE-LR)
(check-expect (pipe-at GRID-1 2 2) #f)
(check-expect (pipe-at GRID-4 3 3) PIPE-TBLR)
(check-expect (pipe-at GRID-2 5 9) #f)
 
(define (pipe-at grid row col)
  (cond
    [(empty? (grid-pipes grid)) #f]
    [(cons? (grid-pipes grid))
     (if (and (= (pipe-coords-x (first (grid-pipes grid))) row)
              (= (pipe-coords-y (first (grid-pipes grid))) col))
         (pipe-coords-pipe (first (grid-pipes grid)))
         (pipe-at (make-grid (grid-n grid) (rest (grid-pipes grid))) row col))]
    ))


;; goo-pipe-at GooFlow Integer Integer -> Boolean
;; determines if there is already goo at given position
(check-expect (goo-pipe-at GF-1 2 4) #t)
(check-expect (goo-pipe-at GF-2 2 4) #t)
(check-expect (goo-pipe-at GF-1 5 7) #f)




(define (goo-pipe-at gf row col)
  (cond
    [(empty? (goo-flow-pipes gf)) #f]
    [(cons? (goo-flow-pipes gf))
     (if (and (= (pipe-coords-x (first (goo-flow-pipes gf))) row)
              (= (pipe-coords-y (first (goo-flow-pipes gf))) col))
         #t
         (goo-pipe-at (make-goo-flow (rest (goo-flow-pipes gf)) (goo-flow-direction gf)) row col))]
    ))



;; place-pipe: Grid Pipe Integer Integer -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.
#;(check-expect (place-pipe GRID-2 PIPE-TL 5 7)
                (make-grid 10 (list (make-pipe-coords PIPE-TL 5 7))))
#;(check-expect (place-pipe GRID-4 PIPE-TL 1 2)
                (make-grid 4 (list PC-2 PC-1 PC-3)))

(define (place-pipe grid pipe row col gs)
  (cond
    [(false? (pipe-at grid row col))
     (make-game-state
      (make-grid (grid-n grid)
                 (cons (make-pipe-coords pipe row col) (grid-pipes grid)))
      (rest (game-state-pipes gs))
      (game-state-goo gs)
      (game-state-replaced gs)
      (game-state-time gs))]
    [(pipe? (pipe-at grid row col))
     (if (goo-pipe-at (game-state-goo gs) row col)
         gs
         (make-game-state
          (make-grid (grid-n grid)
                     (cons (make-pipe-coords pipe row col) (remove-pipe (grid-pipes grid) row col)))
          (rest (game-state-pipes gs))
          (game-state-goo gs)
          (add1 (game-state-replaced gs))
          (game-state-time gs)))]))
 





;; grid->image: Grid Integer Integer -> Image
;; Draws the grid of pipes. Every tile should be a square with side length
;; tile-side-length and every pipe should have width pipe-width.
(define (grid->image grid tile-side-length pipe-width gf pipes)
  (local [;; goo-pipe-direction : Pipe -> String
          ;; finds the direction of pipe
          (define (goo-pipe-direction pipec)
            (local [(define (goo-pipe/acc n temp-pipes)
                      (cond
                        [(empty? temp-pipes) ""]
                        [(cons? temp-pipes)
                         (if (pipec=? (first temp-pipes) pipec)
                             (if (goo? pipec (rest temp-pipes)) "all"
                                 (list-ref (goo-flow-direction gf) n))
                             (goo-pipe/acc (add1 n) (rest temp-pipes)))]))]
              (goo-pipe/acc 0 (goo-flow-pipes gf))))      
          ;; grid-col : Grid PosInt -> Image
          ;; Produces a column of the given grid with n number of columns
          (define (grid-col grid n)
            (local [;; grid-row : Grid PosInt -> Image
                    ;; Produces a row of the given grid for column number n
                    (define (grid-row grid)
                      (cond
                        [(= (grid-n grid) 0)
                         (cond
                           [(false? (pipe-at grid (grid-n grid) n))
                            (overlay (square tile-side-length "outline" "black")
                                     (square tile-side-length "solid" "white"))]
                           [(pipe? (pipe-at grid (grid-n grid) n))
                            (pipe->image (pipe-at grid (grid-n grid) n)
                                         tile-side-length
                                         pipe-width
                                         (goo? (make-pipe-coords
                                                (pipe-at grid (grid-n grid) n)
                                                (grid-n grid) n)
                                               (goo-flow-pipes gf))
                                         (goo-pipe-direction (make-pipe-coords
                                                              (pipe-at grid (grid-n grid) n)
                                                              (grid-n grid) n)))])]
                        [(> (grid-n grid) 0)
                         (cond
                           [(false? (pipe-at grid (grid-n grid) n))
                            (beside (overlay (square tile-side-length "outline" "black")
                                             (square tile-side-length "solid" "white"))
                                    (grid-row (make-grid (- (grid-n grid) 1) (grid-pipes grid))))]
                           [(pipe? (pipe-at grid (grid-n grid) n))
                            (beside (pipe->image (pipe-at grid (grid-n grid) n)
                                                 tile-side-length
                                                 pipe-width
                                                 (goo? (make-pipe-coords
                                                        (pipe-at grid (grid-n grid) n)
                                                        (grid-n grid) n)
                                                       (goo-flow-pipes gf))
                                                 (goo-pipe-direction (make-pipe-coords
                                                                      (pipe-at grid (grid-n grid) n)
                                                                      (grid-n grid) n)))
                                    (grid-row (make-grid (- (grid-n grid) 1) (grid-pipes grid))))])]))]
              (cond
                [(= n 0)
                 (grid-row grid)]
                [(> (grid-n grid) 0)
                 (above (grid-row grid)
                        (grid-col grid (- n 1)))])))]
    
    (beside (grid-col grid (grid-n grid)) (incoming-pipes grid pipes tile-side-length pipe-width))))

;; incoming-pipes : Grid Number Number -> Image
;; produces an image of incoming pipes
(define (incoming-pipes grid pipes tile-side-length pipe-width)
  (cond
    [(empty? pipes) (rectangle tile-side-length
                               (* tile-side-length (- (grid-n grid) 2))
                               "solid" "white")]
    [(>= (length pipes) 3)
     (overlay/align "middle" "top" (pipe->image (first pipes)
                                                tile-side-length
                                                pipe-width
                                                #f "")
                    (overlay/align "middle" "middle"
                                   (pipe->image (second pipes)
                                                tile-side-length
                                                pipe-width
                                                #f "")
                                   (overlay/align "middle" "bottom"
                                                  (pipe->image (third pipes)
                                                               tile-side-length
                                                               pipe-width
                                                               #f "")
                                                  (rectangle tile-side-length
                                                             (* tile-side-length (- (grid-n grid) 2))
                                                             "solid" "white"))))]
    [(= (length pipes) 2)
     (overlay/align "middle" "top" (pipe->image (first pipes)
                                                tile-side-length
                                                pipe-width
                                                #f "")
                    (overlay/align "middle" "middle"
                                   (pipe->image (second pipes)
                                                tile-side-length
                                                pipe-width
                                                #f "")
                                   (rectangle tile-side-length
                                              (* tile-side-length (- (grid-n grid) 2))
                                              "solid" "white")))]
    [(= (length pipes) 1)
     (overlay/align "middle" "top" (pipe->image (first pipes)
                                                tile-side-length
                                                pipe-width
                                                #f "")
                    (rectangle tile-side-length
                               (* tile-side-length (- (grid-n grid) 2))
                               "solid" "white"))]))
                                                                



;; goo? : PipeCoords [List-of PipeCoords] -> Boolean
;; is true is the given pipe is in goo flow
(check-expect (goo? PC-1 (list PC-1 PC-2 PC-3)) #t)
(check-expect (goo? PC-2 (list PC-1 PC-2 PC-3)) #t)
(check-expect (goo? PC-1 (list PC-3 PC-2 PC-3)) #f)
(define (goo? pipec pipes)
  (cond
    [(empty? pipes) #f]
    [(list? pipes)
     (if (and (pipe=? (pipe-coords-pipe pipec) (pipe-coords-pipe (first pipes)))
              (= (pipe-coords-x pipec) (pipe-coords-x (first pipes)))
              (= (pipe-coords-y pipec) (pipe-coords-y (first pipes))))
         #t
         (goo? pipec (rest pipes)))]))




;; place-pipe-on-click : GameState Integer Integer MouseEvent -> GameState`
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile. If no pipes are available, does nothing.
(define (place-pipe-on-click gs x y mouse)
  (if (mouse=? mouse "button-up")
      (cond
        [(empty? (game-state-pipes gs))
         (make-game-state
          (game-state-grid gs)
          (game-state-pipes gs)
          (grid-goo-propogate (game-state-goo gs) (game-state-grid gs))
          (game-state-replaced gs)
          (game-state-time gs))]
        [(cons? (game-state-pipes gs))
         (place-pipe (game-state-grid gs) (first (game-state-pipes gs))
                     (- (grid-n (game-state-grid gs)) (floor (/ x TILE-LENGTH)))
                     (- (grid-n (game-state-grid gs)) (floor (/ y TILE-LENGTH))) gs)])
      gs))

;; render-grid : GameState -> Image
;; renders game state
(define (render-grid gs)
  (above (grid->image (game-state-grid gs) TILE-LENGTH PIPE-WIDTH
                      (game-state-goo gs) (game-state-pipes gs))
         (text (number->string (get-score gs)) 100 "black")))



;; start-pipe : PosInt -> Pipe
;; generates a start pipe based on given number
(check-expect (start-pipe 0) SPIPE-T)
(check-expect (start-pipe 1) SPIPE-B)
(check-expect (start-pipe 2) SPIPE-L)
(check-expect (start-pipe 3) SPIPE-R)

(define (start-pipe n)
  (cond
    [(= n 0) SPIPE-T]
    [(= n 1) SPIPE-B]
    [(= n 2) SPIPE-L]
    [(= n 3) SPIPE-R]))




;; grid-goo-propogate : GooFlow Grid -> GooFlow
;; moves the goo along a made grid
(check-expect (grid-goo-propogate GF-1
                                  (make-grid 7 (list (make-pipe-coords PIPE-BL 2 5)
                                                     (make-pipe-coords PIPE-TB 2 4)
                                                     (make-pipe-coords PIPE-TL 2 3)
                                                     (make-pipe-coords SPIPE-L 3 3))))
              (make-goo-flow (list (make-pipe-coords PIPE-BL 2 5)
                                   (make-pipe-coords PIPE-TB 2 4)
                                   (make-pipe-coords PIPE-TL 2 3)
                                   (make-pipe-coords SPIPE-L 3 3))
                             (list "left" "up" "up" "left")))
(check-expect (grid-goo-propogate GF-3
                                  (make-grid 7 (list (make-pipe-coords PIPE-TBLR 7 6)
                                                     (make-pipe-coords SPIPE-B 7 7))))
              (make-goo-flow (list (make-pipe-coords PIPE-TBLR 7 6)
                                   (make-pipe-coords SPIPE-B 7 7))
                             (list "down" "down")))
(check-expect (grid-goo-propogate (make-goo-flow (list (make-pipe-coords SPIPE-L 6 5)) (list "left"))
                                  (make-grid 7 (list (make-pipe-coords PIPE-LR 7 5)
                                                     (make-pipe-coords SPIPE-L 6 5))))
              (make-goo-flow (list (make-pipe-coords PIPE-LR 7 5)
                                   (make-pipe-coords SPIPE-L 6 5)) (list "left" "left")))
(check-expect (grid-goo-propogate (make-goo-flow (list (make-pipe-coords SPIPE-B 4 4)) (list "down"))
                                  (make-grid 7 (list (make-pipe-coords SPIPE-B 4 4)
                                                     (make-pipe-coords PIPE-TB 4 3))))
              (make-goo-flow (list (make-pipe-coords PIPE-TB 4 3)
                                   (make-pipe-coords SPIPE-B 4 4)) (list "down" "down")))

(define (grid-goo-propogate gf grid)
  (local [;; next-pipe : String -> [Optional Pipe]
          ;; finds the next pipe, return false if no pipe
          (define (next-pipe dir)
            (cond
              [(string=? dir "up")
               (cond
                 [(false? (pipe-at grid
                                   (pipe-coords-x (first (goo-flow-pipes gf)))
                                   (+ 1 (pipe-coords-y (first (goo-flow-pipes gf)))))) #f]
                 [(pipe? (pipe-at grid
                                  (pipe-coords-x (first (goo-flow-pipes gf)))
                                  (+ 1 (pipe-coords-y (first (goo-flow-pipes gf))))))
                  (pipe-at grid
                           (pipe-coords-x (first (goo-flow-pipes gf)))
                           (+ 1 (pipe-coords-y (first (goo-flow-pipes gf)))))])]
              [(string=? dir "down")
               (cond
                 [(false? (pipe-at grid
                                   (pipe-coords-x (first (goo-flow-pipes gf)))
                                   (- (pipe-coords-y (first (goo-flow-pipes gf))) 1))) #f]
                 [(pipe? (pipe-at grid
                                  (pipe-coords-x (first (goo-flow-pipes gf)))
                                  (- (pipe-coords-y (first (goo-flow-pipes gf))) 1)))
                  (pipe-at grid
                           (pipe-coords-x (first (goo-flow-pipes gf)))
                           (- (pipe-coords-y (first (goo-flow-pipes gf))) 1))])]
              [(string=? dir "left")
               (cond
                 [(false? (pipe-at grid
                                   (+ (pipe-coords-x (first (goo-flow-pipes gf))) 1)
                                   (pipe-coords-y (first (goo-flow-pipes gf))))) #f]
                 [(pipe? (pipe-at grid
                                  (+ (pipe-coords-x (first (goo-flow-pipes gf))) 1)
                                  (pipe-coords-y (first (goo-flow-pipes gf)))))
                  (pipe-at grid
                           (+ (pipe-coords-x (first (goo-flow-pipes gf))) 1)
                           (pipe-coords-y (first (goo-flow-pipes gf))))])]
              [(string=? dir "right")
               (cond
                 [(false? (pipe-at grid
                                   (- (pipe-coords-x (first (goo-flow-pipes gf))) 1)
                                   (pipe-coords-y (first (goo-flow-pipes gf))))) #f]
                 [(pipe? (pipe-at grid
                                  (- (pipe-coords-x (first (goo-flow-pipes gf))) 1)
                                  (pipe-coords-y (first (goo-flow-pipes gf)))))
                  (pipe-at grid
                           (- (pipe-coords-x (first (goo-flow-pipes gf))) 1)
                           (pipe-coords-y (first (goo-flow-pipes gf))))])]))]
    (cond
      [(string=? (first (goo-flow-direction gf)) "up")
       (if (pipe? (next-pipe "up"))
           (if (pipe-bot (next-pipe "up"))
               (make-goo-flow
                (cons (make-pipe-coords (next-pipe "up")
                                        (pipe-coords-x (first (goo-flow-pipes gf)))
                                        (+ 1 (pipe-coords-y (first (goo-flow-pipes gf)))))
                      (goo-flow-pipes gf))
                (cons (pipe-direction "up" (next-pipe "up")) (goo-flow-direction gf)))
               gf)
           gf)]
      [(string=? (first (goo-flow-direction gf)) "down")
       (if (pipe? (next-pipe "down"))
           (if (pipe-top (next-pipe "down"))
               (make-goo-flow
                (cons (make-pipe-coords (next-pipe "down")
                                        (pipe-coords-x (first (goo-flow-pipes gf)))
                                        (- (pipe-coords-y (first (goo-flow-pipes gf))) 1))
                      (goo-flow-pipes gf))
                (cons (pipe-direction "down" (next-pipe "down")) (goo-flow-direction gf)))
               gf)
           gf)]
      [(string=? (first (goo-flow-direction gf)) "left")
       (if (pipe? (next-pipe "left"))
           (if (pipe-right (next-pipe "left"))
               (make-goo-flow
                (cons (make-pipe-coords (next-pipe "left")
                                        (+ (pipe-coords-x (first (goo-flow-pipes gf))) 1)
                                        (pipe-coords-y (first (goo-flow-pipes gf))))
                      (goo-flow-pipes gf))
                (cons (pipe-direction "left" (next-pipe "left")) (goo-flow-direction gf)))
               gf)
           gf)]
      [(string=? (first (goo-flow-direction gf)) "right")
       (if (pipe? (next-pipe "right"))
           (if (pipe-left (next-pipe "right"))
               (make-goo-flow
                (cons  (make-pipe-coords (next-pipe "right")
                                         (- (pipe-coords-x (first (goo-flow-pipes gf))) 1)
                                         (pipe-coords-y (first (goo-flow-pipes gf))))
                       (goo-flow-pipes gf))
                (cons (pipe-direction "right" (next-pipe "right")) (goo-flow-direction gf)))
               gf)
           gf)])))
      

;; pipe-direction : String Pipe -> String
;; finds the new direction of the pipe given the direction goo is coming from
(check-expect (pipe-direction "up" PIPE-TBLR) "up")
(check-expect (pipe-direction "up" PIPE-BL) "left")
(check-expect (pipe-direction "up" PIPE-BR) "right")
(check-expect (pipe-direction "down" PIPE-TBLR) "down")
(check-expect (pipe-direction "down" PIPE-TL) "left")
(check-expect (pipe-direction "down" PIPE-TR) "right")
(check-expect (pipe-direction "left" PIPE-TBLR) "left")
(check-expect (pipe-direction "left" PIPE-TR) "up")
(check-expect (pipe-direction "left" PIPE-BR) "down")
(check-expect (pipe-direction "right" PIPE-TBLR) "right")
(check-expect (pipe-direction "right" PIPE-TL) "up")
(check-expect (pipe-direction "right" PIPE-BL) "down")

(define (pipe-direction dir pipe)
  (cond
    [(string=? dir "up")
     (cond
       [(pipe-top pipe) "up"]
       [(pipe-left pipe) "left"]
       [(pipe-right pipe) "right"])]
    [(string=? dir "down")
     (cond
       [(pipe-bot pipe) "down"]
       [(pipe-left pipe) "left"]
       [(pipe-right pipe) "right"])]
    [(string=? dir "left")
     (cond
       [(pipe-left pipe) "left"]
       [(pipe-top pipe) "up"]
       [(pipe-bot pipe) "down"])]
    [(string=? dir "right")
     (cond
       [(pipe-right pipe) "right"]
       [(pipe-top pipe) "up"]
       [(pipe-bot pipe) "down"])]))


;; start-goo : GameState -> GameState
;; creates starting goo flow


(define (start-goo gs)
  (cond
    [(pipe=? SPIPE-T (pipe-coords-pipe (first (grid-pipes (game-state-grid gs)))))
     (make-game-state (game-state-grid gs)
                      (game-state-pipes gs)
                      (make-goo-flow (grid-pipes (game-state-grid gs)) (list "up"))
                      (game-state-replaced gs)
                      (game-state-time gs))]
    [(pipe=? SPIPE-B (pipe-coords-pipe (first (grid-pipes (game-state-grid gs)))))
     (make-game-state (game-state-grid gs)
                      (game-state-pipes gs)
                      (make-goo-flow (grid-pipes (game-state-grid gs)) (list "down"))
                      (game-state-replaced gs)
                      (game-state-time gs))]
    [(pipe=? SPIPE-L (pipe-coords-pipe (first (grid-pipes (game-state-grid gs)))))
     (make-game-state (game-state-grid gs)
                      (game-state-pipes gs)
                      (make-goo-flow (grid-pipes (game-state-grid gs)) (list "left"))
                      (game-state-replaced gs)
                      (game-state-time gs))]
    [(pipe=? SPIPE-R (pipe-coords-pipe (first (grid-pipes (game-state-grid gs)))))
     (make-game-state (game-state-grid gs)
                      (game-state-pipes gs)
                      (make-goo-flow (grid-pipes (game-state-grid gs)) (list "right"))
                      (game-state-replaced gs)
                      (game-state-time gs))]))


;; gamestate-init : GameState -> GameState
;; creates starting game state

(define (gamestate-init gs)
  (if (empty? (grid-pipes (game-state-grid gs)))
      (start-goo (start-grid gs))
      gs))

;; start-grid : GameState -> GameState
;; places starting pipe randomly onto grid
(define (start-grid gs)
  (place-pipe (game-state-grid gs)
              (start-pipe (random 4))
              (+ 1 (random (- (grid-n (game-state-grid gs)) 2)))
              (+ 1 (random (- (grid-n (game-state-grid gs)) 2))) gs))





;; get-score : GameState -> Integer
;; returns the current score
(check-expect (get-score GAME-1) 0)
(check-expect (get-score GAME-2) 0)
(check-expect (get-score EX-GAME) 50)
(define (get-score gs)
  (* 50 (- (length (goo-flow-pipes (game-state-goo gs))) (game-state-replaced gs))))

;; timer : GameState -> GameState
;; operates timer
(check-expect (timer GAME-1) (make-game-state STARTING-GRID
                                (list PIPE-LR
                                      PIPE-TB
                                      PIPE-TBLR
                                      PIPE-TR
                                      PIPE-BL
                                      PIPE-TB
                                      PIPE-TL
                                      PIPE-BR
                                      PIPE-LR
                                      PIPE-TBLR
                                      PIPE-LR
                                      PIPE-TB
                                      PIPE-TL
                                      PIPE-LR
                                      PIPE-BR
                                      PIPE-LR
                                      PIPE-TBLR
                                      PIPE-BL
                                      PIPE-BR
                                      PIPE-TL
                                      PIPE-TR)
                                SGF 0 139))

(check-expect (timer GAME-2)  (make-game-state (make-grid 12 '())
                                (list PIPE-LR
                                      PIPE-TB
                                      PIPE-TBLR
                                      PIPE-TR
                                      PIPE-BL
                                      PIPE-TB
                                      PIPE-TL
                                      PIPE-BR
                                      PIPE-LR
                                      PIPE-TBLR
                                      PIPE-LR
                                      PIPE-TB
                                      PIPE-TL
                                      PIPE-LR
                                      PIPE-BR
                                      PIPE-LR
                                      PIPE-TBLR
                                      PIPE-BL
                                      PIPE-BR
                                      PIPE-TL
                                      PIPE-TR)
                                SGF 0 139))
(define (timer gs)
  (cond
    [(= 0 (game-state-time gs))
     (make-game-state
      (game-state-grid gs)
      (game-state-pipes gs)
      (grid-goo-propogate (game-state-goo gs) (game-state-grid gs))
      (game-state-replaced gs)
      28)]
    [(> (game-state-time gs) 0)
     (make-game-state
      (game-state-grid gs)
      (game-state-pipes gs)
      (game-state-goo gs)
      (game-state-replaced gs)
      (sub1 (game-state-time gs)))]))

    
(define GAME-1 (make-game-state STARTING-GRID
                                (list PIPE-LR
                                      PIPE-TB
                                      PIPE-TBLR
                                      PIPE-TR
                                      PIPE-BL
                                      PIPE-TB
                                      PIPE-TL
                                      PIPE-BR
                                      PIPE-LR
                                      PIPE-TBLR
                                      PIPE-LR
                                      PIPE-TB
                                      PIPE-TL
                                      PIPE-LR
                                      PIPE-BR
                                      PIPE-LR
                                      PIPE-TBLR
                                      PIPE-BL
                                      PIPE-BR
                                      PIPE-TL
                                      PIPE-TR)
                                SGF 0 140))
(define GAME-2 (make-game-state (make-grid 12 '())
                                (list PIPE-LR
                                      PIPE-TB
                                      PIPE-TBLR
                                      PIPE-TR
                                      PIPE-BL
                                      PIPE-TB
                                      PIPE-TL
                                      PIPE-BR
                                      PIPE-LR
                                      PIPE-TBLR
                                      PIPE-LR
                                      PIPE-TB
                                      PIPE-TL
                                      PIPE-LR
                                      PIPE-BR
                                      PIPE-LR
                                      PIPE-TBLR
                                      PIPE-BL
                                      PIPE-BR
                                      PIPE-TL
                                      PIPE-TR)
                                SGF 0 140))
(define EX-GAME (make-game-state
                 (make-grid 5 (list (make-pipe-coords PIPE-TL 1 3)
                                    (make-pipe-coords PIPE-LR 2 3)
                                    (make-pipe-coords PIPE-BR 4 3)
                                    (make-pipe-coords PIPE-TBLR 4 2)
                                    (make-pipe-coords PIPE-TR 4 1)
                                    (make-pipe-coords PIPE-TL 3 1)
                                    (make-pipe-coords PIPE-TB 3 2)
                                    (make-pipe-coords PIPE-TBLR 3 3)
                                    (make-pipe-coords PIPE-BL 3 4)
                                    (make-pipe-coords SPIPE-R 4 4)))
                 (list PIPE-BR PIPE-TB PIPE-LR PIPE-TBLR PIPE-TL)
                 (make-goo-flow (list (make-pipe-coords SPIPE-R 4 4))
                                (list "right"))
                 0
                 140))

;; pipe-fantasy: GameState -> GameState
(define (pipe-fantasy initial-game-state)
  (big-bang (gamestate-init initial-game-state)
    [to-draw render-grid]
    [on-mouse place-pipe-on-click]
    [on-tick timer]))
