;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 0.3)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 0.3)
(define INVADER-DY 10)           ;velocity of invader in y
(define TANK-SPEED 7)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT (image-height TANK))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))
(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
(define I4 (make-invader 200 200 -10))          ;another invader


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1)) 
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game (list I1 I4) (list M1 M2) T1))


;; ============================
;; Additional Data Definitions

;; ListOfInvader is one of:
;; - empty
;; - ListOfInvader
;; interp. a list of Invaders

(define LOI1 empty)
(define LOI2 (list I1 I2 I3))

#;
(define (fn-for-loinvader loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))

;; Template Rules Used:
;; - one-of:
;; - atomic-distinct: empty
;; - compound: (cons Invader ListOfInvader)
;; - reference: (first loi) is Invader
;; - self-reference: (rest loi) is ListOfInvader

;; ListOfMissile is one of:
;; - empty
;; - ListOfMissile
;; interp. a list of Missile

(define LOM1 empty)
(define LOM2 (list M1 M2 M3))

#;
(define (fn-for-missile lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template Rules Used:
;; - one-of:
;; - atomic-distinct: empty
;; - compound: (cons Missile ListOfMissile)
;; - reference: (first loi) is Missile
;; - self-reference: (rest loi) is ListOfMissile



;; ======================================
;; Functions:

;; Game -> Game
;; start Invaders Game by evaluating (main G0)

(define (main game)
  (big-bang game                 ; Game
    (on-tick   next-game)     ; Game -> Game
    (to-draw   render-game)   ; Game -> Image
    (stop-when end-game)      ; Game -> Boolean
    (on-key    game-input)))    ; Game KeyEvent -> Game


;; Game -> Game 
;; produce next game state. the next list of invaders, list of missles, and tank
(check-random (next-game G0) (make-game empty
                                        empty
                                        (game-tank G0))) ;start state adds first invader
(check-random (next-game G2) (make-game (list (make-invader
                                               (+ (invader-x I1) (* INVADER-X-SPEED (invader-dx I1)))
                                               (+ (invader-y I1) (* INVADER-Y-SPEED INVADER-DY))
                                               (invader-dx I1)))
                                        (list (make-missile
                                               (missile-x M1)
                                               (- (missile-y M1) MISSILE-SPEED)))
                                        (game-tank G2))) ;move one invader, one new invader and one missile 

(check-random (next-game G4) (make-game (list (make-invader (+ (invader-x I4) (* INVADER-X-SPEED (invader-dx I4)))
                                                            (+ (invader-y I4) (* INVADER-Y-SPEED INVADER-DY))
                                                            (invader-dx I4)))
                                        (list (make-missile
                                               (missile-x M1)
                                               (- (missile-y M1) MISSILE-SPEED)))
                                        (game-tank G4))) ;some invaders and some missiles. missile hit invader 



;(define (next-game g) (make-game empty empty T0)) ;stub

;<template from Game>

(define (next-game s)
  (make-game (update-invaders (alive-invaders (game-invaders s) (game-missiles s)))
             (update-missiles (active-missiles (game-invaders s) (game-missiles s)))
             (game-tank s)))


;; ListOfInvaders ListOfMissiles - > ListOfInvaders
;; Removes any Invaders that have been hit by a missile
(check-expect (alive-invaders empty empty) empty)
(check-expect (alive-invaders (list I1 I4) (list M1 M2)) (list I4))

;(define (alive-invaders loi lom) loi) ;stub

;<template from ListOfInvaders>
(define (alive-invaders loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (missile-hit lom (first loi))                ;if invader hit by any missile return remaining list otherwise whole list
             (alive-invaders (rest loi) lom)
             (cons (first loi) (alive-invaders (rest loi) lom)))]))

;; ListOfMissiles Invader -> Boolean
;; Returns true if the invader has been hit by any of the missiles
(check-expect (missile-hit empty (make-invader 150 150 10)) false)
(check-expect (missile-hit (list (make-missile 50 200)
                                 (make-missile 50 100)) (make-invader 200 100 10)) false)
(check-expect (missile-hit (list (make-missile 50 200)
                                 (make-missile 200 100)) (make-invader 200 100 10)) true)
(check-expect (missile-hit (list (make-missile 50 200)
                                 (make-missile 200 100)) (make-invader 200 90 10)) true)

;(define (missile-hit lom i) false) ;stub
;<template from ListOfMissiles with additional compound input>
(define (missile-hit lom i)
  (cond [(empty? lom) false]
        [else
         (if (is-hit? (first lom) i)
             true
             (missile-hit (rest lom) i))]))






;; Missile Invader -> Boolean
;; Compares an invader and missile. returns true if missile and invader are in same hit radius box
(check-expect (is-hit? (make-missile 150 100) (make-invader 100 150 10)) false)
(check-expect (is-hit? (make-missile 100 95) (make-invader 100 (- 100 (/ HIT-RANGE 2)) 10)) true) ;hit top of the box

(check-expect (is-hit? (make-missile 100 105) (make-invader 100 (+ 100 (/ HIT-RANGE 2)) 10)) true)  ;hit bottom of the box
(check-expect (is-hit? (make-missile 95 100) (make-invader (- 100 (/ HIT-RANGE 2)) 100 10)) true) ;hit left of the box
(check-expect (is-hit? (make-missile 105 100) (make-invader (+ 100 (/ HIT-RANGE 2)) 100 10)) true) ;hit right of the box

;(define (is-hit? m i) false) ;stub

;<template from Missile with additional compound parameter invader>
(define (is-hit? m i)
  (if (and (and (>= (missile-x m) (- (invader-x i) HIT-RANGE))   ;left of hit box
                (<= (missile-x m) (+ (invader-x i) HIT-RANGE)))  ;right of hit box
           
           (and (>= (missile-y m) (- (invader-y i) HIT-RANGE))   ;top of hit box
                (<= (missile-y m) (+ (invader-y i) HIT-RANGE)))) ;bottom of hit box
      true
      false)) 
;missile-x > left bounday of hit box
; missile-x < right boundary of hit box


;; ListOfInvaders ListOfMissiles - > ListOfMissiles
;; Removes any Missiles that have destroyed an Invader
(check-expect (active-missiles empty empty) empty)
(check-expect (active-missiles (list I1 I4) (list M1 M2)) (list M1))

;(define (active-missiles loi lom) lom) ;stub

(define (active-missiles loi lom)
  (cond [(empty? lom) empty]
        [else
         (if (missile-detonate loi (first lom))
             (active-missiles loi (rest lom))
             (cons (first lom) (active-missiles loi (rest lom))))]))



;; ListOfInvaders Missile -> Boolean
;; Returns true if the invader has been hit by any of the missiles
(check-expect (missile-detonate empty (make-missile 150 150)) false)
(check-expect (missile-detonate (list (make-invader 75 200 10)
                                      (make-invader 50 100 10)) (make-missile 200 100)) false)
(check-expect (missile-detonate (list (make-invader 75 200 10)
                                      (make-invader 50 100 10)) (make-missile 75 205)) true)

;(define (missile-hit lom i) false) ;stub
;<template from ListOfMissiles with additional compound input>
(define (missile-detonate loi m)
  (cond [(empty? loi) (if (<= (missile-y m) 5)
                          true
                          false)]
        [else
         (if (is-hit? m (first loi))
             true
             (missile-detonate (rest loi) m))]))

;; Natural -> ListOfInvader
;; creates an invader if random number generated by n is less than INVADE-RATE
(check-random (create-invader 1000) (if (< (random 1000) INVADE-RATE)
                                        (list (make-invader (random WIDTH) 0 10))
                                        empty))

;(define (create-invader) empty) ;stub
;<template from invader>
(define (create-invader n)
  (if (< (random n) INVADE-RATE)                    ;number of invaders increases with the invade rate
      (list (make-invader (random WIDTH) 0 10))
      empty))
              
;; ListOfInvader -> ListOfInvader
;; updates the list of invaders for the next clock tick

(check-random (update-invaders empty) (if (< (random 1000) INVADE-RATE)
                                          (list (make-invader (/ WIDTH 2) 0 10))
                                          empty))
(check-random (update-invaders (list I1 I2)) (list (make-invader (+ (invader-x I1) (* INVADER-X-SPEED (invader-dx I1)))
                                                                 (+ (invader-y I1) (* INVADER-Y-SPEED INVADER-DY))
                                                                 (invader-dx I1))
                                                   (make-invader (+ (invader-x I2) (* INVADER-X-SPEED (invader-dx I2)))
                                                                 (+ (invader-y I2) (* INVADER-Y-SPEED INVADER-DY))
                                                                 (invader-dx I2))))  ;update invaders

;(define (update-invaders loi) loi) ;stub

;<template from ListOfInvaders>
(define (update-invaders loi)
  (cond [(empty? loi) (create-invader 5000)]
        [else
         (if (touch-side (first loi))
             (cons (make-invader (+ (invader-x (first loi)) (* INVADER-X-SPEED (- (invader-dx (first loi)))))
                                 (+ (invader-y (first loi)) (* INVADER-Y-SPEED INVADER-DY))
                                 (- (invader-dx (first loi))))
                   (update-invaders (rest loi)))
             (cons (make-invader (+ (invader-x (first loi)) (* INVADER-X-SPEED (invader-dx (first loi))))
                                 (+ (invader-y (first loi)) (* INVADER-Y-SPEED INVADER-DY))
                                 (invader-dx (first loi)))
                   (update-invaders (rest loi))))]))





;; Invader -> Boolean
;; returns True if an invader will touch the left or right boundary of the game area on next position update
(check-expect (touch-side (make-invader 150 100 10)) false)  ;invader in middle of screen
(check-expect (touch-side (make-invader (- WIDTH (/ HIT-RANGE 2)) 150 10)) true)  ;invader on right edge
(check-expect (touch-side (make-invader (+ 0 (/ HIT-RANGE 2)) 175 -10)) true)  ;invader on left edge

;(define (touch-side i) false) ;stub

;<template from Invader>

(define (touch-side i)
  (cond [(>= (invader-x i) (- WIDTH (/ HIT-RANGE 2))) true]
        [(<= (invader-x i) (/ HIT-RANGE 2)) true]
        [else false]))

;;; Invader -> Invader
;;; changes the direction of an invader when encountering a left or right boundary
;(check-expect (change-direction I1) (make-invader (invader-x I1)
;                                                  (invader-y I1)
;                                                  (- (invader-dx I1))))
;
;
;;(define (change-direction i) i) ;stub
;
;;<template from Invader>
;(define (change-direction i)
;  (make-invader (invader-x i) (invader-y i) (- (invader-dx i))))


;; ListOfMissile -> ListOfMissile
;; updates the list of missiles for the next clock tick
(check-expect (update-missiles empty) empty) ;base case
(check-expect (update-missiles (list M1)) (list (make-missile
                                                 (missile-x M1)
                                                 (- (missile-y M1) MISSILE-SPEED))))
(check-expect (update-missiles (list M1 M2)) (list (make-missile
                                                    (missile-x M1)
                                                    (- (missile-y M1) MISSILE-SPEED))
                                                   (make-missile
                                                    (missile-x M2)
                                                    (- (missile-y M2) MISSILE-SPEED))))

;(define (update-missiles lom) lom) ;stub

;<template from ListOfMissile>

(define (update-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (make-missile
                (missile-x (first lom))
                (- (missile-y (first lom)) MISSILE-SPEED))
               (update-missiles (rest lom)))]))



;; Game -> Image
;; render game images onto BACKGROUND
;; !!!
(check-expect (render-game G0) (underlay/xy BACKGROUND (- (tank-x (game-tank G0)) TANK-WIDTH/2) (- HEIGHT TANK-HEIGHT) TANK))    
(check-expect (render-game G1) (underlay/xy BACKGROUND (- (tank-x (game-tank G1)) TANK-WIDTH/2) (- HEIGHT TANK-HEIGHT) TANK))
(check-expect (render-game G2) (underlay/xy ;missile
                                (underlay/xy  ;invader
                                 (underlay/xy BACKGROUND (- (tank-x (game-tank G2)) TANK-WIDTH/2) (- HEIGHT TANK-HEIGHT) TANK) ;tank
                                 (invader-x (first (game-invaders G2)))
                                 (invader-y (first (game-invaders G2)))
                                 INVADER)
                                (missile-x (first (game-missiles G2)))
                                (missile-y (first (game-missiles G2)))
                                MISSILE))
(check-expect (render-game G4) (underlay/xy ;missile2
                                (underlay/xy ;missile1
                                 (underlay/xy ;invader2
                                  (underlay/xy  ;invader 1
                                   (underlay/xy BACKGROUND (- (tank-x (game-tank G4)) TANK-WIDTH/2) (- HEIGHT TANK-HEIGHT) TANK) ;tank
                                   (invader-x (first (game-invaders G4)))
                                   (invader-y (first (game-invaders G4)))
                                   INVADER)
                                  (invader-x (second (game-invaders G4)))
                                  (invader-y (second (game-invaders G4)))
                                  INVADER)
                                 (missile-x (first (game-missiles G4)))
                                 (missile-y (first (game-missiles G4)))
                                 MISSILE)
                                (missile-x (second (game-missiles G4)))
                                (missile-y (second (game-missiles G4)))
                                MISSILE))

;(define (render-game g) BACKGROUND) ;stub
;<template from game, with function composition>
(define (render-game s)
  (missile-img (game-missiles s) (invader-img (game-invaders s) (tank-img (game-tank s)))))

;; Tank -> Image
;; produces image of tank based on position
(check-expect (tank-img T0) (underlay/xy BACKGROUND
                                         (- (tank-x T0) TANK-WIDTH/2)
                                         (- HEIGHT TANK-HEIGHT)
                                         TANK))

;(define (tank-img t) BACKGROUND) ;stub
;<template from tank>
(define (tank-img t)
  (underlay/xy BACKGROUND
               (- (tank-x t) TANK-WIDTH/2)
               (- HEIGHT TANK-HEIGHT)
               TANK))


;;ListOfInvaders Image -> Image
;; produces image of all invaders on top of the passed image
(check-expect (invader-img empty (tank-img T0)) (tank-img T0))
(check-expect (invader-img (list I1 I4) (tank-img T0)) (underlay/xy 
                                                        (underlay/xy (tank-img T0)
                                                                     (invader-x I1)
                                                                     (invader-y I1)
                                                                     INVADER)
                                                        (invader-x I4)
                                                        (invader-y I4)
                                                        INVADER))

;(define (invader-img loi img) BACKGROUND) ;stub
;<template from ListOfInvaders with additional image paramerter>
(define (invader-img loi img)
  (cond [(empty? loi) img]
        [else
         (underlay/xy (invader-img (rest loi) img)
                      (invader-x (first loi))
                      (invader-y (first loi))
                      INVADER)]))


;; ListOfMissiles Image -> Image
;; produces image of all missules on top of the passed image
(check-expect (missile-img (list M1 M2) (invader-img (list I1 I4) (tank-img T0))) (underlay/xy
                                                                                   (underlay/xy (invader-img (list I1 I4) (tank-img T0))
                                                                                                (missile-x M1)
                                                                                                (missile-y M1)
                                                                                                MISSILE)
                                                                                   (missile-x M2)
                                                                                   (missile-y M2)
                                                                                   MISSILE))

;(define (missile-img lom img) BACKGROUND) ;stub
;<template from ListOfMissiles with additional image paramerter>
(define (missile-img lom img)
  (cond [(empty? lom) img]
        [else
         (underlay/xy (missile-img (rest lom) img)
                      (missile-x (first lom))
                      (missile-y (first lom))
                      MISSILE)]))


#;
;(define G2 (make-game (list I1) (list M1) T1))
;(define G3 (make-game (list I1 I2) (list M1 M2) T1))
;(define G4 (make-game (list I1 I4) (list M1 M2) T1))


;; Game -> Boolean
;; Stop the game when invader reaches HEIGHT

(check-expect (end-game G0) false) ;game inital state
(check-expect (end-game G2) false) ;invader on screen not landed
(check-expect (end-game G3) true)  ;invader landed. game end

;(define (end-game g) false) ;stub

;<teamplate from Game>
(define (end-game s)
  (if (>= (max-invader (game-invaders s)) HEIGHT)
      true
      false))

;; ListOfInvader -> Natural
;; Returns the invader with the highest Height

(check-expect (max-invader empty) 0)
(check-expect (max-invader (list I1)) (invader-y I1))
(check-expect (max-invader (list I1 I2)) (invader-y I2))
(check-expect (max-invader (list I1 I2 I3)) (invader-y I3))

;(define (max-invader loi) 0) ;stub

;<template from ListOfInvader with reference to invader

(define (max-invader loi)
  (cond [(empty? loi) 0]
        [else
         (if (> (invader-y (first loi)) (max-invader (rest loi)))
             (invader-y (first loi))
             (max-invader (rest loi)))]))


;; Game KeyEvent -> Game
;; Move the Tank with left and right arrow and shoot Blasters with spacebar

 
(check-expect (game-input G0 "n") (make-game empty
                                             empty
                                             T0)) ;any key other than "space" or "left/right"

(check-expect (game-input G0 " ") (make-game empty
                                             (list (make-missile (tank-x T0) HEIGHT))
                                             T0)) ;blaster shot key

(check-expect (game-input G1 "left") (make-game empty
                                                empty
                                                (make-tank (+ 50 (* TANK-SPEED -1)) -1))) ;tank going right, change to left
              
(check-expect (game-input G1 "right") (make-game empty
                                                 empty
                                                 (make-tank (+ 50 (* TANK-SPEED 1)) 1))) ;tank going right, continue right

;(define (game-input g k) g) ;stub

(define (game-input g k)
  (cond [(key=? k " ") (make-game (game-invaders g) (add-missile (game-missiles g) (game-tank g)) (game-tank g))]
        [(key=? k "left") (make-game (game-invaders g) (game-missiles g) (move-tank (game-tank g) -1))]
        [(key=? k "right") (make-game (game-invaders g) (game-missiles g) (move-tank (game-tank g) 1))]
        [else g]))


;; ListOfMissile Tank -> ListOfMissile
;; Add a missile to the game

(check-expect (add-missile empty T0) (list (make-missile (tank-x T0) HEIGHT)))
(check-expect (add-missile (list M1 M2 M3) T0) (list (make-missile (tank-x T0) HEIGHT) M1 M2 M3))

;(define (add-missile lom t) lom) ;stub

;<template from ListOfMissile with additional compound paramter Tank>

(define (add-missile lom t)
  (cond [(empty? lom) (list (make-missile (tank-x t) HEIGHT))]
        [else
         (cons (make-missile (tank-x t) HEIGHT) lom)]))


;; Tank Natural[-1, 1] -> Tank
;; Moves the tank left or right by TANK-SPEED

(check-expect (move-tank T0 1) (make-tank (+ (tank-x T0) (* TANK-SPEED 1)) 1)) ;move tank to right
(check-expect (move-tank T0 -1) (make-tank (+ (tank-x T0) (* TANK-SPEED -1)) -1)) ;move tank to left

;(define (move-tank g n) g) ;stub

;<template from Tank with additional atomic non-distinct parameter>

(define (move-tank t n)
  (make-tank (+ (tank-x t) (* TANK-SPEED n)) n))