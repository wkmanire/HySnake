;;;; snake.hy
(import [sys [exit stderr]])
(import [curses [wrapper curs-set napms error flushinp]])
(import [random [randint]])
(require [hy.contrib.loop [loop]])
(require [hy.contrib.walk [let]])

(setv +config+ {
                ;; Change these to larger numbers if you want the game
                ;; to take longer to complete.
                "max-screen-height" 24
                "max-screen-width" 40

                ;; If the screen is too small, the game will just quit
                "min-screen-height" 20
                "min-screen-width" 30
                })


(defclass Game []
  "Contains the state of the game"
  (defn --init-- [self]
    (setv self.game-over? False)
    (setv self.snake-parts [[3 3] [2 3]])
    (setv self.snake-direction 'down)
    (setv self.food-pos [5 5]))

  (defn get-snake-parts [self]
    self.snake-parts)

  (defn get-food-coords [self]
    self.food-pos)

  (defn set-food-coords! [self coords]
    (setv self.food-pos coords))

  (defn get-snake-direction [self]
    self.snake-direction)

  (defn game-is-over? [self]
    self.game-over?)

  (defn set-game-is-over! [self value]
    (setv self.game-over? value))

  (defn set-snake-direction! [self new-direction]
    (setv self.snake-direction new-direction)))

(defn is-opposite-direction? [a b]
  (let [opposites {'left 'right 'right 'left 'up 'down 'down 'up}]
    (= b (get opposites a))))

(defn clamp [value minimum maximum]
  "Return a value which is no less than MINIMUM and no more than
  MAXIMUM. If VALUE is between minimum and maximum (inclusive) then
  value is returned."
  (max (min value maximum) minimum))

(defn render-snake [win game is-head?]
  (setv snake-parts (.get-snake-parts game))
  (setv head-direction (.get-snake-direction game))
  (setv head-char (cond [(= head-direction 'up) "^"]
                        [(= head-direction 'down) "V"]
                        [(= head-direction 'left) "<"]
                        [(= head-direction 'right) ">"]))
  (loop [[segment (first snake-parts)]
         [tail (rest snake-parts)]
         [is-head? is-head?]]
        (when (not (none? segment))
          (let [y (get segment 0)
                x (get segment 1)]
            (if is-head?
                (.addch win y x head-char)
                (.addch win y x "O")))
          (recur (first tail) tail False))))

(defn render-food [win food]
  (let [y (get food 0)
        x (get food 1)]
    (.addch win y x "*")))

(defn get-next-head [game]
  (let [direction (.get-snake-direction game)
        head (first (.get-snake-parts game))]
    (let [y (get head 0)
          x (get head 1)]
      (cond [(= 'left  direction) [     y (dec x)]]
            [(= 'right direction) [     y (inc x)]]
            [(= 'up    direction) [(dec y)     x]]
            [(= 'down  direction) [(inc y)     x]]))))

(defn get-random-inbound-coords [win]
  (let [begyx (.getbegyx win)
        maxyx (.getmaxyx win)]
    (let [min-y (inc (first begyx))
          min-x (inc (last begyx))
          max-y (dec (first maxyx))
          max-x (dec (last maxyx))]
      [(randint (inc min-y) (dec max-y))
       (randint (inc min-x) (dec max-x))])))

(defn move-snake! [game]
  (let [parts (.get-snake-parts game)]
    (let [head (get parts 0)]
      (let [y (get head 0)
            x (get head 1)]
        (.insert parts 0 (get-next-head game))
        (.pop parts)))))

(defn grow-snake! [game amount]
  (let [parts (.get-snake-parts game)
        tail (last (.get-snake-parts game))]
    (loop [(i amount)]
          (when (>= i 0)
            (.append parts (list tail))
            (recur (dec i))))))

(defn set-snake-direction-if-not-opposite! [game new-direction]
  (let [current-direction (.get-snake-direction game)]
    (when (not (is-opposite-direction? current-direction new-direction))
      (.set-snake-direction! game new-direction))))

(defn handle-terminal-resize! [game win]
  (assert-screen-large-enough win))

(defn handle-input! [game win]
  (try
    (let [key (.getkey win)]
      (cond [(= key "KEY_RESIZE")    (handle-terminal-resize! game win)]
            [(= key "KEY_LEFT")  (set-snake-direction-if-not-opposite! game 'left)]
            [(= key "KEY_RIGHT") (set-snake-direction-if-not-opposite! game 'right)]
            [(= key "KEY_UP")    (set-snake-direction-if-not-opposite! game 'up)]
            [(= key "KEY_DOWN")  (set-snake-direction-if-not-opposite! game 'down)]
            [(= key "q")         (.set-game-is-over! game True)]))
    (except [error]) ;; no input
    (except [Exception] (raise))))

(defn move-food! [game win]
  (loop []
    (.set-food-coords! game (get-random-inbound-coords win))
    (when (in (.get-food-coords game) (.get-snake-parts game))
      (recur))))

(defn handle-collisions! [game win]
  "Handle if the snake has touched food or is biting itself."
  (let [head (first (.get-snake-parts game))]
    (if (= head (.get-food-coords game))  ;; Did we get food?
        (do
          (move-food! game win)
          (grow-snake! game 3))
        (when (in head (rest (.get-snake-parts game)))  ;; Did we bite ourselves?
          (.set-game-is-over! game True)))))

(defn wrap-snake! [game win]
  "Wrap the snake around the play field"
  (let [parts (.get-snake-parts game)]
    (let [height (- (first (.getmaxyx win)) 1)
          width (- (last (.getmaxyx win)) 1)
          y (first (first parts))
          x (last (first parts))]
      (setv new-coords [y x])
      (when (= y 0)
        (setv (get new-coords 0) (dec height)))
      (when (= y height)
        (setv (get new-coords 0) 1))
      (when (= x 0)
        (setv (get new-coords 1) (dec width)))
      (when (= x width)
        (setv (get new-coords 1) 1))
      (setv (get parts 0) new-coords))))

(defn assert-screen-large-enough [stdscr]
  (let [maxyx (.getmaxyx stdscr)]
    (let [height (first maxyx)
          width (last maxyx)]
      (when (or (< width (get +config+ "min-screen-width"))
                (< height (get +config+ "min-screen-height")))
        (.write stderr (.format "The terminal window is too small to play the game. A minimum size of {0}x{1} is required"
                                    (get +config+ "min-screen-width") (get +config+ "min-screen-height")))
        (exit 1)))))

(defn snake-loop [stdscr]
  "Main loop of the game"
  (assert-screen-large-enough stdscr)  ;; Will exit if the screen is too small
  (curs-set 0)

  ;; Create the two windows here

  (setv num-lines (first (.getmaxyx stdscr)))
  (setv num-columns (last (.getmaxyx stdscr)))

  (setv info-bar-win (.subwin
                       stdscr 3
                       (clamp num-columns
                              (get +config+ "min-screen-width")
                              (get +config+ "max-screen-width"))
                       0 0))
  (.keypad info-bar-win True)
  (.nodelay info-bar-win True)
  (.timeout info-bar-win 0)

  (setv play-area-win (.subwin
                        stdscr
                        (clamp (- num-lines 3)
                               (get +config+ "min-screen-height")
                               (get +config+ "max-screen-height"))
                        (clamp num-columns
                              (get +config+ "min-screen-width")
                              (get +config+ "max-screen-width"))
                        3 0))
  (.keypad play-area-win True)
  (.nodelay play-area-win True)
  (.timeout play-area-win 0)

  (setv g (Game))
  (move-food! g play-area-win)
  (loop []
        (when (not (.game-is-over? g))
          (handle-input! g play-area-win)
          (move-snake! g)
          (wrap-snake! g play-area-win)          
          (handle-collisions! g play-area-win)

          (.clear play-area-win)
          (.border play-area-win)
          (render-snake play-area-win g True)
          (render-food play-area-win (.get-food-coords g))
          (.refresh play-area-win)

          (.clear info-bar-win)
          (.border info-bar-win)
          (.addstr info-bar-win 1 1 (.format "Score: {0}" (len (.get-snake-parts g))))
          (.refresh info-bar-win)

          (napms 100)
          (recur))))

(defn run-snake []
  "Initialize the UI and start the game's main loop"
  (wrapper snake-loop))
