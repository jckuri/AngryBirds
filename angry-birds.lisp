#!/opt/local/bin/sbcl --script

(load "~/LISP/packages/ltk-0.96/ltk.lisp")
(defpackage #:angry-birds (:use #:cl #:ltk))
(in-package #:angry-birds)

; class point

(defclass point () (
 (x :accessor x :initarg :x :initform 0)
 (y :accessor y :initarg :y :initform 0)))

(defmethod point-to-string ((point point))
 (format nil "(~d,~d)" (x point) (y point)))

(defmethod new-point ((x number) (y number))
 (make-instance 'point :x x :y y))

(defmethod sum ((p0 point) (p1 point))
 (new-point (+ (x p0) (x p1)) (+ (y p0) (y p1))))

(defmethod subtract ((p1 point) (p0 point))
 (new-point (- (x p1) (x p0)) (- (y p1) (y p0))))

(defmethod get-magnitude ((p point))
 (sqrt (+ (expt (x p) 2) (expt (y p) 2))))

(defmethod angle ((p point))
 (atan (y p) (x p)))

(defmethod multiply ((p point) (factor number))
 (new-point (* factor (x p)) (* factor (y p))))

(defmethod get-distance ((p1 point) (p0 point))
 (get-magnitude (subtract p1 p0)))

(defmethod polar-to-point ((magnitude number) (angle number))
 (multiply (new-point (cos angle) (sin angle)) magnitude))

; wish commands

(defun send-wish-and-read-data (tcl)
 (send-wish (format nil "senddatastrings [~a]" tcl))
 (ltk::read-data))

(defun format-wish-and-read-data (control &rest args)
 (send-wish-and-read-data (apply #'format nil control args)))

(defun package-require (name)
 (format-wish "package require ~a" name))

(defun move-image (canvas image x y)
 (format-wish "~a coords ~a ~d ~d" canvas image x y))

(defun change-image (canvas tag image)
 (format-wish "~a itemconfigure ~a -image ~a" canvas tag image))

(defun change-state (canvas tag state)
 (format-wish "~a itemconfigure ~a -state ~a" canvas tag state))

(defun create-image (image file)
 (format-wish "image create photo ~a -file \"~a\"" image file))

(defun create-canvas (canvas background)
 (format-wish "canvas ~a -height [image height ~a] -width [image width ~a]" canvas background background)
 (set-background-image canvas background))

(defun set-background-image (canvas background)
 (format-wish "~a create image 0 0 -anchor nw -image ~a" canvas background))

(defun pack (widget)
 (format-wish "pack ~a" widget))

(defun create-canvas-image (canvas image tag x y)
 (format-wish "~a create image ~d ~d -image ~a -tag ~a" canvas x y image tag))

(defun delete-all-objects-in-canvas (canvas)
 (format-wish "~a delete \"all\"" canvas))

(defun set-title (title)
 (format-wish "wm title . \"~a\"" title))

(defun top-most (window value)
 (format-wish "wm attributes ~a -topmost ~d" window value))

(defun focus (window)
 (format-wish "wm focusmodel ~a active" window)
 (format-wish "wm deiconify ~a" window)
 (format-wish "raise ~a" window)
 (format-wish "focus -force ~a" window))

(defun get-window-size (window)
 (let (width height)
  (update-ltk)
  (setq width (parse-integer (first (format-wish-and-read-data "winfo width ~a" window))))
  (setq height (parse-integer (first (format-wish-and-read-data "winfo height ~a" window))))
  `(,width ,height)))

(defun get-screen-size (window)
 (let (width height)
  (setq width (parse-integer (first (format-wish-and-read-data "winfo vrootwidth ~a" window))))
  (setq height (parse-integer (first (format-wish-and-read-data "winfo vrootheight ~a" window))))
  `(,width ,height)))

(defun set-window-geometry (window x y width height)
 (format-wish "wm geometry ~a ~dx~d+~d+~d" window width height x y))

(defun center-window (window)
 (let (size screen-size x y)
  (setq size (get-window-size window))
  (setq screen-size (get-screen-size window))
  (setq x (round (* (- (first screen-size) (first size)) 0.5)))
  (setq y (round (* (- (second screen-size) (second size)) 0.5)))
  (set-window-geometry window x y (first size) (second size))))

(defun update-ltk ()
 (send-wish "update"))

(defun setup-window (window title)
 (set-title title)
 (top-most window 1)
 (center-window window)
 (focus window))
            
(defun quit-ltk ()
 (send-wish "exit"))

; some macros

(defmacro push-end (item list)
 `(setf ,list (nconc ,list (list ,item))))
 
(defmacro += (variable text)
 `(setf ,variable (format nil "~a~a" ,variable ,text)))

; some constants

(defvar explosion-time 20)
(defvar contact-radius 20)
(defvar gravity 0.1)
(defvar initial-point (new-point 145 415))
(defvar ground 475)
(defvar maximum-magnitude 100)
(defvar throwing-factor 0.15)
(defvar dt 1)

; animated class

(defclass animated () (
  (point :accessor point :initarg :point)
  (kind :accessor kind :initarg :kind)
  (tag :accessor tag :initarg :tag)
  (counter :accessor counter :initarg :counter :initform 0)
  (status :accessor status :initarg :status :initform "normal")))

; game class

(defclass game () (
  (level :accessor level :initform 1)
  (bird :accessor bird)
  (entities :accessor entities :initform '())
  (canvas-name :accessor canvas-name :initform ".canvas")
  (gifs :accessor gifs)))

; animated methods

(defgeneric move-animated (animated game)
 (:documentation "Move animated object in a game."))

(defmethod add-to-game ((animated animated) (game game) (image-name string))
 (create-canvas-image (canvas-name game) image-name (tag animated) (x (point animated)) (y (point animated))))

(defmethod iterate-destruction-variables ((animated animated))
 (unless (equal (status animated) "destroyed")
  (when (> (counter animated) 0)
   (incf (counter animated) -1)
   (setf (status animated) "explosion")
   (when (= (counter animated) 0)
    (setf (status animated) "destroyed")))))

(defmethod repaint ((animated animated) (game game))
 (move-image (canvas-name game) (tag animated) (x (point animated)) (y (point animated)))
 (when (equal (status animated) "explosion")
  (change-image (canvas-name game) (tag animated) (tag (get-gif 'E))))
 (when (equal (status animated) "destroyed")
  (change-state (canvas-name game) (tag animated) "hidden")))

(defmethod is-touching ((object1 animated) (object2 animated))
 (<= (get-distance (point object1) (point object2)) contact-radius))

(defmethod explode ((object animated))
 (setf (counter object) explosion-time))

; class square

(defclass square (animated) (
  (indexes :accessor indexes)))

(defmethod new-square ((i integer) (j integer) (kind symbol))
 (let (square)
  (setq square (make-instance 'square))
  (setf (indexes square) (new-point i j))
  (setf (point square) (new-point (+ (* i 30) 18) (+ (* j 30) 18)))
  (setf (tag square) (format nil "square~d,~d" i j))
  (setf (kind square) kind)
  square))

(defmethod move-animated ((object square) (game game))
 (when 
  (and
   (equal (status object) "normal") 
   (equal (status (bird game)) "normal")
   (is-touching object (bird game)))
  (when (eq (kind object) 'R) (explode (bird game)))
  (when (eq (kind object) 'W)
   (explode (bird game))
   (explode object))
  (when (or (eq (kind object) 'G) (eq (kind object) 'P))
   (explode object)))
 (iterate-destruction-variables object))

(defmethod repaint ((square square) (game game))
 (when (equal (status square) "explosion")
  (change-image (canvas-name game) (tag square) (tag (get-gif 'E))))
 (when (equal (status square) "destroyed")
  (change-state (canvas-name game) (tag square) "hidden")))

; bird class

(defclass bird (animated) (
  (velocity :accessor velocity)
  (acceleration :accessor acceleration)
  (mouse-offset :accessor mouse-offset)
  (last-throw :accessor last-throw)))

(defmethod add-new-bird ((game game))
 (let (bird)
  (setq bird (make-instance 'bird))
  (setf (point bird) initial-point)
  (setf (velocity bird) (new-point 0 0))
  (setf (acceleration bird) (new-point 0 0))
  (setf (tag bird) "bird")
  (setf (status bird) "static")
  (setf (kind bird) 'B)
  (add-to-game bird game "bird")
  (push-end bird (entities game))
  (setf (bird game) bird)))

(defmethod move-animated ((bird bird) (game game))
 (when (equal (status bird) "normal")
  (setf (point bird) (sum (point bird) (multiply (velocity bird) 1)))
  (setf (velocity bird) (sum (velocity bird) (multiply (acceleration bird) 1)))
  (when (and (> (y (point bird)) ground) (> (y (velocity bird)) 0))
   (explode bird)))
 (when (equal (status bird) "destroyed")
  (reset-bird bird))
 (iterate-destruction-variables bird))

(defmethod repaint ((bird bird) (game game))
 (move-image (canvas-name game) (tag bird) (x (point bird)) (y (point bird)))
 (change-image (canvas-name game) (tag bird) 
  (tag (get-gif (if (equal (status bird) "explosion") 'E 'B))))
 (change-state (canvas-name game) (tag bird) 
  (if (equal (status bird) "destroyed") "hidden" "normal")))

(defmethod reset-bird ((bird bird))
 (setf (point bird) initial-point)
 (setf (velocity bird) (new-point 0 0))
 (setf (acceleration bird) (new-point 0 0))
 (setf (mouse-offset bird) nil)
 (setf (status bird) "static"))

(defmethod touches-mouse ((bird bird) (mouse point))
 (<= (get-distance (point bird) mouse) (* contact-radius 0.5)))

(defmethod set-mouse-offset ((bird bird) (mouse point))
 (setf (mouse-offset bird) (subtract mouse (point bird))))
   
; level class

(defclass level () (
  (squares :accessor squares :initform '())))

(defmethod new-level ((n integer))
 (let (level)
  (setq level (make-instance 'level))
  (read-level level (get-file-name n))
  level))

(defmethod get-square ((level level) (i integer) (j integer) (letter symbol))
 (if (find letter '(P G R W))
  (new-square i j letter)
  nil))

(defun char-to-symbol (c)
 (intern (string c)))

(defmethod process-line ((level level) (line string) (j integer))
 (let (n square)
  (setq n (min (length line) 34))
  (loop for i from 0 to (1- n) do
   (setq square (get-square level i j (char-to-symbol (char line i))))
   (when square
    (push-end square (squares level))))))

(defmethod read-level ((level level) (filename string))
 (let (j)
  (setq j 0)
  (with-open-file (stream filename)
   (do 
    ((line (read-line stream nil) (read-line stream nil)))
    ((null line))
    (process-line level line j)
    (incf j 1)))))
  
(defmethod get-level-string ((n integer))
 (let (s l)
  (setq s (write-to-string n))
  (setq l (length s))
  (when (= l 1) (setq s (format nil "00~a" s)))
  (when (= l 2) (setq s (format nil "0~a" s)))
  s))

(defmethod get-file-name ((n integer))
 (format nil "level-~a.txt" (get-level-string n)))

; gif class

(defclass gif () (
  (file :accessor file :initarg :file)
  (tag :accessor tag :initarg :tag)))

(defmethod new-gif ((file string) (tag string))
 (make-instance 'gif :file file :tag tag))

(defmethod get-gifs ()
 `(,(new-gif "angry-bird.gif" "bird")
  ,(new-gif "pig.gif" "pig")
  ,(new-gif "glass.gif" "glass")
  ,(new-gif "rock.gif" "rock")
  ,(new-gif "wood.gif" "wood")
  ,(new-gif "explosion.gif" "explosion")))

(defun get-gif-index (letter)
 (case letter
  (B 0) (P 1) (G 2) (R 3) (W 4) (E 5)))

(defmethod initialize-game ((game game))
 (load-level 1))

(defmethod new-game ()
 (let (game)
  (setq game (make-instance 'game))
  (setf (gifs game) (get-gifs))
  game))

(defvar game (new-game))

(defmethod get-new-point ((bird bird) (mouse point))
 (if (null (mouse-offset bird))
  nil
  (let (new-point difference magnitude)
   (setq new-point (subtract mouse (mouse-offset bird)))
   (setq difference (subtract initial-point new-point))
   (setq magnitude (get-magnitude difference))
   (when (> magnitude maximum-magnitude)
    (setq new-point (subtract initial-point (polar-to-point maximum-magnitude (angle difference)))))
   new-point)))

(defmethod move-bird-with-mouse ((bird bird) (mouse point))
 (let ((new-point (get-new-point bird mouse)))
  (when new-point (setf (point bird) new-point))))
 
(defmethod throw-bird ((bird bird) (mouse point))
 (let (new-point difference)
  (setq new-point (get-new-point bird mouse))
  (when new-point
   (setq difference (subtract initial-point new-point))
   (setf (status bird) "normal")
   (setf (velocity bird) (multiply difference throwing-factor))
   (setf (acceleration bird) (new-point 0 gravity))
   (setf (last-throw bird) new-point))))

(defun get-gif (letter)
 (nth (get-gif-index letter) (gifs game)))

(defmethod create-images ()
 (loop for gif in (gifs game) do
  (create-image (tag gif) (file gif)))
 (create-image "background" "background.gif"))

(defmethod move-entities ()
 (loop for entity in (entities game) do
  (move-animated entity game))
 (unless (is-there-a-pig-alive)
  (incf (level game))
  (load-level (level game))))

(defmethod repaint-entities ()
 (loop for entity in (entities game) do
  (repaint entity game)))

(defun iteration ()
 (move-entities)
 (repaint-entities)
 (after 10 #'iteration))

(defun start-after (ms)
 (after ms #'iteration))

(defun get-mouse-point (event)
 (new-point (event-x event) (event-y event)))

(defun button-press-1 (event)
 (let (mouse)
  (setq mouse (get-mouse-point event))
  (setf (mouse-offset (bird game)) nil)
  (when (touches-mouse (bird game) mouse)
   (set-mouse-offset (bird game) mouse))))

(defun mouse-motion-b1 (event)
 (move-bird-with-mouse (bird game) (get-mouse-point event)))

(defun button-release-1 (event)
 (throw-bird (bird game) (get-mouse-point event)))
  
(defmethod get-image-name ((letter symbol))
 (tag (get-gif letter)))

(defmethod load-level ((n integer))
 (let (level)
  (delete-all-objects-in-canvas (canvas-name game))
  (set-background-image ".canvas" "background")
  (setf (entities game) '())
  (add-new-bird game)
  (setq level (new-level n))
  (loop for square in (squares level) do
   (add-to-game square game (get-image-name (kind square)))
   (push-end square (entities game)))))
  
(defmethod is-there-a-pig-alive ()
 (let ((alive-pig nil))
  (loop for entity in (entities game) do
   (when (and (eq (kind entity) 'P) (not (equal (status entity) "destroyed")))
    (setq alive-pig t)))
   alive-pig))

; ltk constructions

(with-ltk ()
 (package-require "Img")
 (create-images)
 (create-canvas ".canvas" "background")
 (pack ".canvas")
 (start-after 1000)
 (bind ".canvas" "<ButtonPress-1>" #'button-press-1)
 (bind ".canvas" "<B1-Motion>" #'mouse-motion-b1)
 (bind ".canvas" "<ButtonRelease-1>" #'button-release-1)
 (setup-window "." "Angry Birds")
 (initialize-game game))
