(in-package #:cl-user)

(progn
  (asdf:load-systems '#:vecto . #1=('#:alexandria '#:xarray '#:fiveam '#:anaphora))
  (use-package (list . #1#)))

(shadowing-import '(alexandria:rotate))
(use-package '#:vecto)

(defvar cube
  #3A(("wia" ;; top/0
       "uir"
       "tee")
      ("ntm" ;; front/1
       "reu"
       "oqs")
      ("thc" ;; right/2
       "keb"
       "sen")
      ("asu" ;; back/3
       "eik"
       "sau")
      ("bhc" ;; left/4
       "rbb"
       "dnh")
      ("ets" ;; bottom/5
       "fuc"
       "rod")))

(defvar colours
  (let ((copy (copy-array cube)))
    (loop
      for i from 0
      for colour in '(white red blue orange green yellow)
      do (xsetf (slice copy i :all :all) (make-array '(3 3) :initial-element colour)))
    copy))

;; layout assumed as on wikipedia, white up, red front, blue right

(defun single-rotate-side (copy cube side)
  (xsetf (slice copy side 0 :all) (slice cube side '(2 0) 0))
  (xsetf (slice copy side :all 0) (slice cube side 2 :all))
  (xsetf (slice copy side 2 :all) (slice cube side '(2 0) 2))
  (xsetf (slice copy side :all 2) (slice cube side 0 :all)))

(defun single-rotate-cube (cube code)
  (let ((copy (copy-array cube)))
    (ecase code
      (F
       (xsetf (slice copy 2 :all 0) (slice cube 0 2 :all))
       (xsetf (slice copy 0 2 :all) (slice cube 4 '(2 0) 2))
       (xsetf (slice copy 4 :all 2) (slice cube 5 0 :all))
       (xsetf (slice copy 5 0 :all) (slice cube 2 '(2 0) 0)))
      (B
       (xsetf (slice copy 0 0 :all) (slice cube 2 :all 2))
       (xsetf (slice copy 2 :all 2) (slice cube 5 2 '(2 0)))
       (xsetf (slice copy 5 2 :all) (slice cube 4 :all 0))
       (xsetf (slice copy 4 :all 0) (slice cube 0 0 '(2 0))))
      (U
       (xsetf (slice copy 1 0 :all) (slice cube 2 0 :all))
       (xsetf (slice copy 2 0 :all) (slice cube 3 0 :all))
       (xsetf (slice copy 3 0 :all) (slice cube 4 0 :all))
       (xsetf (slice copy 4 0 :all) (slice cube 1 0 :all)))
      (D
       (xsetf (slice copy 1 2 :all) (slice cube 4 2 :all))
       (xsetf (slice copy 4 2 :all) (slice cube 3 2 :all))
       (xsetf (slice copy 3 2 :all) (slice cube 2 2 :all))
       (xsetf (slice copy 2 2 :all) (slice cube 1 2 :all)))
      (L
       (xsetf (slice copy 1 :all 0) (slice cube 0 :all 0))
       (xsetf (slice copy 0 :all 0) (slice cube 3 '(2 0) 2))
       (xsetf (slice copy 3 :all 2) (slice cube 5 '(2 0) 0))
       (xsetf (slice copy 5 :all 0) (slice cube 1 :all 0)))
      (R
       (xsetf (slice copy 1 :all 2) (slice cube 5 :all 2))
       (xsetf (slice copy 5 :all 2) (slice cube 3 '(2 0) 0))
       (xsetf (slice copy 3 :all 0) (slice cube 0 '(2 0) 2))
       (xsetf (slice copy 0 :all 2) (slice cube 1 :all 2)))
      (M
       (xsetf (slice copy 1 :all 1) (slice cube 0 :all 1))
       (xsetf (slice copy 0 :all 1) (slice cube 3 '(2 0) 1))
       (xsetf (slice copy 3 :all 1) (slice cube 5 '(2 0) 1))
       (xsetf (slice copy 5 :all 1) (slice cube 1 :all 1)))
      (E
       (xsetf (slice copy 1 1 :all) (slice cube 4 1 :all))
       (xsetf (slice copy 4 1 :all) (slice cube 3 1 :all))
       (xsetf (slice copy 3 1 :all) (slice cube 2 1 :all))
       (xsetf (slice copy 2 1 :all) (slice cube 1 1 :all)))
      (S
       (xsetf (slice copy 0 1 :all) (slice cube 4 '(2 0) 1))
       (xsetf (slice copy 4 :all 1) (slice cube 5 1 :all))
       (xsetf (slice copy 5 1 :all) (slice cube 2 '(2 0) 1))
       (xsetf (slice copy 2 :all 1) (slice cube 0 1 :all))))
    (awhen (position code '(U F R B L D))
      (single-rotate-side copy cube it))
    copy))

(defun render-cell (x y string colour)
  (set-rgb-stroke 0 0 0)
  (apply #'set-rgb-fill
         (ecase colour
           (white '(1 1 1))
           (red '(1 0 0))
           (blue '(0 0 1))
           (orange '(1 0.5 0))
           (green '(0 1 0))
           (yellow '(1 1 0))))
  (move-to x y)
  (line-to (+ x 40) y)
  (line-to (+ x 40) (+ y 40))
  (line-to x (+ y 40))
  (line-to x y)
  (fill-and-stroke)
  (set-rgb-fill 0 0 0)
  (draw-centered-string (+ x 20) (+ y 14) string)
  (stroke))

(defun render-side (x y side colours)
  (loop
    for i from 0 to 2
    do (loop
         for j from 0 to 2
         do (render-cell
             (+ x (* 40 i))
             (+ y (* 40 j))
             (string (xref side (- 2 j) i))
             (xref colours (- 2 j) i)))))

(defun side (cube side)
  (slice cube side :all :all))

(defun render-cube (pathname cube colours)
  (with-canvas (:width 480 :height 360)
    (let ((font (get-font #P"/usr/share/fonts/steamfonts/arial.ttf")))
      (set-font font 18)
      (set-rgb-fill 1 1 1)
      (set-rgb-stroke 0 0 0)
      (clear-canvas)
      (render-side 120 240 (side cube 0) (side colours 0))
      (render-side 120 120 (side cube 1) (side colours 1))
      (render-side 240 120 (side cube 2) (side colours 2))
      (render-side 360 120 (side cube 3) (side colours 3))
      (render-side 0 120 (side cube 4) (side colours 4))
      (render-side 120 0 (side cube 5) (side colours 5))
      (save-png pathname))))

(defun expand-cube-sequence (sequence)
  (loop
    with result
    for code in sequence
    do (ecase code
         ((F B U D L R M E S)
          (push code result))
         ((F2 B2 U2 D2 L2 R2 M2 E2 S2)
          (let ((code (intern (string (char (string code) 0)) '#:cl-user)))
            (dotimes (i 2)
              (push code result))))
         ((|F'| |B'| |U'| |D'| |L'| |R'| |M'| |E'| |S'|)
          (let ((code (intern (string (char (string code) 0)) '#:cl-user)))
            (dotimes (i 3)
              (push code result)))))
    finally (return (reverse result))))

(defun rotate-cube (cube sequence)
  (loop
    for code in (expand-cube-sequence sequence)
    do (setf cube (single-rotate-cube cube code)))
  cube)

(defun rotate-cubes (sequence &rest cubes)
  (mapcar (lambda (cube)
            (rotate-cube cube sequence))
          cubes))

(defun rotate-and-render (sequence &optional (pathname #P"cube.png"))
  (apply #'render-cube pathname (rotate-cubes sequence cube colours)))

(defvar superflip
  '(U R2 F B R B2 R U2 L B2 R |U'| |D'| R2 F |R'| L B2 U2 F2))

(defvar superflip2
  '(S U B2 D2 M |D'| M2 S U R2 D M2 U B2 U S2))

(def-test rotate-cube ()
  (is (equal cube (rotate-cube cube NIL))))
