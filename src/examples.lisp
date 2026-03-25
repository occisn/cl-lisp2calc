(in-package :lisp2calc)

(defun main ()
  ;; PE 1 :

  (convert
   '(let ((n 1000)
          (res 0))
     (dotimes (i n)
       (when (= 0 (* (mod i 3) (mod i 5)))
         (setq res (+ res i))))
     res))

  ;; 233168

  ;; PE 5

  (convert
   '(let ((n 20)
          (res 1))
     (dotimes (i n)
       (setq res (lcm res (+ i 1))))
     res))

  ;; 232792560

  ;; PE 6:

  (convert
   '(let ((n 100) (res 0))
     (dotimes (i (+ n 1))
       (setq res (+ res i)))
     (setq res (* res res))
     (dotimes (i (+ n 1))
       (setq res (- res (* i i))))
     res))

  ;; 25164150

  ;; PE 9:

  (convert
   '(let ((n 1000)
          ;;(nb-solutions 0)
          (res -1))
     (let ((c n))
       (while (>= c 3)
         (let* ((bmax (min (- c 1) (- n c 1)))
                (bmin (max 2 (/ (- n c) 2)))
                (b bmax))
           (while (>= b bmin)
             (let ((a (- n b c)))
               (when (= (* c c) (+ (* a a) (* b b)))
                 ;;(incf nb-solutions)
                 (setq res (* a b c))))
             (setq b (- b 1))))
         (setq c (- c 1))))
     res))

  ;; 31875000

  ) ; end of main

;;; end
