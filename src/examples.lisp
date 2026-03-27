(in-package :lisp2calc)

(defun main ()
  ;; PE 1 :

  (convert
   '(let ((n 1000)
          (sum 0))
     (dotimes (i n)
       (when (or (= 0 (mod i 3)) (= 0 (mod i 5)))
         (incf sum i)))
     sum))

  ;; 233168

  ;; PE 3 :

  (convert
   '(last-element (prime-factorization 600851475143)))

  ;; 6857

  ;; PE 2 :

  (convert
   '(let ((n 4000000)
          (f1 0)
          (f2 1)
          (tmp 0)
          (sum 0))
     (while (<= f2 n)
       (when (= 0 (mod f2 2)) (incf sum f2))
       (setq tmp f1
             f1 f2
             f2 (+ tmp f2)))
     sum))

  ;; 4613732

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
