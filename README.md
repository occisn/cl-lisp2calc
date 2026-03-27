# cl-lisp2calc

Hobby project written in Common Lisp, which converts Lisp code (Common Lisp) into stack-based GNU Emacs Calc.

Table of contents:  
- [Explanations](#explanations)  
- [Testing](#testing)  
- [Applications](#applications) : Project Euler [1](#project-euler-1), [2](#project-euler-2), [5](#project-euler-5), [6](#project-euler-6), [9](#project-euler-9)

## Explanations

Calc is a stack-based calculator included in GNU Emacs. It can be used as a standard calculator similar to HP28/48 calculators, or for advanced mathematics.

Its [manual](https://www.gnu.org/software/emacs/manual/html_mono/calc.html) is available on-line, starting with a [Getting Started](https://www.gnu.org/software/emacs/manual/html_mono/calc.html#Getting-Started) section encompassing a [demonstration](https://www.gnu.org/software/emacs/manual/html_mono/calc.html#Demonstration-of-Calc).

There are several ways to program Calc (see [programming section](https://www.gnu.org/software/emacs/manual/html_mono/calc.html#Programming) of the manual). Here we will exclusively use Calc's _keyboard macros_, which lead to esoteric-looking code. 

For instance, if the stack contains the following elements:
```
2: 123456789
1: 3
```
the following macro will manipulate the above stack and return the 3 first digits of 123456789, that is to say 123:
```
TAB RET H L F 1 + C-u 3 C-M-i - n f S F
``` 
(to execute it, copy these instructions to Emacs, highlight them, `M-x read-kbd-macro`, then go to Calc and press `X`)

You can also refer to my project 'calc-programming', which essentially uses Calc to solve Project Euler puzzles.

Let's give an exemple of the possible use of this 'lisp2calc' code.

We choose [Project Euler 1](https://projecteuler.net/problem=1) as an example.  
_If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23. Find the sum of all the multiples of 3 or 5 below 1000._

Following code :
``` lisp
(lisp2calc:convert
 '(let ((n 1000)
        (sum 0))
   (dotimes (i n)
     (when (or (= 0 (mod i 3)) (= 0 (mod i 5)))
       (incf sum i)))
   sum))
```
... returns:
```
1000 SPC 0 SPC 0 Z{ RET C-u 4 C-j 1 - a> Z/ 0 C-j 3 % a= Z[ 1 Z: 0 C-j 5 % a= Z] Z[ C-j C-j + C-u 3 M-DEL TAB Z: Z] RET 1 + M-DEL Z} DEL RET M-DEL M-DEL
```
When executed in GNU Emacs Calc, it yields the right result.
(to execute it, copy these instructions to Emacs, highlight them, `M-x read-kbd-macro`, then go to Calc and press `X`)

Recognized Common Lisp macros or functions:  
     - `mod`  
     - `min` (with 2 arguments)  
     - `max` (with 2 arguments)  
     - `lcm` (with 2 arguments)  
     - `+`  
     - `*`  
     - `-` (which means 'negate' if only one argument)  
     - `/` (which means 'inverse' if only one argument)  
     - `progn`  
     - `let` or `let*`  
     - `(setq n (+ m 3))` with no use of the value,  
     - `(setq n (+ m 3) j (+ k 2))` = setq with multiple assignements,  
     - `(incf i)`  
     - `(incf i k)`  
     - `(decf i)`  
     - `(decf i k)`  
     - `(= a b)` as standalone expression (returns 0 or 1)
     - `(or (= a b) (= c d))` (returns 0 or 1, short-circuit via nested conditionals)
     - `(if (= ...) ...)` or `(if (or ...) ...)`
     - `(when (= ...) ...)` or `(when (or ...) ...)`

Recognized operators not available in Common Lisp (`l2c` suffix):  
     - `(while (<= a b) body)` but body shall not increase stack, and variants with `<` `>=` `>`

## Testing

Tests use the [Parachute](https://github.com/Shinmera/parachute) framework.

```lisp
(asdf:test-system "cl-lisp2calc")
```

### Emacs integration tests

In addition to unit tests, an optional suite of integration tests executes the generated Calc macros in a real Emacs Calc session (via `emacs --batch`) and verifies the results. These tests are **disabled by default**.

To enable them:

```lisp
(setf lisp2calc-tests::*run-emacs-tests* t)
(asdf:test-system "cl-lisp2calc")
```

**Prerequisite:** `emacs` must be available on your `PATH`. On Windows, you may need to set the full path to the executable:

```lisp
(setf lisp2calc-tests::*emacs-program*
      "C:/portable-programs/emacs-30.2/bin/emacs.exe")
```

You can also run a single macro manually from the command line:

```bash
emacs --batch -l elisp/calc-runner.el -f calc-runner--main "3 RET 4 +"
# Output: {"status":"ok","result":"7","stack_depth":1,...}
```

**How does it work ?**

Code is interpreted, and output-and-stack = (output . stack) is updated at each stage  
... where output is the current state of the future Calc command  
           at this stage of the conversion  
       for instance: (111 4 2 3 * 5 + + 222)  
... and stack is the current state of the stack (newest element first)  
       at this stage of the conversion  
       for instance: ((4 . NIL) (5. I))
       
Any comment? Open an [issue](https://github.com/occisn/cl-lisp2calc/issues), or start a discussion [here](https://github.com/occisn/cl-lisp2calc/discussions) or [at profile level](https://github.com/occisn/occisn/discussions).

## Applications

### Project Euler 1

_If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23. Find the sum of all the multiples of 3 or 5 below 1000._ [(source)](https://projecteuler.net/problem=1)

Lisp implementation:
``` lisp
(let ((n 1000)
      (sum 0))
  (dotimes (i n)
    (when (or (= 0 (mod i 3)) (= 0 (mod i 5)))
      (incf sum i)))
  sum)
```

Calc:
```
1000 SPC 0 SPC 0 Z{ RET C-u 4 C-j 1 - a> Z/ 0 C-j 3 % a= Z[ 1 Z: 0 C-j 5 % a= Z] Z[ C-j C-j + C-u 3 M-DEL TAB Z: Z] RET 1 + M-DEL Z} DEL RET M-DEL M-DEL
```

### Project Euler 2

_Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be: 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ... By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms._ [(source)](https://projecteuler.net/problem=2)

Lisp implementation:
``` lisp
(let ((n 4000000)
      (f1 0)
      (f2 1)
      (tmp 0)
      (sum 0))
  (while (<= f2 n)
    (when (= 0 (mod f2 2)) (incf sum f2))
    (setq tmp f1
          f1 f2
          f2 (+ tmp f2)))
  sum)
```

Calc:
```
4000000 SPC 0 SPC 1 SPC 0 SPC 0 Z{ C-u 3 C-j C-u 6 C-j a> Z/ 0 SPC C-u 4 C-j 2 % a= Z[ RET C-u 4 C-j + M-DEL Z: Z] C-u 4 C-j C-u 3 M-DEL TAB C-u 3 C-j C-u 5 M-DEL C-u 4 TAB C-j C-u 4 C-j + C-u 4 M-DEL C-u 3 TAB Z} RET M-DEL M-DEL M-DEL M-DEL M-DEL
```

### Project Euler 5

_What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?_ [(source)](https://projecteuler.net/problem=5)

Lisp implementation:
``` lisp
(let ((n 20)
      (res 1))
  (dotimes (i n)
    (setq res (lcm res (+ i 1))))
  res)
```

Calc:
```
20 SPC 1 SPC 0 Z{ RET C-u 4 C-j 1 - a> Z/ C-j C-j 1 + k l C-u 3 M-DEL TAB RET 1 + M-DEL Z} DEL RET M-DEL M-DEL
```

### Project Euler 6

_Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum._ [(source)](https://projecteuler.net/problem=6)

Lisp implementation:
``` lisp
(let ((n 100) (res 0))
  (dotimes (i (+ n 1))
    (setq res (+ res i)))
  (setq res (* res res))
  (dotimes (i (+ n 1))
    (setq res (- res (* i i))))
  res)
```

Calc:
```
100 SPC 0 SPC 0 Z{ RET C-u 4 C-j 1 + 1 - a> Z/ C-j C-j + C-u 3 M-DEL TAB RET 1 + M-DEL Z} DEL RET C-j * M-DEL 0 Z{ RET C-u 4 C-j 1 + 1 - a> Z/ C-j C-j C-u 3 C-j * - C-u 3 M-DEL TAB RET 1 + M-DEL Z} DEL RET M-DEL M-DEL
```

### Project Euler 9

_There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc._ [(source)](https://projecteuler.net/problem=9)

Lisp implementation:
``` lisp
(let ((n 1000)
      (res -1))
  (let ((c n))
    (while (>= c 3)
      (let* ((bmax (min (- c 1) (- n c 1)))
             (bmin (max 2 (/ (- n c) 2)))
             (b bmax))
        (while (>= b bmin)
          (let ((a (- n b c)))
            (when (= (* c c) (+ (* a a) (* b b)))
              (setq res (* a b c))))
          (setq b (- b 1))))
      (setq c (- c 1))))
  res)
```

Calc:
```
1000 SPC 1 n C-j Z{ 3 C-j a> Z/ RET 1 - C-u 4 C-j C-u 3 C-j 1 + - f n 2 SPC C-u 5 C-j C-u 4 C-j - 2 / f x C-j Z{ C-j C-j a> Z/ C-u 6 C-j C-j C-u 6 C-j + - C-u 5 C-j C-u 6 C-j * C-j C-u 3 C-j * C-u 4 C-j C-u 5 C-j * + a= Z[ RET C-u 3 C-j C-u 7 C-j * * C-u 7 M-DEL C-u 6 TAB Z: Z] DEL RET 1 - M-DEL Z} DEL DEL DEL RET 1 - M-DEL Z} DEL RET M-DEL M-DEL
```

## Additional commands

### prime-factorization

Returns the prime factors of a fixnum N (>= 2) as a flat list with multiplicity.

``` lisp
(lisp2calc::prime-factorization 12)   ;; => (2 2 3)
(lisp2calc::prime-factorization 210)  ;; => (2 3 5 7)
(lisp2calc::prime-factorization 1024) ;; => (2 2 2 2 2 2 2 2 2 2)
```

(end of README)
