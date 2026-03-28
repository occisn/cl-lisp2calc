# cl-lisp2calc

Hobby project written in Common Lisp, which converts Lisp code (Common Lisp) into stack-based GNU Emacs Calc.

Table of contents:  
- [Usage](#usage)  
- [About GNU Emacs Calc](#about-gnu-emacs-calc)  
- [Explanations on lisp2calc (the present project](#explanations-on-lisp2calc-the-present-project)  
- [Testing](#testing)  
- [Applications](#applications) : Project Euler [1](#project-euler-1), [2](#project-euler-2), [3](#project-euler-3), [4](#project-euler-4), [5](#project-euler-5), [6](#project-euler-6), [7](#project-euler-7)

## Usage

```lisp
;; Load the system:
(asdf:load-system "cl-lisp2calc")

;; Convert a Lisp expression to a Calc macro:
(lisp2calc:convert '(+ 3 4))
```

The `convert` function prints the corresponding Calc keyboard macro:
```
output = 3 SPC 4 +
```

To run it: copy the macro to Emacs, highlight it, `M-x read-kbd-macro`, then go to Calc and press `X`.

The `main` function provides more elaborate examples based on Project Euler problems (see [Applications](#applications)):
```lisp
(lisp2calc::main)
```

## About Gnu Emacs Calc

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

## Explanations on lisp2calc (the present project)
    
Let's give an exemple of the possible use of this 'lisp2calc' code.

We choose [Project Euler 1](https://projecteuler.net/problem=1) as an example.  
_If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23. Find the sum of all the multiples of 3 or 5 below 1000._

Following Lisp (Common Lisp) code :
``` lisp
(lisp2calc:convert
 '(let* ((n 1000)
         (sum 0))
   (dotimes (i n)
     (when (or (= 0 (mod i 3)) (= 0 (mod i 5)))
       (incf sum i)))
   sum))
```
... is convert info:
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
     - `floor` (with 2 arguments: `(floor n p)` → integer division)
     - `+`  
     - `*`  
     - `-` (which means 'negate' if only one argument)  
     - `/` (which means 'inverse' if only one argument)  
     - `progn`  
     - `let` (one binding only) or `let*`
     - `(setq n (+ m 3))` or `(setf n (+ m 3))` with no use of the value,
     - `(setq n (+ m 3) j (+ k 2))` = setq/setf with multiple assignements,
     - `(incf i)`  
     - `(incf i k)`  
     - `(decf i)`  
     - `(decf i k)`  
     - `(= a b)` as standalone expression (returns 0 or 1)
     - `(not expr)` — boolean negation (returns 0 or 1)
     - `(or expr1 expr2)` (returns 0 or 1, short-circuit via nested conditionals; args can be any comparison including `=`/`>`/`>=`/`<`/`<=`, or nested logical expr)
     - `(and expr1 expr2)` (returns 0 or 1, short-circuit via nested conditionals; args can be any comparison including `=`/`>`/`>=`/`<`/`<=`, or nested logical expr)
     - `(if cond ...)` or `(when cond ...)` where `cond` is any logical expression (`=`, `>`, `>=`, `<`, `<=`, `or`, `and`, `not`)
     - `(dotimes (_ N) body...)` → Calc's `N Z< body Z>` (repeat body N times, unused loop variable)
     - `(loop repeat N do body...)` → Calc's `N Z< body Z>` (repeat body N times)

Recognized operators not available in Common Lisp (internal to `lisp2calc` package, use `l2c::` prefix):
     - `(while cond body)` where `cond` is a direct comparison (`<=`, `<`, `>=`, `>`, `/=`) or a logical expression (`or`, `and`, `not` wrapping any comparison including `=`), but body shall not increase stack
     - `(prime-factorization n)` → Calc's `k f`
     - `(primep n)` → Calc's `k f v l 1 a=` (returns 1 if prime, 0 otherwise)
     - `(next-prime n)` → Calc's `k n` (next prime after n)
     - `(last-element lst)` = `(car (last lst))` → Calc's `v v v r 1`

## Testing

Tests use the [Parachute](https://github.com/Shinmera/parachute) framework.

```lisp
(asdf:test-system "cl-lisp2calc")
```

### Emacs integration tests

In addition to unit tests, an optional suite of integration tests executes the generated Calc macros in a real Emacs Calc session (via `emacs --batch`) and verifies the results. These tests are **enabled by default**.

To disable them:

```lisp
(setf lisp2calc-tests::*run-emacs-tests* nil)
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
(let* ((n 1000)
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
(let* ((n 4000000)
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

### Project Euler 3

_The prime factors of 13195 are 5, 7, 13 and 29. What is the largest prime factor of the number 600851475143?_ [(source)](https://projecteuler.net/problem=3)

Lisp implementation:
``` lisp
(last-element (prime-factorization 600851475143))
```

Calc:
```
600851475143 k f v v v r 1
```

### Project Euler 4

_A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 x 99. Find the largest palindrome made from the product of two 3-digit numbers._ [(source)](https://projecteuler.net/problem=4)

Lisp implementation:
``` lisp
(let* ((max-palindrome 0)
       (i 999))
  (while (>= i 100)
    (let ((j i))
      (while (and (>= j 100) (> (* i j) max-palindrome))
        (let* ((product (* i j))
               (reversed (let* ((num product)
                                (acc 0))
                           (while (/= num 0)
                             (setf acc (+ (* 10 acc) (mod num 10)))
                             (setf num (floor num 10)))
                           acc)))
          (when (= product reversed)
            (setf max-palindrome product)))
        (decf j)))
    (decf i))
  max-palindrome)
```

Calc:
```
0 SPC 999 Z{ 100 C-j a> Z/ RET Z{ RET 100 SPC 1 - a> Z[ C-j C-j * C-u 4 C-j a> Z: 0 Z] 0 a= Z/ C-j C-j * RET 0 Z{ C-j 0 a= Z/ 10 C-j * C-u 3 C-j 10 % + M-DEL C-j 10 / F C-u 3 M-DEL TAB Z} RET M-DEL M-DEL C-j C-j a= Z[ C-j C-u 6 M-DEL C-u 5 TAB Z: Z] DEL DEL RET 1 - M-DEL Z} DEL RET 1 - M-DEL Z} C-j M-DEL M-DEL
```

### Project Euler 5

_2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder. What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?_ [(source)](https://projecteuler.net/problem=5)

Lisp implementation:
``` lisp
(let* ((n 20)
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

_The sum of the squares of the first ten natural numbers is 385. The square of the sum of the first ten natural numbers is 3025. Hence the difference is 3025 - 385 = 2640. Find the difference between the square of the sum and the sum of the squares of the first one hundred natural numbers._ [(source)](https://projecteuler.net/problem=6)

Lisp implementation:
``` lisp
(let* ((n 100) (res 0))
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

### Project Euler 7

_By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13. What is the 10001st prime number?_ [(source)](https://projecteuler.net/problem=7)

Two variants are proposed.

First Lisp implementation (using `loop repeat`):
``` lisp
(let ((n 2))
  (loop repeat 10000 do (setq n (next-prime n)))
  n)
```

Second Lisp implementation (using `dotimes _`):
``` lisp
(let ((n 2))
  (dotimes (_ 10000) (setq n (next-prime n)))
  n)
```

Calc:
```
2 SPC 10000 Z< RET k n M-DEL Z> RET M-DEL
```

(end of README)
