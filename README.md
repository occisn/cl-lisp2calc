# cl-lisp2calc

Hobby project written in Common Lisp, which converts Lisp code (Common Lisp) into stack-based GNU Emacs Calc.

**WORK IN PROGRESS**

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
(to execute it, highlight instructions, `M-x read-kbd-macro`, then go to Calc and press `X`)

You can also refer to my project 'calc-programming', which essentially uses Calc to solve Project Euler puzzles.

Let's give an exemple of the possible use of this 'lisp2calc' code.

We choose [Project Euler 1](https://projecteuler.net/problem=1) as an example.  
_If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23. Find the sum of all the multiples of 3 or 5 below 1000._

Following code :
``` lisp
(cl-cl2calc:convert
 '(let ((n 1000)
        (res 0))
   (dotimes (i n)
     (when (= 0 (* (mod i 3) (mod i 5)))
       (setq res (+ res i))))
   res))
```
... returns:
```
1000 SPC 0 SPC 0 Z{ RET C-u 4 C-j 1 - a> Z/ 0 C-j 3 % C-u 3 C-j 5 % * a= Z[ C-j C-j + C-u 3 M-DEL TAB Z: Z] RET 1 + M-DEL C-u 1 TAB Z} DEL RET M-DEL M-DEL
```
When executed in GNU Emacs Calc, it yields the right result.  
(To execute it, highlight instructions, `M-x read-kbd-macro`, then go to Calc and press `X`)

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
     - `(setq n (+ m 3))` with no use of the value, and only one assignment  
     - `(incf i)`  
     - `(decf i)`  
     - `(if (= ...) ...)`  
     
Recognized operators not available in Common Lisp:
     - `(while (<= a b) body)` but body shall not increase stack, and variants with < >= >

**How does it work ?**

Code is interpreted, and output-and-stack = (output . stack) is updated at each stage  
... where output is the current state of the future Calc command  
           at this stage of the conversion  
       for instance: (111 4 2 3 * 5 + + 222)  
... and stack is the current state of the stack (newest element first)  
       at this stage of the conversion  
       for instance: ((4 . NIL) (5. I))

(end of README)
