# lisp2calc

Convert Lisp code (Common Lisp) into stack-based GNU Emacs Calc.

**TODO**

Exemple : xx

Add image

Recognized instructions in CL and Emacs Lisp

recognized operators:  
     - mod  
     - min (with 2 arguments)  
     - max (with 2 arguments)  
     - lcm (with 2 arguments)  
     - +  
     - *  
     - - unary (divide) or not  
     - / unary (inverse) or not  
     - progn  
     - let or let*  
     - (setq n (+ m 3)) with no use of the value, and only one assignment  
     - (incf i)
     - (decf i)
     
Not in CL :
     - (while (<= a b) body) but body shall not increase stack  
     - variants with < >= >

TODO : functions specific to calc, as those prime-related  
... we have to code them again here, so that codes could be executed in CL and Emacs Lisp also.

how does it work?  
output-and-stack = (output . stack)  
    where output is the current state of the future Calc command  
           at this stage of the conversion  
       for instance: (111 4 2 3 * 5 + + 222)  
   and stack is the current state of the stack (newest element first)  
       at this stage of the conversion  
       for instance: ((4 . NIL) (5. I))

(end of README)
