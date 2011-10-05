;Copyright (c) 2011 Daniel Minor 
;
;Permission is hereby granted, free of charge, to any person obtaining a copy
;of this software and associated documentation files (the "Software"), to deal
;in the Software without restriction, including without limitation the rights
;to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;copies of the Software, and to permit persons to whom the Software is
;furnished to do so, subject to the following conditions:
;
;The above copyright notice and this permission notice shall be included in
;all copies or substantial portions of the Software.
;
;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;THE SOFTWARE.

(define-macro (match-structure exp . clauses)
    (letrec (
        (expand-args
            (lambda (struct args)
                (if (null? args)
                    '()
                    (let ((name (symbol->string (car args)))
                          (symname (car args)))
                        (append (list (list symname (list (string->symbol (string-append struct "-" name)) exp)))
                            (expand-args struct (cdr args))))))) 
        (expand-clauses 
            (lambda (exp clauses)
                (if (null? clauses)
                    '()
                    (if (eq? (caar clauses) 'else)
                        (list (list 'else (cadar clauses)))
                        (let ((name (symbol->string (caar clauses)))
                              (args (cadar clauses))
                              (body (caddar clauses)))
                            (append (list (list (list (string->symbol (string-append name "?")) exp)
                                          (list 'let (expand-args name args) body)))
                                    (expand-clauses exp (cdr clauses)))))))))
    (let ((expansion (expand-clauses exp clauses)))
        `(cond ,@expansion))))
