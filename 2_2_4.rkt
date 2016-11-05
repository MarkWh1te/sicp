#lang sicp
(#%require sicp-pict)
(paint diagonal-shading)
(define a (beside einstein(flip-vert einstein)))
(paint a)
(define b (below a a))
(paint b)