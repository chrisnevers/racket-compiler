(let ((x (box 0)))
(for ((:= x 0) (< (! x) 5) (incr x))
    (for-let ((:= j 0) (< (! j) 5) (incr j))
        (print (! j)))))

