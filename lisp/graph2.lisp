(load './graph)

; Fill colors to -1 color
; Assuming, that 0 color - it's left side of Biparted Graph, and 1 - right side
(defun resetColor (done lst)
	(cond
		((null lst) done)
		(T
			(resetColor (cons (list (first (car lst)) -1) done) (cdr lst))
		)
	)
)

; Walk through all edges from vertex
(defun processvertex (v col edge)
	(cond
		((null edge) Nil)
		(T (let ((from (first (first edge))) (to (second (first edge))))
			(cond
				((eq from v)
					(dfs to col))
			)
			(processvertex v col (cdr edge))
		))
	)
)

; Paint vertex
; If we already has visited this vertex, check for right color
(defun dfs (v col)
	(let ((curcolor (getcolor v)))
		(cond
			((eq curcolor col)
				Nil)
			((eq curcolor -1)
				(setcolor v col)
				(cond
					((eq col 0)
						(inc 'G 'Lcount)
					)
					(T
						(inc 'G 'Rcount)
					)
				)
				(processvertex v (logxor col 1) (get 'G 'edge))
			)
			(T
				(put 'G 'error 1)
				Nil
			)
		)
	)
)

; The main function
(defun run ()
	(loadGraph)
	(put 'G 'Lcount 0)
	(put 'G 'Rcount 0)
	(put 'G 'color (resetColor Nil (get 'G 'color)))
	(print (get 'G 'color))
	(dfs (car (get 'G 'vertex)) 0)
	(cond
		((get 'G 'error)
			(print 'Not-Bipartite-Graph)
		)
		(T (let ((L (get 'G 'Lcount)) (R (get 'G 'Rcount)))
			(cond
				((eq (+ L R) (get 'G 'N))
					(cond
						((eq (get 'G 'M) (* L R))
							(print 'Whole-Bipartite-Graph)
						)
						(T
							(print 'Bipartite-Graph)
						)
					)
				)
				(T
					(print 'Not-Connected-Graph)
				)
			)
		))
	)
)
