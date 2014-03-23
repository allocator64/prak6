(load './graph)

(defun processvertex (v cnt lst edge)
	(cond
		((null edge) Nil)
		(T (let ((from (first (first edge))) (to (second (first edge))))
			(cond
				((eq from v)
					(dfs to (+ 1 cnt) (cons v lst))
				)
			)
			(processvertex v cnt lst (cdr edge))
		))
	)
)

(defun dfs (v cnt lst)
	(cond
		((get 'G 'answer)
			Nil
		)
		((= cnt (get 'G 'N))
			(cond
				((eq v (get 'G 'first))
					(put 'G 'answer lst)
				)
				(T
					Nil
				)
			)
		)
		((eq (getcolor v) 1)
			Nil)
		(T
			(setcolor v 1)
			(processvertex v cnt lst (get 'G 'edge))
			(setcolor v 0)
		)
	)
)

(defun run ()
	(loadGraph)
	(put 'G 'answer Nil)
	(put 'G 'first (car (get 'G 'vertex)))
	(dfs (get 'G 'first) 0 Nil)
	(let ((ans (get 'G 'answer)))
		(cond
			((null ans)
				(print 'Not-Hamiltonian-Graph)
			)
			(T
				(print (cons (get 'G 'first) ans))
			)
		)
	)
)