; Put element to property
(defun put (obj name p)(setf (get obj name) p))

; Append element to property
(defun apped (obj name val)
	(put obj name (cons val (get obj name)))
)

; Increment property by 1
(defun inc (obj name)
	(put obj name (+ 1 (get obj name)))
)

; Read list edges from stdin
; Assume, we have not directed graph
(defun loadEdge (lst cnt)
	(cond
		((= cnt 0) lst)
		(T (loadEdge 
			(let
				((v1 (read)) (v2 (read)))
				(cons (list v2 v1) (cons (list v1 v2) lst))
			)
			(- cnt 1)))
	)
)

; Check exising element in list
(defun hasinlist (lst elem)
	(cond
		((null lst) Nil)
		((eq (first (first lst)) elem) T)
		(T (hasinlist (cdr lst) elem))
	)
)

; Set color 0 to all vertex
(defun fillColor (vertex colorlist)
	(cond
		((null vertex) colorlist)
		(T (let ((elem (first vertex)))
			(cond
				((hasinlist colorlist elem)
					(fillColor (cdr vertex) colorlist))
				(T (fillColor (cdr vertex) (cons (list elem 0) colorlist)))
			)
			)
		)
	)
)

; Set value by key in list
(defun alterlist (lst v c)
	(cond
		((null lst) Nil)
		((eq (first (first lst)) v) (cons (list v c) (cdr lst)))
		(T (cons (car lst) (alterlist (cdr lst) v c)))
	)
)

; Change color for vertex
(defun setcolor (v c)
	(put 'G 'color (alterlist (get 'G 'color) v c))
)

; Find value by key in list
(defun findlist (lst v)
	(cond
		((null lst) Nil)
		((eq (first (first lst)) v) (second (first lst)))
		(T (findlist (cdr lst) v))
	)
)

; Get color by vertex
(defun getcolor (v)
	(findlist (get 'G 'color) v)
)

; Find key by value in list
(defun findValList (lst val)
	(cond
		((null lst) Nil)
		((eq (second (first lst)) val) (first (first lst)))
		(T (findValList (cdr lst) val))
	)
)

; Find any vertex with color 0
(defun nullcolor ()
	(findValList (get 'G 'color) 0)
)

; Read vertex list from stdin
(defun loadVertex (cnt lst)
	(cond
		((= cnt 0) lst)
		(T (loadVertex (- cnt 1) (cons (read) lst)))
	)
)

; Load graph in format:
; <Vertex count>
; <Vertex 1> <Vertex 2> ... <Vertex <Vertex count>>
; <Edges count>
; <Vertex-from 1> <Vertex-to 1>
; <Vertex-from 2> <Vertex-to 2>
; ...
; <Vertex-from <Edges count>> <Vertex-to <Edges count>>
(defun loadGraph ()
	(put 'G 'n (read))
	(put 'G 'vertex (loadVertex (get 'G 'n) Nil))
	(put 'G 'm (read))
	(put 'G 'edge (loadEdge Nil (get 'G 'm)))
	(put 'G 'color (fillColor (get 'G 'vertex) Nil))
	(put 'G 'error Nil)
	(put 'G 'answer Nil)
)

; Walk through all edges from vertex, except parent
; Also check for cycles
(defun processvertex (v parent cnt edge)
	(cond
		((null edge) Nil)
		(T (let ((from (first (first edge))) (to (second (first edge))))
			(cond
				((and (eq from v) (not (eq to parent)))
					(cond
						((eq (getcolor to) 0) 
							(dfs to from cnt))
						(T (put 'G 'error 1))
					)
				)
			)
			(processvertex v parent cnt (cdr edge))
		))
	)
)


; Find tree with vertex v
(defun dfs (v parent cnt)
	(setcolor v -1)
	(apped 'G 'curlist v)
	(processvertex v parent cnt (get 'G 'edge))
	(setcolor v cnt)
)

; Get first non-visited vertex and find tree with it
(defun getforest (cnt)
	(put 'G 'curlist Nil)
	(let ((elem (nullcolor)))
		(cond
			((null elem) Nil)
			(T
				(dfs elem Nil cnt)
				(apped 'G 'answer (get 'G 'curlist))
				(getforest (+ 1 cnt))
			)
		)
	)
)

; The main function
(defun run ()
	(loadGraph)
	(getforest 1)
	(cond
		((get 'G 'error) (print 'Not-a-forest))
		(T (print (get 'G 'answer)))
	)
)