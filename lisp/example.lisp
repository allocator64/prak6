(defun put (a name p)(setf (get a name) p))
(defun Plus5()
	(princ "Введите целое число: ")
	(put 'N 'Value (+ 5 (read)))
	(terpri)
	(princ "Число, большее на 5: ")
	(print (get 'N 'Value))
)
