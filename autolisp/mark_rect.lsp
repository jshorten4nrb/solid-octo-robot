(defun c:mark_rect ( / p1 p2)
	(command "circle")
	(command "select" "previous")
	(command "CENTERMARK")
);_end defun