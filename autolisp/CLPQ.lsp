(vl-load-com)
(defun C:CLPQ ; = Center Lines in Polyline Quadrilaterals
  (/ ss n pl)
  (if (setq ss (ssget '((0 . "LWPOLYLINE") (90 . 4) (-4 . "&") (70 . 1))))
    (repeat (setq n (sslength ss))
      (setq pl (ssname ss (setq n (1- n))))
      (command
        "_.line"
        "_non" (vlax-curve-getPointAtParam pl 0.5)
        "_non" (vlax-curve-getPointAtParam pl 2.5)
        ""
        "_.line"
        "_non" (vlax-curve-getPointAtParam pl 1.5)
        "_non" (vlax-curve-getPointAtParam pl 3.5)
        ""
      ); command
    ); repeat
  ); if
  (princ)
); defun