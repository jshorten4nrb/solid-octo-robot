;;  DimPoly.lsp [command names: DPI = Dimension Polyline(s) Inside, DPO = Outside]
;;  To dimension the lengths of all segments of selected Polylines on the Inboard or
;;    Outboard side.  For self-intersecting or open Polyline without a clear "inside" and
;;    "outside," will determine a side -- if not as desired, undo and run other command.
;;  Dimensions along arc segments will be angular Dimensions, showing length of arc
;;    as text override, not included angle native to angular Dimensions.  They will not
;;    update if Polyline is stretched, as those along line segments will; redo DPI/DPO.
;;  Uses current Dimension and Units settings; dimension line location distance from
;;    Polyline segment = 1.5 x dimension text height for stacked fractions to clear [see
;;    commentary at setting of dtxt variable re: stacked fractions].
;;  Accepts LW and 2D "heavy" Polylines, but not 3D Polylines or meshes.  Rejects any
;;    on locked Layers, because of Offset used to determine inside/outside.
;;  Kent Cooper, last edited 18 May 2016

(vl-load-com)

(defun DP (side / *error* doc svnames svvals styht dpss n pl cw inc pt1 pt3 pt2 ang1 ang2 dtxt pt4)

  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); if
    (mapcar 'setvar svnames svvals); reset System Variables
    (vla-endundomark doc)
    (princ)
  ); defun -- *error*

  (vla-startundomark (setq doc (vla-get-activedocument (vlax-get-acad-object))))
  (setq ; System Variable saving/resetting without separate variables for each:
    svnames '(osmode cmdecho blipmode clayer)
    svvals (mapcar 'getvar svnames)
  ); setq
  (mapcar 'setvar svnames '(0 0)); turn off Osnap, command echoing
  (command "_.layer" "_make" "A-ANNO-DIMS" "_color" 2 "" "");; <---EDIT Layer name/color as desired
  (setq styht (cdr (assoc 40 (tblsearch "style" (getvar 'dimtxsty))))); height of text style in current dimension style
  (if (= styht 0.0) (setq styht (* (getvar 'dimtxt) (getvar 'dimscale)))); if above is non-fixed-height

  (prompt (strcat "\nTo Dimension Polyline(s) on the " side "side,"))
  (if (setq dpss (ssget "_:L" '((0 . "*POLYLINE"))))
    (progn ; then [can omit this if you never use blips; also two other lines below]
      (setvar 'blipmode 0); [omit if you never use blips and omitted line above]
      (repeat (setq n (sslength dpss)); step through selection set
        (setq pl (ssname dpss (setq n (1- n))))
        (if (= (logand (cdr (assoc 70 (entget pl))) 88) 0)
          ;; not 3D or mesh [88 = 8 (3D) + 16 (polygon mesh) + 64 (polyface mesh)]
          (progn ; then
            (setq pl (vlax-ename->vla-object pl))
            (vla-offset pl styht); temporary
            (setq cw (< (vla-get-area (vlax-ename->vla-object (entlast))) (vla-get-area pl)))
              ;; clockwise for closed or clearly inside/outside open; may not give
              ;; desired result for open without obvious inside/outside
            (entdel (entlast))
            (repeat (setq inc (fix (vlax-curve-getEndParam pl))); segments
              (setq
                pt1 (vlax-curve-getPointAtParam pl inc)
                pt3 (vlax-curve-getPointAtParam pl (1- inc))
              ); setq
              (if (not (equal pt1 pt3 1e-8)); not coincident vertices
                (progn ; then -- proceed to dimension segment
                  (setq
                    pt2 (vlax-curve-getPointAtParam pl (- inc 0.5)); segment midpoint
                    ang1 (angle pt1 pt2)
                    ang2 (angle pt2 pt3)
                  ); setq
                  (if
                    (or ; line segment?
                      (equal ang1 ang2 1e-8); any non-0 direction or both reading as 0 or 2 pi
                      (equal (abs (- ang2 ang1)) (* pi 2) 1e-8); 0-degree with one reading as 2 pi +/-
                    ); or
                    (command "_.dimaligned" pt1 pt3); then [leaves at dimension line location prompt]
                    (progn ; else [arc segment]
                      (setq dtxt
                        (rtos
                          (abs ; length along arc segment
                            (- (vlax-curve-getDistAtParam pl inc) (vlax-curve-getDistAtParam pl (1- inc)))
                          ); abs
                          ;; [include mode/precision here if current dimension style's settings not desired]
                        ); rtos
                      ); setq
                      (if (wcmatch dtxt "*/*"); includes fraction?
                        ;; can omit this entire (if) function if you don't use stacked fractions
                        (setq dtxt ; stack it
                          (strcat
                            "\\A1;"
                            (vl-string-subst ";}\"" "\""
;                              (vl-string-subst "#" "/"
                                ;; remove ; from beginning of above line and its closing parenthesis line
                                ;; below to make diagonal stack [makes horizontal-line stack without]
                                (vl-string-subst "{\\H0.875x;\\S" " " dtxt)
                                  ;; change 0.875 ratio to agree with Dimension Style's setting
;                              ); -subst
                            ); -subst
                          ); strcat & dtxt
                        ); setq
                      ); if
                      (command
                        "_.dimangular" ""
                        (inters ; arc center
                          (setq pt4 (mapcar '/ (mapcar '+ pt1 pt2) '(2 2 2)))
                          (polar pt4 (+ (angle pt1 pt2) (/ pi 2)) 1)
                          (setq pt4 (mapcar '/ (mapcar '+ pt2 pt3) '(2 2 2)))
                          (polar pt4 (+ (angle pt2 pt3) (/ pi 2)) 1)
                          nil
                        ); inters
                        pt1 pt3
                        "_text" dtxt
                      ); command [leaves at dimension line location prompt]
                    ); progn
                  ); if
                  (command ; complete Dimension: dimension line location
                    (polar
                      pt2
                      (apply ; angle
                        (if (or (and cw (= side "in")) (and (not cw) (= side "out"))) '- '+)
                        (list
                          (angle '(0 0 0) (vlax-curve-getFirstDeriv pl (- inc 0.5)))
                          (/ pi 2)
                        ); list
                      ); apply
                      (* styht 1.5); distance
                        ;; [If you don't use stacked fractions, consider using styht without multiplier]
                    ); polar
                  ); command
                ); progn
              ); if [not coincident]
              (setq inc (1- inc))
            ); repeat [segments]
          ); progn -- then [LW/2D]
        ); if
      ); repeat [Polylines]
    ); progn -- then [valid selection]; [omit if you never use blips and omitted 2 lines above]
    (prompt "\nNo Polyline(s) on unlocked Layer(s) selected."); else
  ); if

  (mapcar 'setvar svnames svvals); reset System Variables
  (vla-endundomark doc)
  (princ)
); defun -- DP

(defun C:DPI () (DP "in")); = Dimension Polyline Inside
(defun C:DPO () (DP "out")); = Dimension Polyline Outside

(prompt "\nType DPI to Dimension Polyline(s) on the Inside, DPO to do so on the Outside.")