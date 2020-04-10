;; Function: fix_spline_gap
;; Author: Andrew Bentzen 
;; Company: Haworth
;; Description: This helper function is designed to be called by another LISP function rather than via 
;;; AutoCAD command line. For 2 adjacent line entities, this function will look to see if they are 
;;; very close to touching. If they are very close, but not quite touching, then fix_spline_gap will
;;; move the endpoint of the spline to meet up with the other line entity. This function works great 
;;; as a way to "clean up" your line entities before using them in an EDGESURF command. Note: fix_spline_gap
;;; work by acting on the spline, but the function itself is a void function (i.e., it doesn't actually
;;; return a value. 
;; Input: 2 adjacent line entities (one of them must be a spline or else the function returns nothing)
;; Output: corrected spline

(defun fix_spline_gap(ent1 ent2 / e1_prop e2_prop connection_info e1_connection_pt e2_connection_pt flag_colocated info_new
                      e1_type e1_endpt1 e1_endpt2 e1_stype e2_type e2_endpt1 e2_endpt2 e2_stype info_new_e1 info_new_e2)
  ;; get ent type and endpoint
  ;;;; input:  ent 
  ;;;; output: list (<type>, <endpoint_1>, <endpoint_2> [<spline_type>])
  (defun get_ent_properties(ent / type spline_type end_pt_1 end_pt_2)
    (setq type (cdr (assoc 0 (entget ent))))
    (if (= type "SPLINE")
      (setq spline_type (vla-get-splinemethod (vlax-ename->vla-object ent)))
    )
    (setq end_pt_1 (car  (LM:ent->pts ent 10)))
    (setq end_pt_2 (last (LM:ent->pts ent 10)))
    
    (list type end_pt_1 end_pt_2 spline_type)
  )
  
  ;; find the connection point between the two entities 
  ;;;; input:  4 end points
  ;;;; output: connection_pt
  (defun get_connection_pt(lst1 lst2 / i j conn_pt )
    (setq i 0)
    (while (< i (length lst1))
      (setq j 0)
      (while (< j (length lst2))
        (if (< (distance (nth i lst1) (nth j lst2)) 0.0001) (setq conn_pt (nth j lst2)))
        (setq j (+ j 1))
      )
      (setq i (+ i 1))
    )
    conn_pt
  )
  
  
  ;; if the endpoints are a fraction of an inch apart, modify only the spline ent to match the other
  ;;;; input:  list (<entget for spline>, <spline_type>, <list of other ent's endpoints>)
  ;;;; output: new entget for spline
  (defun get_new_ent_info(info spline_type pt_new / dxf info_new i pt_old)  ;; pt_new
    (setq dxf (if (= spline_type 0) 11 10))
    (setq i 0)
    (while (< i (length info))
      (if (= (nth 0 (nth i info)) 10)
        (if (< (distance (cdr (nth i info)) pt_new) 0.0001)
          (setq pt_old (cdr (nth i info)))
        )
      )
      (setq i (+ i 1))
    )
    
    (setq info_new (subst (cons dxf pt_new) (cons dxf pt_old) info))
  )
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <Main program>
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq e1_prop (get_ent_properties ent1))
  (setq e2_prop (get_ent_properties ent2))
  
  ;;;; output: list (<type>, <endpoint_1>, <endpoint_2> [<spline_type>])
  (setq e1_type   (nth 0 e1_prop))
  (setq e1_endpt1 (nth 1 e1_prop))
  (setq e1_endpt2 (nth 2 e1_prop))
  (setq e1_stype  (nth 3 e1_prop))
  
  (setq e2_type   (nth 0 e2_prop))
  (setq e2_endpt1 (nth 1 e2_prop))
  (setq e2_endpt2 (nth 2 e2_prop))
  (setq e2_stype  (nth 3 e2_prop))
  
  (setq e1_connection_pt (get_connection_pt (list e1_endpt1 e1_endpt2) (list e2_endpt1 e2_endpt2)))
  (setq e2_connection_pt (get_connection_pt (list e2_endpt1 e2_endpt2) (list e1_endpt1 e1_endpt2)))
  
  (if (= e1_type "SPLINE")
    (progn 
      (setq info_new_e1 (get_new_ent_info (entget ent1) e1_stype e1_connection_pt))
      (entmod info_new_e1))
    (if (= e2_type "SPLINE")
      (progn 
        (setq info_new_e2 (get_new_ent_info (entget ent2) e2_stype e2_connection_pt))
        (entmod info_new_e2))
    )
  )
  (princ)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; </Main program>
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <HELPER FUNCTIONS>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Entity to Point List  -  Lee Mac
;; Returns a list of WCS points describing or approximating the supplied entity, else nil if the entity is not supported.
;; ent - [ent] Entity name to be described by point list (POINT/LINE/ARC/CIRCLE/LWPOLYLINE/POLYLINE/ELLIPSE/SPLINE)
;; acc - [num] Positive number determining the point density for non-linear objects
(defun LM:ent->pts ( ent acc / ang bul cen cls di1 di2 enx inc itm lst num ocs rad tot typ vt1 vt2 vtl )
(setq enx (entget ent)
      typ (cdr (assoc 0 enx))
)
(cond
    (   (= "POINT" typ)
        (list (cdr (assoc 10 enx)))
    )
    (   (= "LINE" typ)
        (mapcar '(lambda ( x ) (cdr (assoc x enx))) '(10 11))
    )
    (   (or (= "ARC" typ) (= "CIRCLE" typ))
        (if (= "ARC" typ)
            (setq ang (cdr (assoc 50 enx))
                  tot (rem (+ pi pi (- (cdr (assoc 51 enx)) ang)) (+ pi pi))
                  num (fix (+ 1.0 1e-8 (* acc (/ tot (+ pi pi)))))
                  inc (/ tot (float num))
                  num (1+ num)
            )
            (setq ang 0.0
                  tot (+ pi pi)
                  num (fix (+ 1e-8 acc))
                  inc (/ tot (float num))
            )
        )
        (setq cen (cdr (assoc 010 enx))
              rad (cdr (assoc 040 enx))
              ocs (cdr (assoc 210 enx))
        )
        (repeat num
            (setq lst (cons (trans (polar cen ang rad) ocs 0) lst)
                  ang (+ ang inc)
            )
        )
        (reverse lst)
    )
    (   (or (= "LWPOLYLINE" typ)
            (and (= "POLYLINE" typ) (zerop (logand (logior 16 64) (cdr (assoc 70 enx)))))
        )
        (if (= "LWPOLYLINE" typ)
            (setq vtl (LM:ent->pts:lwpolyvertices enx))
            (setq vtl (LM:ent->pts:polyvertices   ent))
        )
        (if (setq ocs (cdr (assoc 210 enx))
                  cls (= 1 (logand 1 (cdr (assoc 70 enx))))
            )
            (setq vtl (append vtl (list (cons (caar vtl) 0.0))))
        )
        (while (setq itm (car vtl))
            (setq vtl (cdr vtl)
                  vt1 (car itm)
                  bul (cdr itm)
                  lst (cons (trans vt1 ocs 0) lst)
            )
            (if (and (not (equal 0.0 bul 1e-8)) (setq vt2 (caar vtl)))
                (progn
                    (setq rad (/ (* (distance vt1 vt2) (1+ (* bul bul))) 4.0 bul)
                          cen (polar vt1 (+ (angle vt1 vt2) (- (/ pi 2.0) (* 2.0 (atan bul)))) rad)
                          rad (abs rad)                            
                          tot (* 4.0 (atan bul))
                          num (fix (+ 1.0 1e-8 (* acc (/ (abs tot) (+ pi pi)))))
                          inc (/ tot (float num))
                          ang (+ (angle cen vt1) inc)
                    )                        
                    (repeat (1- num)
                        (setq lst (cons (trans (polar cen ang rad) ocs 0) lst)
                              ang (+ ang inc)
                        )
                    )
                )
            )
        )
        (reverse (if cls (cdr lst) lst))
    )
    (   (= "ELLIPSE" typ)
        (setq di1 (vlax-curve-getdistatparam ent (vlax-curve-getstartparam ent))
              di2 (vlax-curve-getdistatparam ent (vlax-curve-getendparam   ent))
              di2 (- di2 1e-8)
        )
        (while (< di1 di2)
            (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
                  rad (distance '(0.0 0.0) (vlax-curve-getfirstderiv ent (vlax-curve-getparamatdist ent di1)))
                  di1 (+ di1 (/ di2 (1+ (fix (* acc (/ di2 rad (+ pi pi)))))))
            )
        )
        (reverse (if (vlax-curve-isclosed ent) lst (cons (vlax-curve-getendpoint ent) lst)))
    )
    (   (= "SPLINE" typ)
        (setq di1 (vlax-curve-getdistatparam ent (vlax-curve-getstartparam ent))
              di2 (vlax-curve-getdistatparam ent (vlax-curve-getendparam   ent))
              lst (list (vlax-curve-getstartpoint ent))
              inc (/ (- di2 di1) (float acc))
              di1 (+ di1 inc)
        )
        (repeat (1- (fix (+ 1e-8 acc)))
            (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
                  di1 (+ di1 inc)
            )
        )
        (reverse (if (vlax-curve-isclosed ent) lst (cons (vlax-curve-getendpoint ent) lst)))
    )
)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; </HELPER FUNCTIONS>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
