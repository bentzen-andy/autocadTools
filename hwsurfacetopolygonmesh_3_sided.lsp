(if (null combinemesh) (load "C:\\hw_commands\\lisp\\hwcombinemesh.lsp"))

;; Command: HW Surface to Polygon Mesh (3-Sided)
;; Author: Andrew Bentzen 
;; Company: Haworth
;; Description: Turns a 3-sided surface into a polygon mesh. The mesh's counts 
;;; depend on SURFTAB1 and SURFTAB2 
;; Input: 1 (3-Sided) surface entity, and the "main vertex" of that entity. 
;; Output: 1 polygon mesh entity
(defun c:hwsurfacetopolygonmesh_3_sided( / result ucs_pts jnk_lst i snap)
  (setq *JUNK* nil)
  ;; set an undo marker. the end undo marker will be at the end of the entire script
  (vl-load-com)
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  (vla-StartUndoMark doc)
  (setvar "meshtype" 0)
  
  
  ;; Get user input
  ;;;; inputs:  (1) 1 3-sided surface entity (from the user)
  ;;;;          (2) 1 "main vertex" of the 3-sided entity (from the user)
  ;;;; outputs: list: the 2 edges of the surface adjacent to the main vertex. 
  ;;;; info:    picks up the surface and main vertex from the user. copy/explode to 
  ;;;;          get the edges. check end points of each spline to see if touching 
  ;;;;          the main vertex. if true add to spline list, else not. 
  (defun get_user_input( / ent vertex_main ss i lst_splines)
    (vl-cmdf "ucs" "world")
    (while (= (setq ent (car (entsel "\nSURFACE -> MESH (3-SIDED): Select 3-sided surface: "))) nil))
    (setq *MAIN_SURFACE* ent)
    (while (= (setq vertex_main (getpoint "\nSURFACE -> MESH (3-SIDED): Pick the main vertex of the surface: ")) nil)) 
    (setq *MAIN_VERTEX* vertex_main)
    
    (vla-copy (vlax-ename->vla-object ent))
    (vl-cmdf "explode" (entlast))
    (setq ss (ssget "_p"))
    (setq i 0)
    (while (< i (sslength ss))
      (if (or (< (distance (last (LM:ent->pts (ssname ss i) 50.0)) vertex_main) 0.0001)
              (< (distance (car  (LM:ent->pts (ssname ss i) 50.0)) vertex_main) 0.0001))
        (setq lst_splines (cons (ssname ss i) lst_splines))
        (setq *MAIN_SPLINE* (ssname ss i))
      )
      (setq i (+ i 1))
    )
    (setq i 0)
    (while (< i (length lst_splines))
      (setq *JUNK* (cons (nth i lst_splines) *JUNK*))
      (setq i (+ i 1))
    )
    lst_splines
  )
  
  
  ;; Draw lines
  ;;;; inputs:  2 splines adjacent to the "main vertex"
  ;;;; outputs: list of line entities 
  ;;;; info:    find div points for each spline (according to surftab settings). 
  ;;;;          draw lines between the spline div points 
  ;;;;          TODO: figure out how to tell which points to connect between....   
  (defun draw_lines(ent1 ent2 / lst_lines lst1 lst2 i dist1 dist2)
    (progn
    (vl-cmdf "divide" ent1 (getvar "surftab2"))
    (setq lst1 (LM:ss->ent (ssget "_p")))

    (vl-cmdf "divide" ent2 (getvar "surftab2"))
    (setq lst2 (LM:ss->ent (ssget "_p")))
    (setq i 0)
    (while (< i (length lst1))
      (setq *JUNK* (cons (nth i lst1) *JUNK*))
      (setq *JUNK* (cons (nth i lst2) *JUNK*))
      (setq i (+ i 1))
    )
    (setq lst1 (mapcar '(lambda ( x ) (cdr (assoc 10 (entget x)))) lst1))
    (setq lst2 (mapcar '(lambda ( x ) (cdr (assoc 10 (entget x)))) lst2))
    )
    
    
    (setq dist1
      (+ 
        (distance (car lst1) (car lst2))
        (distance (last lst1) (last lst2))
      )
    )
    (setq dist2
      (+ 
        (distance (car lst1) (last lst2))
        (distance (last lst1) (car lst2))
      )
    )
    
    (if (> dist1 dist2)
      (setq lst2 (reverse lst2))
    )
    
    (setq i 0)
    (while (< i (length lst1))
      (vl-cmdf "line" (nth i lst1) (nth i lst2) "")
      (setq lst_lines (cons (entlast) lst_lines))
      (setq *JUNK* (cons (entlast) *JUNK*))
      (setq i (+ i 1))
    )
    (setq _lst lst_lines)
    
    lst_lines
  )
  
  
  ;; Extrude lines 
  ;;;; inputs:  list of line entities 
  ;;;; outputs: list of extruded surfaces entities
  ;;;; info:    
  (defun extrude_all(lst / acadObj doc modelSpace height taperAngle solidObj i lst_extruded_surfaces)
    (setq i 0)
    (while (< i (length lst))
      (vl-cmdf "extrude" (nth i lst) "" "d" '(0 0 0) '(0 0 1000))
      (setq lst_extruded_surfaces (cons (entlast) lst_extruded_surfaces))
      (vl-cmdf "extrude" (nth i lst) "" "d" '(0 0 0) '(0 0 -1000))
      (setq lst_extruded_surfaces (cons (entlast) lst_extruded_surfaces))
      (setq i (+ i 1))
    )
    lst_extruded_surfaces
  )
  
  
  ;; Map Surfaces 
  ;;;; inputs:  (1) list of extruded surface entities 
  ;;;;          (2) 1 surface entity 
  ;;;; outputs: list of const. spline entities. 
  ;;;; info:    
  (defun map_lines_to_surface(lst ent_surface / i lst2 j lst_const)
    (setq i 0)
    (setq lst_const nil)
    (while (< i (length lst))
      (vla-copy (vlax-ename->vla-object ent_surface))
      (vl-cmdf "intersect" (entlast) (nth i lst) "")
      (if (= (cdr (assoc 0 (entget (entlast)))) "POINT")
        (progn 
          (entdel (entlast))
          (if (= (cdr (assoc 0 (entget (entlast)))) "POINT")
            (setq *JUNK* (cons (entlast) *JUNK*))
          )
        )
        (setq lst_const (cons (entlast) lst_const))
      )
      (setq i (+ i 1))
    )
    lst_const
  )
  
  
  ;; Rulesurf between all const. splines and the main vertex
  ;;;; inputs:  list of construction lines 
  ;;;; outputs: none
  ;;;; info:    
  (defun rulesurf_all(lst / lst2 i rulesurf_lst)
    (defun mk_rulesurfs(lst / pt1 pt2 ss i rulesurf_lst st curve_factor surftab1_is_1)
      (setq ss (ssadd))
      (foreach x lst
        (ssadd x ss)
      )

      (setq i 0)
      (setq rulesurf_lst nil)
      (while (< i (sslength ss))
        (vl-cmdf "rulesurf" (ssname ss i) (ssname ss (+ i 1)))
        (setq rulesurf_lst (cons (entlast) rulesurf_lst))
        (if (and (> (length rulesurf_lst) 1) (/= i (- (sslength ss) 1)))
          (progn
            (combinemesh (nth 0 rulesurf_lst) (nth 1 rulesurf_lst))
            (setq rulesurf_lst (cons (entlast) rulesurf_lst))
          )
        )
        (setq i (+ i 1))
      )
    )
  
    (setq lst2 nil)
    (vl-cmdf "point" *MAIN_VERTEX*)
    (setq *MAIN_VERTEX* (entlast))
    
    
    
    
    (if (<
          (+ (distance (cdr (assoc 10 (entget *MAIN_VERTEX*))) (car  (LM:ent->pts (nth 0 lst) 10.0)))
             (distance (cdr (assoc 10 (entget *MAIN_VERTEX*))) (last (LM:ent->pts (nth 0 lst) 10.0)))
          )
          (+ (distance (cdr (assoc 10 (entget *MAIN_VERTEX*))) (car  (LM:ent->pts (last lst) 10.0)))
             (distance (cdr (assoc 10 (entget *MAIN_VERTEX*))) (last (LM:ent->pts (last lst) 10.0)))
          )
        )
      (progn
        (setq lst2 (cons *MAIN_VERTEX* lst2))
        (setq i 0)
        (while (< i (length lst))
          (setq lst2 (cons (nth i lst) lst2))
          (setq i (+ i 1))
        )
        (setq lst2 (cons *MAIN_SPLINE* lst2))
      )
    
      (progn
        (setq lst2 (cons *MAIN_SPLINE* lst2))
        (setq i 0)
        (while (< i (length lst))
          (setq lst2 (cons (nth i lst) lst2))
          (setq i (+ i 1))
        )
        (setq lst2 (cons *MAIN_VERTEX* lst2))
      )
    )
    
    (mk_rulesurfs lst2)
    
    (setq i 0)
    (while (< i (length lst2))
      (setq *JUNK* (cons (nth i lst2) *JUNK*))
      (setq i (+ i 1))
    )
    (princ)
  )
  
  ;; Delete excess construction lines
  ;;;; inputs:  list of construction lines 
  ;;;; outputs: none
  ;;;; info:    
  (defun const_lines_off(lst / i )
    (setq i 0)
    (while (< i (length lst))
      (if (entget (nth i lst))
        (entdel (nth i lst))
      )
      (setq i (+ i 1))
    )
    (princ)
  )


  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; <MAIN PROGRAM>
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (setq result (get_user_input))
  (setq snap (getvar "osmode"))
  (setvar "osmode" 0)
  ;; set the UCS to the 3 points on the second spline
  (setq ucs_pts nil)
  (vl-cmdf "divide" (nth 1 result) 2)
  (setq jnk_lst (LM:ss->ent (ssget "_p")))
  (setq i 0)
  (while (< i (length jnk_lst))
    (setq *JUNK* (cons (nth i jnk_lst) *JUNK*))
    (setq i (+ i 1))
  )
  (setq ucs_pts (cons (car  (LM:ent->pts (nth 1 result) 10.0)) ucs_pts))
  (setq ucs_pts (cons (cdr (assoc 10 (entget (ssname (ssget "_p") 0 )))) ucs_pts))
  (setq ucs_pts (cons (last (LM:ent->pts (nth 1 result) 10.0)) ucs_pts))
  
  (setq result (draw_lines (nth 0 result) (nth 1 result)))
  
  (setq i 0)
  (while (< i (length result))
    (vl-cmdf "lengthen" "percent" 120 (cdr (assoc 10 (entget (nth i result)))) "")
    (vl-cmdf "lengthen" "percent" 120 (cdr (assoc 11 (entget (nth i result)))) "")
    (setq i (+ i 1))
  )
  
  (vl-cmdf "ucs" "3p" (nth 0 ucs_pts) (nth 1 ucs_pts) (nth 2 ucs_pts))
  
  (setq result (extrude_all result))
  (vl-cmdf "ucs" "world")
  (setq result (map_lines_to_surface result *MAIN_SURFACE*))
  (setq result (rulesurf_all result))
  
  (const_lines_off *JUNK*)
  (setq *JUNK* nil)
  (setvar "osmode" snap)
  ;; end of undo marker 
  (vla-EndUndoMark doc)
  (princ "\nSURFACE -> POLYGON MESH (3-SIDED): Complete. ")
  (princ)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; </MAIN PROGRAM>
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun LM:ent->pts:lwpolyvertices ( enx / elv lst vtx )
(setq elv (list (cdr (assoc 38 enx))))
(while (setq vtx (assoc 10 enx))
    (setq enx (cdr (member vtx enx))
          lst (cons (cons (append (cdr vtx) elv) (cdr (assoc 42 enx))) lst)
    )
)
(reverse lst)
)

(defun LM:ent->pts:polyvertices ( ent / lst vte vtx )
(setq vte (entnext ent)
      vtx (entget  vte)
)   
(while (= "VERTEX" (cdr (assoc 0 vtx)))
    (setq lst (cons (cons (cdr (assoc 10 vtx)) (cdr (assoc 42 vtx))) lst)
          vte (entnext vte)
          vtx (entget  vte)
    )
)
(reverse lst)
)

(defun LM:ss->ent ( ss / i l )
(if ss
  (repeat (setq i (sslength ss))
    (setq l (cons (ssname ss (setq i (1- i))) l))
  )
)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; </HELPER FUNCTIONS>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;