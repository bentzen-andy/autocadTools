;; Function: HW Mesh It
;; Author: Andrew Bentzen 
;; Company: Haworth
;; Description: Mesh It is a helper function that can be used by other LISP programs. It's meant to be 
;;; a replacement for the EDGESURF command. It will take in the the 4 splines (or other line types) and 
;;; attempt to guess how "curvy" it is. If it things the spline is very curvy it will set the SURFTABs 
;;; to a high count. Otherwise it will try for a lower count. It has 4 "tiers" with hard-coded values.
;;; This function assumes your AutoCAD units are set to inches. 
;; Input: 4 splines (splines will be read by the program; all other entity types are ignored.), 
;;;       string (must be "edgesurf" or "rulesurf")
;;;       boolean - if true:  Mesh It will change the surfttab counts AND create a polygon mesh entity.
;;;               - if false: Mesh It will only change the surftab counts. 
;; Output: Changes AutoCAD system variables: SURFTAB1 and SURFTAB2. [Creates polygon mesh entity]

(defun mesh_it(ent0 ent1 ent2 ent3 mesh_cmd meshit / tier1 tier2 tier3 tier4 tier5 len_curve0 len_curve1
              len_curve2 len_curve3 dist_end_to_end0 dist_end_to_end1 dist_end_to_end2 
              dist_end_to_end3 curve_factor1 curve_factor2 surftab1_is_1 surftab2_is_1 midpoint
              x y z pt_lst spline1_type info i dist_from_mid1 dist_from_mid2 diff_of_distances ent_spline)
              
(progn
  ;; surtab 16
;  (setq tier1 0.300105)
  (setq tier1 0.4683625)
  ; ;surtab 8
;  (setq tier2 0.63662)
  (setq tier2 0.768468)
  ;; surtab 4
;  (setq tier3 0.900316)
  (setq tier3 0.9374045)
  ;; surtab 2
;  (setq tier4 0.974493)
  (setq tier4 0.9872465)
  ;; surtab 2
  (setq tier5 1)
)
  ;; set surftab values according to len_curve and curve of the splines
  (if (/= ent3 nil)
    (progn
      (setq len_curve0 (vlax-curve-getdistatparam ent0 (vlax-curve-getendparam ent0)))
      (setq len_curve1 (vlax-curve-getdistatparam ent1 (vlax-curve-getendparam ent1)))
      (setq len_curve2 (vlax-curve-getdistatparam ent2 (vlax-curve-getendparam ent2)))
      (setq len_curve3 (vlax-curve-getdistatparam ent3 (vlax-curve-getendparam ent3)))
  
      (setq dist_end_to_end0 (distance (vlax-curve-getendpoint ent0) (vlax-curve-getstartpoint ent0)))
      (setq dist_end_to_end1 (distance (vlax-curve-getendpoint ent1) (vlax-curve-getstartpoint ent1)))
      (setq dist_end_to_end2 (distance (vlax-curve-getendpoint ent2) (vlax-curve-getstartpoint ent2)))
      (setq dist_end_to_end3 (distance (vlax-curve-getendpoint ent3) (vlax-curve-getstartpoint ent3)))
      
      (setq curve_factor1 (min (/ dist_end_to_end0 len_curve0) (/ dist_end_to_end2 len_curve2)))
      (setq curve_factor2 (min (/ dist_end_to_end1 len_curve1) (/ dist_end_to_end3 len_curve3)))
    )
    (progn
      (setq len_curve0 (vlax-curve-getdistatparam ent0 (vlax-curve-getendparam ent0)))
      (setq len_curve1 (vlax-curve-getdistatparam ent1 (vlax-curve-getendparam ent1)))
      
      (setq dist_end_to_end0 (distance (vlax-curve-getendpoint ent0) (vlax-curve-getstartpoint ent0)))
      (setq dist_end_to_end1 (distance (vlax-curve-getendpoint ent1) (vlax-curve-getstartpoint ent1)))
      
      (setq curve_factor1 (min (/ dist_end_to_end0 len_curve0) (/ dist_end_to_end1 len_curve1)))
    )
  )
  
  (setvar "surftab1" 16)
  (setq surftab1_is_1 nil)
  (if (>= curve_factor1 tier1) (setvar "surftab1" 16))
  (if (>= curve_factor1 tier2) (setvar "surftab1" 8))
  (if (>= curve_factor1 tier3) (setvar "surftab1" 4))
  (if (>= curve_factor1 tier4) (setvar "surftab1" 4))
  (if (>= curve_factor1 tier5) (setq surftab1_is_1 T))

  (setvar "surftab2" 16)
  (setq surftab2_is_1 nil)
  (if (>= curve_factor2 tier1) (setvar "surftab2" 16))
  (if (>= curve_factor2 tier2) (setvar "surftab2" 8))
  (if (>= curve_factor2 tier3) (setvar "surftab2" 4))
  (if (>= curve_factor2 tier4) (setvar "surftab2" 4))
  (if (>= curve_factor2 tier5) (setq surftab2_is_1 T))
  
  (if (/= ent3 nil)
    (progn
      (setq diff 0)
      ;; measure the "curviness" of the spline.
      (foreach ent_spline (list ent0 ent2)
        (if (= (cdr (assoc 0 (entget ent_spline))) "SPLINE")
          (progn
            (setq x (/ (+ (nth 0 (vlax-curve-getstartpoint ent_spline)) (nth 0 (vlax-curve-getendpoint ent_spline))) 2))
            (setq y (/ (+ (nth 1 (vlax-curve-getstartpoint ent_spline)) (nth 1 (vlax-curve-getendpoint ent_spline))) 2))
            (setq z (/ (+ (nth 2 (vlax-curve-getstartpoint ent_spline)) (nth 2 (vlax-curve-getendpoint ent_spline))) 2))
  
            (setq midpoint (list x y z))

            (setq pt_lst nil)
            (setq spline1_type (vla-get-splinemethod (vlax-ename->vla-object ent_spline)))
            (setq info (entget ent_spline))
            (setq i 0)
            (while (< i (length info))
              (if (= (nth 0 (nth i info)) 10)
                (setq pt_lst (cons (cdr (nth i info)) pt_lst))
              )
              (setq i (+ i 1))
            )

            (setq dist_from_mid1 nil)
            (setq i 0)
            (while (< i (/ (length pt_lst) 2))
              (setq dist_from_mid1 (cons (distance midpoint (nth i pt_lst)) dist_from_mid1))
              (setq i (+ i 1))
            )
      
            (setq pt_lst (reverse pt_lst))
      
            (setq dist_from_mid2 nil)
            (setq i 0)
            (while (< i (/ (length pt_lst) 2))
              (setq dist_from_mid2 (cons (distance midpoint (nth i pt_lst)) dist_from_mid2))
              (setq i (+ i 1))
            )
        
            (setq diff_of_distances nil)
            (setq diff_of_distances (mapcar '- dist_from_mid1 dist_from_mid2))
      
  
            (setq diff_of_distances (mapcar 'abs diff_of_distances))
            (setq diff_of_distances (apply 'max diff_of_distances))
        
;            (princ "\n>>>>>>>>>>>>>>>>>>>>>diff_of_distances: ") (princ diff_of_distances)
        
            (setq diff (max diff diff_of_distances))
          )
        )
      )
  
      (if (> diff 2) (setvar "surftab1" (* (getvar "surftab1") 2)))
  
    
      (setq diff 0)
      ;; measure the "curviness" of the spline.
      (foreach ent_spline (list ent1 ent3)
        (if (= (cdr (assoc 0 (entget ent_spline))) "SPLINE")
          (progn
            (setq x (/ (+ (nth 0 (vlax-curve-getstartpoint ent_spline)) (nth 0 (vlax-curve-getendpoint ent_spline))) 2))
            (setq y (/ (+ (nth 1 (vlax-curve-getstartpoint ent_spline)) (nth 1 (vlax-curve-getendpoint ent_spline))) 2))
            (setq z (/ (+ (nth 2 (vlax-curve-getstartpoint ent_spline)) (nth 2 (vlax-curve-getendpoint ent_spline))) 2))
  
            (setq midpoint (list x y z))

            (setq pt_lst nil)
            (setq spline1_type (vla-get-splinemethod (vlax-ename->vla-object ent_spline)))
            (setq info (entget ent_spline))
            (setq i 0)
            (while (< i (length info))
              (if (= (nth 0 (nth i info)) 10)
                (setq pt_lst (cons (cdr (nth i info)) pt_lst))
              )
              (setq i (+ i 1))
            )

            (setq dist_from_mid1 nil)
            (setq i 0)
            (while (< i (/ (length pt_lst) 2))
              (setq dist_from_mid1 (cons (distance midpoint (nth i pt_lst)) dist_from_mid1))
              (setq i (+ i 1))
            )
      
            (setq pt_lst (reverse pt_lst))
      
            (setq dist_from_mid2 nil)
            (setq i 0)
            (while (< i (/ (length pt_lst) 2))
              (setq dist_from_mid2 (cons (distance midpoint (nth i pt_lst)) dist_from_mid2))
              (setq i (+ i 1))
            )
        
            (setq diff_of_distances nil)
            (setq diff_of_distances (mapcar '- dist_from_mid1 dist_from_mid2))
      
  
            (setq diff_of_distances (mapcar 'abs diff_of_distances))
            (setq diff_of_distances (apply 'max diff_of_distances))
        
;            (princ "\n>>>>>>>>>>>>>>>>>>>>>diff_of_distances: ") (princ diff_of_distances)
        
            (setq diff (max diff diff_of_distances))
          )
        )
      )
  
      (if (> diff 2) (setvar "surftab2" (* (getvar "surftab2") 2)))
  
    )
    (progn
      (setq diff 0)
      ;; measure the "curviness" of the spline.
      (foreach ent_spline (list ent0 ent1)
        (if (= (cdr (assoc 0 (entget ent_spline))) "SPLINE")
          (progn
            (setq x (/ (+ (nth 0 (vlax-curve-getstartpoint ent_spline)) (nth 0 (vlax-curve-getendpoint ent_spline))) 2))
            (setq y (/ (+ (nth 1 (vlax-curve-getstartpoint ent_spline)) (nth 1 (vlax-curve-getendpoint ent_spline))) 2))
            (setq z (/ (+ (nth 2 (vlax-curve-getstartpoint ent_spline)) (nth 2 (vlax-curve-getendpoint ent_spline))) 2))
  
            (setq midpoint (list x y z))

            (setq pt_lst nil)
            (setq spline1_type (vla-get-splinemethod (vlax-ename->vla-object ent_spline)))
            (setq info (entget ent_spline))
            (setq i 0)
            (while (< i (length info))
              (if (= (nth 0 (nth i info)) 10)
                (setq pt_lst (cons (cdr (nth i info)) pt_lst))
              )
              (setq i (+ i 1))
            )

            (setq dist_from_mid1 nil)
            (setq i 0)
            (while (< i (/ (length pt_lst) 2))
              (setq dist_from_mid1 (cons (distance midpoint (nth i pt_lst)) dist_from_mid1))
              (setq i (+ i 1))
            )
      
            (setq pt_lst (reverse pt_lst))
      
            (setq dist_from_mid2 nil)
            (setq i 0)
            (while (< i (/ (length pt_lst) 2))
              (setq dist_from_mid2 (cons (distance midpoint (nth i pt_lst)) dist_from_mid2))
              (setq i (+ i 1))
            )
        
            (setq diff_of_distances nil)
            (setq diff_of_distances (mapcar '- dist_from_mid1 dist_from_mid2))
      
  
            (setq diff_of_distances (mapcar 'abs diff_of_distances))
            (setq diff_of_distances (apply 'max diff_of_distances))
        
;            (princ "\n>>>>>>>>>>>>>>>>>>>>>diff_of_distances: ") (princ diff_of_distances)
        
            (setq diff (max diff diff_of_distances))
          )
        )
      )
    )
  )
  
  
  (if (/= ent3 nil)
    (progn
      (if (> (max len_curve0 len_curve2) 2) (setvar "surftab1" (* (getvar "surftab1") 2)))
      (if (> (max len_curve1 len_curve3) 2) (setvar "surftab2" (* (getvar "surftab2") 2)))
     
      (if (< (max len_curve0 len_curve2) 0.5) (setvar "surftab1" (max 4 (/ (getvar "surftab1") 2))))
      (if (< (max len_curve1 len_curve3) 0.5) (setvar "surftab2" (max 4 (/ (getvar "surftab2") 2))))
    )
    (progn
      (if (> (max len_curve0 len_curve1) 2) (setvar "surftab1" (* (getvar "surftab1") 2)))
      (if (< (max len_curve0 len_curve1) 0.5) (setvar "surftab1" (max 4 (/ (getvar "surftab1") 2))))
    )
  )
  
  (if meshit
    (progn  
      (if (= mesh_cmd "edgesurf") (vl-cmdf mesh_cmd ent0 ent1 ent2 ent3))
      (if (= mesh_cmd "rulesurf") (vl-cmdf mesh_cmd ent0 ent1))
  
      (if (= (cdr (assoc 0 (entget (entlast)))) "POLYLINE")
        (progn
          (if (= surftab1_is_1 T) (progn (repeat 6 (halfmesh (entlast)))))
          (if (= surftab2_is_1 T) (progn (fliping_MN (entlast)) (repeat 6 (halfmesh (entlast)))))
        )
      )
    )
  )
  curve_factor1
)

(defun fliping_mn(poly1 / lst vtz elst sel_prev lst meshObj_lst1 currVertexCount_M currVertexCount_N 
               m n lst2 new_pt_lst acadObj doc points modelSpace meshObj combo_poly pt_x pt_y pt_z)
  (setq lst nil)
  (setq vtx nil)
  (setq elst nil)
  (command "meshtype" "0")
  (setq sel_prev (getvar "selectionpreview"))
  (setvar "selectionpreview" 2)
  
  (if (= poly1 nil) (setq poly1 (car (entsel "\nFLIP M/N: Select a mesh to swap its M/N directions (i.e., to change the polyline path from vertical to horizontal and vice versa). "))))
  (setq vtx (entnext poly1))
  (while (/= (cdr (assoc 0 (setq elst (entget vtx)))) "SEQEND")
    (setq lst (cons (cdr (assoc 10 elst)) lst)
      vtx (entnext vtx)
    )
  )

  (setq meshObj_lst1 (vlax-ename->vla-object poly1))
  (setq currVertexCount_M (vla-get-MVertexCount meshObj_lst1))
  (setq currVertexCount_N (vla-get-NVertexCount meshObj_lst1))
  
  (setq *m* currVertexCount_M)
  (setq *n* currVertexCount_N)
  
  (setq m *m*)
  (setq n *n*)
  
  (setq n (/ (length lst) m))

  (setq lst (flippy lst m n))
  
  (setq lst2 lst)
  
  (setq new_pt_lst nil)
  (setq acadObj nil)
  (setq doc nil)
  (setq points nil)
  (setq modelSpace nil)
  (setq meshObj nil)
  (setq combo_poly nil)

  (setq lst (reverse lst))
  
  (setq n *m*)
  (setq m *n*)
  
  
  (while (/= lst nil)

    (setq pt_x nil)
    (setq pt_y nil)
    (setq pt_z nil)
  
    (setq pt_x (nth 0 (nth 0 lst)))
    (setq pt_y (nth 1 (nth 0 lst)))
    (setq pt_z (nth 2 (nth 0 lst)))
  
    (setq new_pt_lst (cons pt_z new_pt_lst))
    (setq new_pt_lst (cons pt_y new_pt_lst))
    (setq new_pt_lst (cons pt_x new_pt_lst))
    
    (setq lst (cdr lst))
  )
  
  (vl-load-com)
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))

  ;; Create the matrix of points
  (setq points (vlax-make-safearray vlax-vbDouble (cons 0 (- (length new_pt_lst) 1))))

    
  (vlax-safearray-fill points new_pt_lst)
    
  ;; creates a 3Dmesh in model space
  (setq modelSpace (vla-get-ModelSpace doc))
  (setq meshObj (vla-Add3DMesh modelSpace m n points))
  
  
  (setq combo_poly (vlax-vla-object->ename meshObj))
  (entdel poly1)
  
  (if (/= sel_prev nil) (setvar "selectionpreview" sel_prev))
  (princ "\nFLIP M/N: Complete. ")
  (princ)
)


(defun flippy(lst m n / y i j lst_modified lst_copy lst_reset)
;  (setq x nil)
  (setq y nil)
  (setq i nil)
  (setq j nil)
  (setq lst_modified nil)
;  (setq count nil)
  (setq lst_copy nil)
  (setq lst_reset nil)
  
;  (setq x m) ;; 8 
  (setq y n) ;; 3
  
  (setq i m) ;; 8 
  (setq j n) ;; 3
  
;  (setq count 0)
  (setq lst_copy lst)
  (setq lst_reset lst)

  (while (> j 0) ;; 3
    (while (> i 0) ;; 8 
      (setq lst_modified (cons (nth 0 lst_copy) lst_modified)) ;; _0 _3 _6 _9  _12 _15 _18 _21
      (while (> y 0) (setq lst_copy (cdr lst_copy)) (setq y (- y 1))) ;; skip over _0 _1 _2
      (setq y n)
      (setq i (- i 1))
    )
    (setq i m) ;; reset the incrementer 
;    (setq count (+ count 1)) ;; increment this one    
;    (setq x (- x 1)) ;; 7
    (setq lst_reset (cdr lst_reset))
    (setq lst_copy lst_reset)
    (setq j (- j 1))
  )
  lst_modified
)

(defun halfmesh(ent / acadObj doc set3DMesh_halfmesh vtx ent_lst elst n m i j lst_new flag)
  ;; set an undo marker. the end undo marker will be at the end of the entire script
  (vl-load-com)
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  (vla-StartUndoMark doc)
  ;; error handler to reset user settings if they hit esc 
  (defun *error*(msg)
    ;; end of undo marker 
    (vla-EndUndoMark doc)
    (princ)
  )
  
  (defun set3DMesh_halfmesh(lst nSize / mSize new_pt_lst acadObj doc points modelSpace meshObj)
    (setq mSize (/ (length lst) nSize))
    (setq new_pt_lst (apply 'append lst))
    (setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))
    ;; Create the matrix of points
    (setq points (vlax-make-safearray vlax-vbDouble (cons 0 (- (length new_pt_lst) 1))))
    (vlax-safearray-fill points new_pt_lst)
    ;; creates a 3Dmesh in model space
    (setq modelSpace (vla-get-ModelSpace doc))
    (setq meshObj (vla-Add3DMesh modelSpace mSize nSize points))
  )

(progn
  (setq ent_lst nil)
  (if (= ent nil) (setq ent (car (entsel "\nHALF MESH: Select mesh. "))))
  (if (= (cdr (assoc 0 (entget ent))) "POLYLINE")
    (progn
      (setq vtx (entnext ent))
      (while (/= (cdr (assoc 0 (setq elst (entget vtx)))) "SEQEND")
        (setq ent_lst (cons (cdr (assoc 10 elst)) ent_lst))
        (setq vtx (entnext vtx))
      )
    )
  )

  (setq n (cdr (assoc 72 (entget ent))))
  (setq m (cdr (assoc 71 (entget ent))))
  
  (setq lst_new nil)
  (setq i 0)
  
)
  (while (< i (length ent_lst))
    (setq j 0)
    (setq lst_new (cons (nth i ent_lst) lst_new))
    (if (and (= (rem (+ i 1) n) 0) (= flag nil))
      (while (< j n)
        (setq i (+ i 1))
;        (if (/= i n) (setq i (+ i 1)))
        (setq j (+ j 1))
      )
    )
    (setq i (+ i 1))
    (if (= (+ (+ i 1) n) (length ent_lst)) (setq flag T))
  )

  (set3DMesh_halfmesh lst_new n)
  (entdel ent)
  (setq ent (entlast))
  (setq n (cdr (assoc 72 (entget ent))))
  (setq m (cdr (assoc 71 (entget ent))))
  
  
  
  (setq ent nil)

  ;; end of undo marker 
  (vla-EndUndoMark doc)
  (princ "\nHALF MESH: Mesh ct = ") (princ (- m 1)) (princ " x ") (princ (- n 1))
  (princ "\nHALF MESH: Complete. ")
  (princ)
)