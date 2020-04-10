;; Command: HW Set Mesh
;; Author: Andrew Bentzen 
;; Company: Haworth
;; Description: Command adjusts the alignment of the point list that comprises a polygon mesh. This 
;;; command is mainly intended to be used by other LISP functions. 
;; Input: 3 adjacent vertexes of a polygon mesh entity (points must be in an "L" shape. 
;; Output: Returns polygon mesh with a corrected point list. 
(defun c:hwsetmesh( / ent pt1 pt2 pt3 poly)
(progn
  (vl-cmdf "ucs" "world")
  (setq ent (car (entsel "\nSET MESH: Select mesh. ")))
  (setq pt1 (getpoint "\nSET MESH: Select first point. "))
  (setq pt2 (getpoint "\nSET MESH: Select second point. "))
  (setq pt3 (getpoint "\nSET MESH: Select third point. "))
)
  (setq poly (setmesh ent pt1 pt2 pt3))
  poly
)


(defun setmesh(poly pt1 pt2 pt3 / get_index_pts poly n m i fix_count)
  
  (defun get_index_pts(pt1 pt2 pt3 poly / vtx lst1 elst meshObj_lst1 m n index_pt1 index_pt2 index_pt3
                       indices poly_layer)
    (if (= (cdr (assoc 0 (entget poly))) "POLYLINE")
      (progn
        (setq vtx (entnext poly))
        (while (/= (cdr (assoc 0 (setq elst (entget vtx)))) "SEQEND")
          (setq lst1 (cons (cdr (assoc 10 elst)) lst1))
          (setq vtx (entnext vtx))
        )
      )
    )
  
    (setq meshObj_lst1 (vlax-ename->vla-object poly))
    (setq m (vla-get-MVertexCount meshObj_lst1))
    (setq n (vla-get-NVertexCount meshObj_lst1))
  
    (setq index_pt1 (vl-position T (mapcar '(lambda (x) (equal x pt1 0.0001)) (reverse lst1))))
    (setq index_pt2 (vl-position T (mapcar '(lambda (x) (equal x pt2 0.0001)) (reverse lst1))))
    (setq index_pt3 (vl-position T (mapcar '(lambda (x) (equal x pt3 0.0001)) (reverse lst1))))
    (setq indices (list index_pt1 index_pt2 index_pt3))
    indices
  )
 
  (vl-load-com)
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  ;; set an undo marker. the end undo marker will be at the end of the entire script
  (vla-StartUndoMark doc)
  
  (setq poly_layer (cdr (assoc 8 (entget poly))))
  (setq fix_count 0)
;  (setq poly (ssname (ssget pt1 '((0 . "POLYLINE"))) 0))
  (setq indices (get_index_pts pt1 pt2 pt3 poly))
    
  (if (/= (* (- (nth 0 indices) (nth 1 indices)) (- (nth 0 indices) (nth 1 indices))) 1)
    (progn
      (fliping_MN poly)
      (setq poly (entlast))
      (vl-cmdf "chprop" (entlast) "" "la" poly_layer "")
      (setq fix_count (+ fix_count 1))
    )
  )
  
  (setq indices (get_index_pts pt1 pt2 pt3 poly))
  (if (and 
    (> (nth 0 indices) (nth 1 indices))
    (> (nth 1 indices) (nth 2 indices)))
    (progn
      (hw_reverse_mesh poly)
      (vl-cmdf "chprop" (entlast) "" "la" poly_layer "")
      (setq fix_count (+ fix_count 1))
    )
  )
    
  (if (and 
    (> (nth 0 indices) (nth 1 indices))
    (< (nth 1 indices) (nth 2 indices)))
    (progn
      (zigzag poly)
      (vl-cmdf "chprop" (entlast) "" "la" poly_layer "")
      (setq fix_count (+ fix_count 1))
    )
  )
    
  (if (and 
    (< (nth 0 indices) (nth 1 indices))
    (> (nth 1 indices) (nth 2 indices)))
    (progn 
      (hw_reverse_mesh poly)
      (setq poly (entlast))
      (zigzag poly)
      (vl-cmdf "chprop" (entlast) "" "la" poly_layer "")
      (setq fix_count (+ fix_count 1))
    )
  )
  (if (> fix_count 0) (setq poly (entlast)))
  
  (princ "\nSET MESH: Complete. ")
  poly
)
        
        
        
        
        