;; Command: HW Combine Mesh
;; Author: Andrew Bentzen 
;; Company: Haworth
;; Description: Returns a single, combined polygon mesh when the user clicks 2 adjacent polygon meshes.
;; Input: 2 adjacent polygon mesh entities 
;; Output: 1 combined mesh 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <INPUT FUNCTION>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this function allow you to enter combine mesh by gathering 
;;; input from the command line/ AutoCAD environment.
(defun c:hwcombinemesh( / acadObj doc meshes mesh1 mesh2 is_ready_to_combine i
                          ent1 ent1_type m1 n1 ent2 ent2_type m2 n2 m n lst 
                          lst1 lst2 source_layer)

  ;; get input from the user and validate.
  ;; return value: [ [ent1, ent1_type, m-count, n-count], [ent2, ent2_type m-count, n-count] ]
  (setq meshes (get_input_meshes))
  
  (setq ent1 (nth 0 (nth 0 meshes)))
  (setq ent1_type (nth 1 (nth 0 meshes)))
  (setq m1 (nth 2 (nth 0 meshes)))
  (setq n1 (nth 3 (nth 0 meshes)))
  
  (setq ent2 (nth 0 (nth 1 meshes)))
  (setq ent2_type (nth 1 (nth 1 meshes)))
  (setq m2 (nth 2 (nth 1 meshes)))
  (setq n2 (nth 3 (nth 1 meshes)))
  
  ;; get the source layer. use this for newly created, combined polygon mesh
  (setq source_layer (cdr (assoc 8 (entget ent1))))
  
  ;; validation for if user clicks same mesh twice
  (while (equal ent1 ent2)
    (princ "\nCOMBINE MESH: Cannot combine mesh with itself. ")
    (setq meshes (get_input_meshes))
  )
  ;; validation for if m- or n-counts don't match up 
  (while (not (or 
           (= m1 m2)
           (= m1 n2)
           (= n1 m2)
           (= n1 n2)
         ))
    (princ "\nCOMBINE MESH: Cannot combine meshes. N- or M-counts are unequal. ")
    (setq meshes (get_input_meshes))
  )
  (combinemesh ent1 ent2)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; </INPUT FUNCTION>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <MAIN FUNCTION>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this function allow you to enter combine mesh from another 
;;; LISP function (rather than directly from the command line/AutoCAD environment 
(defun combinemesh(ent1 ent2 / acadObj doc meshes mesh1 mesh2 is_ready_to_combine i
                   ent1 ent1_type m1 n1 ent2 ent2_type m2 n2 m n lst 
                   lst1 lst2 source_layer)
  (vl-load-com)
  
  ;; error handler to reset user settings if they hit esc 
  (defun *error*(msg)
    ;; end of undo marker 
    (vla-EndUndoMark doc)
    (princ)
  )

  ;; set undo marker
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  (vla-StartUndoMark doc)

  (setq ent1_type (cdr (assoc 0 (entget ent1)))) ;; dxf 0
  (setq m1 (cdr (assoc 71 (entget ent1))))  ;; dxf 71
  (setq n1 (cdr (assoc 72 (entget ent1))))  ;; dxf 72
  (setq ent2_type (cdr (assoc 0 (entget ent2))))
  (setq m2 (cdr (assoc 71 (entget ent2))))
  (setq n2 (cdr (assoc 72 (entget ent2))))
  
  ;; get the point list for ent1 and ent2
  (setq lst1 (ent_to_point_list ent1))
  (setq lst2 (ent_to_point_list ent2))

  ;; check if lists line up 
  (setq is_ready_to_combine (is_aligned_point_list lst1 m1 n1 lst2 m2 n2))
  
  ;; loop through the (1) reverse, (2) reverse_n_segments, and (3) flip_m_n_direction 
  ;;; functions until the resulting list passes the is_ready_to_combine test.
  (setq i 1)
  (while (and (< i 512) (not is_ready_to_combine))
    ;; every other time through the loop: reverse the list for mesh 1 
    (if (= (rem i 2) 0)  (setq lst1 (reverse lst1)))
    (if (= (rem i 4) 0)  (setq lst2 (reverse lst2)))
    
    (if (= (rem i 8) 0)  (setq lst1 (reverse_n_segments lst1 m1 n1)))
    (if (= (rem i 16) 0) (setq lst2 (reverse_n_segments lst2 m2 n2)))
    
    (if (= (rem i 32) 0)
      (progn 
        (setq lst1 (flip_m_n_direction lst1 m1 n1))
        ;; swap the m- and n-counts every time you run flip_m_n_direction
        (setq m m1)
        (setq n n1)
        (setq m1 n)
        (setq n1 m)
      )
    )
    (if (= (rem i 64) 0)
      (progn 
        (setq lst2 (flip_m_n_direction lst2 m2 n2))
        ;; swap the m- and n-counts every time you run flip_m_n_direction
        (setq m m2)
        (setq n n2)
        (setq m2 n)
        (setq n2 m)
      )
    )
    
    ;; check if lists line up 
    (setq is_ready_to_combine (is_aligned_point_list lst1 m1 n1 lst2 m2 n2))
    
    (setq i (+ i 1))
 )
  
  
  ;; create a new mesh from the two source meshes. 
  (if is_ready_to_combine 
    (progn
      ;; n-count for new combined mesh needs to be the sum of the two source 
      ;;; mesh n-counts minus 1
      (setq m (- (+ m1 m2) 1))
      
      ;; m-count for the new combined mesh will take the same source mesh's m-count.
      (setq n n1)
      
      
      ;; clean up the source point lists by deleting the duplicate points
      ;;; between them. 
      (setq lst (destroy_duplicate_points lst1 lst2 n))
 
      (list_to_polygon_mesh lst m n)
      (command "chprop" (entlast) "" "layer" source_layer "") ;; change new mesh to be on source_layer
      
      ;; delete the source meshes
      (if (entget ent1) (entdel ent1))
      (if (entget ent2) (entdel ent2))
      (princ "\nCOMBINE MESH: Complete. ")
    )
    (progn
      (princ "\nCOMBINE MESH: Unable to combine meshes. ")
    )
  )
  
  ;; end of undo marker 
  (vla-EndUndoMark doc)
  (princ)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; </MAIN FUNCTION>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <SUPPORTING FUNCTIONS>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; function to take in an validate user input
;;;; validate for polygon mesh / 3Dface input
;;;; validate for when user clicks same mesh twice
;;;; validate for when user misses click
;;;; validate for whether there is a case where the N count 
;;;;; and M count match
(defun get_input_meshes( / sel_prev ent1 ent2)
  (setq ent1 nil)
  (setq ent2 nil)
  (setq sel_prev (getvar "selectionpreview"))
  (setvar "selectionpreview" 2)
  (while (not ent1)
    (setq ent1 (cons (car (entsel (strcat "\nCOMBINE MESH: Select your first mesh. "))) ent1))
    (if (not (car ent1)) (setq ent1 nil))
  )
  
  ;; make ent1 into a list and add its type and m/n counts
  (setq ent1 (cons (cdr (assoc 0 (entget (last ent1)))) ent1))
  (if (= (nth 0 ent1) "POLYLINE")
    (progn
      (setq ent1 (cons (cdr (assoc 71 (entget (last ent1)))) ent1))
      (setq ent1 (cons (cdr (assoc 72 (entget (last ent1)))) ent1))))
  
  ;; force m- and n-counts to be 2 if the entity is a 3Dface 
  ;;; a 3Dface will always have a 2x2 point list.
  (if (= (nth 0 ent1) "3DFACE")
    (progn
      (setq ent1 (cons 2 ent1))
      (setq ent1 (cons 2 ent1))))
  (setq ent1 (reverse ent1))
  (while (not ent2)
    (setq ent2 (cons (car (entsel (strcat "\nCOMBINE MESH: Select your second mesh. "))) ent2))
    (if (not (car ent2)) (setq ent2 nil))
  )
  
  ;; make ent2 into a list and add its type and m/n counts
  (setq ent2 (cons (cdr (assoc 0 (entget (nth 0 ent2)))) ent2))
  (if (= (nth 0 ent2) "POLYLINE")
    (progn
      (setq ent2 (cons (cdr (assoc 71 (entget (last ent2)))) ent2))
      (setq ent2 (cons (cdr (assoc 72 (entget (last ent2)))) ent2))))
      
  ;; force m- and n-counts to be 2 if the entity is a 3Dface 
  ;;; a 3Dface will always have a 2x2 point list.
  (if (= (nth 0 ent2) "3DFACE")
    (progn
      (setq ent2 (cons 2 ent2))
      (setq ent2 (cons 2 ent2))))
  (setq ent2 (reverse ent2))
  
  (setvar "selectionpreview" sel_prev)
  
  ;; return value: [ [ent1, ent1_type, m-count, n-count], [ent2, ent2_type m-count, n-count] ]
  (list ent1 ent2)
)

;; get point list from a polygon mesh or a 3D face
(defun ent_to_point_list(ent / vtx lst elst)
  
  (if (= (cdr (assoc 0 (entget ent))) "POLYLINE")
    (progn
      (setq vtx (entnext ent))
      (while (/= (cdr (assoc 0 (setq elst (entget vtx)))) "SEQEND")
        (setq lst (cons (cdr (assoc 10 elst)) lst))
        (setq vtx (entnext vtx))
      )
    )
  )
  
  (if (= (cdr (assoc 0 (entget ent))) "3DFACE")
    (setq lst (list 
                (cdr (assoc 10 (entget ent)))
                (cdr (assoc 11 (entget ent)))
                (cdr (assoc 13 (entget ent)))
                (cdr (assoc 12 (entget ent)))
              )
    )
  )
  ;; return value 
  lst
)


;; check if the point lists that make up the meshes line up 
;;; with each other. check by dividing the point lists by 
;;; both the n- and the m-counts
;; returns boolean
;; input: pt_lst1, m-ct1, n-ct1, pt_lst2, m-ct2, n-ct2
(defun is_aligned_point_list(lst1 m1 n1 lst2 m2 n2 / i is_aligned is_same_point_lst
                             lst1_m_back lst1_n_back lst2_m_front lst2_n_front)
  
  ;; returns boolean
  ;; true if the points in each point list are identical or very close 
  (defun is_same_point_lst(pt_lst1 pt_lst2 / i flag_lst flag)
    (setq flag nil)
    (setq i 0)
    (while (< i (length pt_lst1))
      (setq flag_lst (cons (equal (nth i pt_lst1) (nth i pt_lst2) 0.0001) flag_lst))
      (setq i (+ i 1))
    )
    (setq flag (apply 'and flag_lst))
    flag
  )
  
  (setq is_aligned nil)
  
  ;; relevant portions of each point list is the first and the  
  ;;; last n-th and m-th points in the list
  
  ;; initialize point lists
  (setq lst1_m_back  nil)
  (setq lst1_n_back  nil)
  
  (setq lst2_m_front nil)
  (setq lst2_n_front nil)
  
  ;; set point lists
  (setq i 0)
  (while (< i m1) (setq lst1_m_back  (cons (nth i (reverse lst1)) lst1_m_back)) (setq i (+ i 1)) )
  (setq lst1_m_back (reverse lst1_m_back))
  (setq i 0)
  (while (< i n1) (setq lst1_n_back  (cons (nth i (reverse lst1)) lst1_n_back)) (setq i (+ i 1)) )
  (setq lst1_n_back (reverse lst1_n_back))
  (setq i 0)
  (while (< i m2) (setq lst2_m_front (cons (nth i lst2) lst2_m_front)) (setq i (+ i 1)) )
  (setq i 0)
  (while (< i n2) (setq lst2_n_front (cons (nth i lst2) lst2_n_front)) (setq i (+ i 1)) )
  
  ;; check back m of list 1 with front m of list 2
  (setq is_aligned (cons (is_same_point_lst lst1_m_back lst2_m_front) is_aligned))

  ;; check back n of list 1 with front n of list 2
  (setq is_aligned (cons (is_same_point_lst lst1_n_back lst2_n_front) is_aligned))
  
  ;; if any of the above return true, then the point list is good and ready to be combined
  (setq is_aligned (apply 'or is_aligned))
  
  is_aligned 
)


;; changes the direction of each individual N segment within the mesh. 
(defun reverse_n_segments(lst m n / result i j )
  ;; make 2D list by dividing the list into n segments. 
  (setq result nil)
  (setq i 0)
  (while (< i (length lst))
    (repeat n 
      (setq n_segment (cons (nth i lst) n_segment))
      (setq i (+ i 1))
    )
    (setq result (cons n_segment result))
    (setq n_segment nil)
  )
  (setq result (reverse result))
  (setq result (apply 'append result))

  ;; return value
  result 
)

;; function that will transpose a 2D list (rows become columns, columns become rows)
(defun flip_m_n_direction(lst m n / result i j )
  (setq i 0)
  (setq j 0)
  (repeat n
    (while (< i (length lst))
      ;; go through the whole list
      (if (= (rem i n) j)
        (setq result (cons (nth i lst) result))
      )
      (setq i (+ i 1))
    )
    (setq i (- (length lst)))
    (setq j (+ j 1))
  )
  (setq result (reverse result))
  ;; return value
  result 
)


;; Gets rid of the duplicate points left over in the middle when lining up the 
;;; meshes end to end.  
(defun destroy_duplicate_points(lst1 lst2 n_value / lst_new )
  (while (/= n_value 0)
    (setq lst2 (cdr lst2))
    (setq n_value (- n_value 1))
  )
  (setq lst_new (append lst1 lst2))
  lst_new
)

;; void function that takes 1 point list and turns them into a single 
;;; polygon mesh. 
(defun list_to_polygon_mesh(coords_list m n / polygonmesh_header polygonmesh_vertex
                            ent_seqend)
  (defun polygonmesh_header (m n / )
    (entmake (list '(0 . "POLYLINE")
                   '(100 . "AcDbEntity")
                   '(100 . "AcDbPolygonMesh")
                   '(70 . 16)
                   (cons 71 m)
                   (cons 72 n)))
  )
  
  (defun polygonmesh_vertex (xyz / )
    (entmake (list '(0 . "VERTEX")
                   '(100 . "AcDbEntity")
                   '(100 . "AcDbVertex")
                   '(100 . "AcDbPolygonMeshVertex")
                   (cons 10 xyz)
                   '(70 . 64)))
  )
  
  (defun ent_seqend ( / )
    (entmake (list '(0 . "SEQEND")
                   '(100 . "AcDbEntity")))
  )


  (polygonmesh_header m n)
  (foreach pt (reverse coords_list) (polygonmesh_vertex pt))
  (ent_seqend)
  (entlast)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; </SUPPORTING FUNCTIONS>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
