;; Command: HW Zap Mesh
;; Author: Andrew Bentzen 
;; Company: Haworth
;; Description: Removes a facet from a polygon mesh. 
;; Input: 2 vertexes on a polygon mesh. 
;; Output: 1 polygon mesh entity (with one of it's facets removed). 
(defun c:hwzapmesh( / pt1 pt2)
  (vl-cmdf "ucs" "world")
  (setq pt1 (getpoint "\nZAP MESH: Select first point. "))
  (setq pt2 (getpoint "\nZAP MESH: Select second point. "))
  (zapmesh pt1 pt2 nil nil)
  (vl-cmdf "ucs" "previous" "")
  
  (princ)
)


(defun zapmesh(pt1 pt2 seam_flag draw_mesh_flag / lst1 doc m_section meshobj_lst1 poly_layer 
                vtx match index_pt2 index_pt1 poly acadobj elst del_index
                n m i)


  ;; error handler to reset user settings if they hit esc 
  (defun *error*(msg)
    (if (/= sel_prev nil) (setvar "selectionpreview" sel_prev))
    (if (not (member msg '("Function cancelled" "quit / exit abort" 
                           "bad argument type: VLA-OBJECT nil")))
      (princ (strcat "\nError: " msg))
    )
;    (vl-cmdf "ucs" "previous" "")
    ;; end of undo marker 
    (vla-EndUndoMark doc)
    (princ)
  )
  
  (vl-load-com)
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  
  ;; set an undo marker. the end undo marker will be at the end of the entire script
  (vla-StartUndoMark doc)
  
;  (setq pt1 (trans pt1 0 1))
;  (setq pt2 (trans pt2 0 1))
  
  (setq poly (ssname (ssget pt1 '((0 . "POLYLINE"))) 0))
  
  (if (= (cdr (assoc 0 (entget poly))) "POLYLINE")
    (progn
      (setq vtx (entnext poly))
      (while (/= (cdr (assoc 0 (setq elst (entget vtx)))) "SEQEND")
        (setq lst1 (cons (cdr (assoc 10 elst)) lst1))
        (setq vtx (entnext vtx))
      )
    )
  )
  
;  (setq lst1 (trans lst1 0 1))
  

  (setq meshObj_lst1 (vlax-ename->vla-object poly))
  (setq m (vla-get-MVertexCount meshObj_lst1))
  (setq n (vla-get-NVertexCount meshObj_lst1))
  
  (setq index_pt1 (vl-position T (mapcar '(lambda (x) (equal x pt1 0.0001)) (reverse lst1))))
  (setq index_pt2 (vl-position T (mapcar '(lambda (x) (equal x pt2 0.0001)) (reverse lst1))))
  
;  (princ "\nindex_pt1") (princ index_pt1)
;  (princ "\nindex_pt2") (princ index_pt2)
;  (princ "\nlst1") (princ lst1)
;  (princ "\npt1") (princ pt1)
;  (princ "\npt2") (princ pt2)
  
  
  (setq i 0)
  (while (or (/= (* (- index_pt1 index_pt2) (- index_pt1 index_pt2)) 1) (> i 3))
    (if (= (rem i 2) 0) (setq lst1 (reverse lst1)))
    (if (= (rem i 4) 0) (setq lst1 (zig_zag_zapmesh lst1 m)))
    (if (= (rem i 8) 0)
      (progn
        (setq poly (flip_mn_zapmesh poly))
        (zapmesh pt1 pt2 seam_flag draw_mesh_flag)

;        (setq lst1 nil)
;        (if (= (cdr (assoc 0 (entget poly))) "POLYLINE")
;          (progn
;            (setq vtx (entnext poly))
;            (while (/= (cdr (assoc 0 (setq elst (entget vtx)))) "SEQEND")
;              (setq lst1 (cons (cdr (assoc 10 elst)) lst1))
;              (setq vtx (entnext vtx))
;            )
;          )
;        )
;        (setq meshObj_lst1 (vlax-ename->vla-object poly))
;        (setq m (vla-get-MVertexCount meshObj_lst1))
;        (setq n (vla-get-NVertexCount meshObj_lst1))
;        (setq index_pt1 (vl-position T (mapcar '(lambda (x) (equal x pt1 0.00000000001)) (reverse lst1))))
;        (setq index_pt2 (vl-position T (mapcar '(lambda (x) (equal x pt2 0.00000000001)) (reverse lst1))))
      )
    )
    (setq index_pt1 (vl-position T (mapcar '(lambda (x) (equal x pt1 0.00000000001)) lst1)))
    (setq index_pt2 (vl-position T (mapcar '(lambda (x) (equal x pt2 0.00000000001)) lst1)))
    
    (setq i (+ i 1))
  )
  
  (if (= (* (- index_pt1 index_pt2) (- index_pt1 index_pt2)) 1)
    (setq match T)
  )
  
    
  (if (= match T)
    (progn
;      (setq m_section (/ (+ index_pt1 1) n))
      (setq m_section (/ index_pt1 n))
;      (setq m_section (+ m_section 1))
;      (setq m_section m_section)
      (setq del_index (* m_section n))
    )
    (princ "\nZAP MESH: Unable to zap the mesh. ")
  )
  
  (setq lst1 (reverse lst1))
  
  (if (= seam_flag nil)
    (progn
      (setq i 0)
      (while (< i n)
        (setq lst1 (removenth del_index lst1))
        (setq i (+ i 1))
      )
  
      (setq poly_layer (cdr (assoc 8 (entget poly))))
      (set3DMesh_zapmesh lst1 n)
      (vl-cmdf "chprop" (entlast) "" "la" poly_layer "")
      (entdel poly)
  
      ;; end of undo marker 
      (vla-EndUndoMark doc)
      (princ "\nZAP MESH: Complete. ")
      (princ)
    )
    
    (progn
      (setq seam_lst nil)
      (setq i 0)
      (if (> (- del_index n) 0)
        (progn
          (setq del_index (- del_index n))
          (while (< i (* n 3))
            (setq seam_lst (cons (nth (+ del_index i) lst1) seam_lst))
            (setq i (+ i 1))
          )
        )
        
        (progn
          (while (< i n)
             (setq seam_lst (cons (nth (+ del_index i) lst1) seam_lst))
             (setq i (+ 1 i))
          )
          (setq i 0)
          (while (< i (* n 2))
            (setq seam_lst (cons (nth (+ del_index i) lst1) seam_lst))
            (setq i (+ i 1))
          )
        )
      )
      
      (setq seam_lst (reverse seam_lst))
      (setq i n)
      (while (<= i (- (- (length seam_lst) n) 1))
        (if (= i 0) (setq pt1 (nth i seam_lst)) (setq pt1 (nth (- i 1) seam_lst)))
        (setq pt2 (nth i seam_lst))
        (setq pt3 (nth (+ i 1) seam_lst))
        (if (< i n) (setq pt4 pt2) (setq pt4 (nth (- i n) seam_lst)))
        (setq pt5 (nth (+ i n) seam_lst))

        (if (= (rem i n) 0) (setq pt1 pt2))
        (if (= (rem (+ i 1) n) 0) (setq pt3 pt2))
        (if (>= (+ i n) (* n m)) (setq pt5 pt2))
    
        (vl-cmdf "ucs" "world")
        (if (= draw_mesh_flag "y")
          (seam_v2 pt1 pt2 pt3 pt4 pt5 T)
          (seam_v1 pt1 pt2 pt3 pt4 pt5)
        )
        (setq i (+ i 1))
      )
      (princ "\nFLAG HERE<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
    )
  )
)

(defun removenth(n l / )
  (if (and l (< 0 n))
    (cons (car l) (removenth (1- n) (cdr l)))
    (cdr l)
  )
)

(defun zig_zag_zapmesh(lst m / n container1 container2 x count count2)
  ;; changes the direction of each individual N segment within the mesh. 
  (setq count 0)
  (setq count2 0)
  
  (setq n m)
  (setq m (/ (length lst) n))

  (while (/= m count2)
    (while (/= n count)
      (setq container1 (cons (car lst) container1))
      (setq lst (cdr lst))
      (setq count (+ count 1))
    )
 
    (setq container2 (cons (reverse container1) container2))
    (setq count2 (+ count2 1))
    (setq count 0)
    (setq container1 nil)
  )
  
  (while (/= container2 nil)
    (setq x (car container2))
    (while (/= x nil)
      (setq lst (cons (car x) lst))
      (setq x (cdr x))
    )
    (setq container2 (cdr container2))
  )
  lst
)


(defun set3DMesh_zapmesh(lst nSize / mSize new_pt_lst acadObj doc points modelSpace meshObj)
  (setq mSize (/ (length lst) nSize))
  
  (setq new_pt_lst (apply 'append lst))
  
  (vl-load-com)
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))

  ;; Create the matrix of points
  (setq points (vlax-make-safearray vlax-vbDouble (cons 0 (- (length new_pt_lst) 1))))

    
  (vlax-safearray-fill points new_pt_lst)
    
  ;; creates a 3Dmesh in model space
  (setq modelSpace (vla-get-ModelSpace doc))
  (setq meshObj (vla-Add3DMesh modelSpace mSize nSize points))
  
)


(defun flip_mn_zapmesh(poly_to_fix / lst vtx elst meshObj currVertexCount_M currVertexCount_N m_copy n_copy m n new_pt_lst acadObj doc points modelSpace poly_fixed)
  ;; Swaps the horizontal and vertical segmentation of the mesh. 
  ;; It was really hard to figure out how to do this!
  
  (setvar "meshtype" 0)
  (if (= (cdr (assoc 0 (entget poly_to_fix))) "POLYLINE")
    (progn
      ;; Gets pointlist of the mesh entity that needs to be corrected. 
      (setq vtx (entnext poly_to_fix))
      (while (/= (cdr (assoc 0 (setq elst (entget vtx)))) "SEQEND")
        (setq lst (cons (cdr (assoc 10 elst)) lst)
          vtx (entnext vtx)
        )
      )
      (setq meshObj (vlax-ename->vla-object poly_to_fix))
      (setq currVertexCount_M (vla-get-MVertexCount meshObj))
      (setq currVertexCount_N (vla-get-NVertexCount meshObj))
    )
  )
  (if (= (cdr (assoc 0 (entget poly_to_fix))) "3DFACE")
    (progn
      (setq lst (face_to_pt_list poly_to_fix))
      (setq meshObj (vlax-ename->vla-object poly_to_fix))
      (setq currVertexCount_M 2)
      (setq currVertexCount_N 2)
    )
  )
  
  (setq m currVertexCount_M)
  (setq n currVertexCount_N)
  
  (setq m_copy m)
  (setq n_copy n)

  (setq n (/ (length lst) m))
  (setq lst (flippy_zapmesh lst m n))
  (setq n m_copy)
  (setq m n_copy)
  
  (setq new_pt_lst (apply 'append lst))

  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))

  ;; Create the matrix of points
  (setq points (vlax-make-safearray vlax-vbDouble (cons 0 (- (length new_pt_lst) 1))))

  (vlax-safearray-fill points new_pt_lst)
    
  ;; creates a 3Dmesh in model space
  (setq modelSpace (vla-get-ModelSpace doc))
  (setq meshObj (vla-Add3DMesh modelSpace m n points))
  
  (setq poly_fixed (vlax-vla-object->ename meshObj))
  (entdel poly_to_fix)
  poly_fixed
)

(defun flippy_zapmesh(lst m n / y i j lst_modified lst_copy lst_reset)  
  (setq y n)
  
  (setq i m)
  (setq j n)
  
  (setq lst_copy lst)
  (setq lst_reset lst)

  (while (> j 0)
    (while (> i 0)
      (setq lst_modified (cons (nth 0 lst_copy) lst_modified))
      (while (> y 0) (setq lst_copy (cdr lst_copy)) (setq y (- y 1)))
      (setq y n)
      (setq i (- i 1))
    )
    (setq i m) ;; reset the incrementer 
    (setq lst_reset (cdr lst_reset))
    (setq lst_copy lst_reset)
    (setq j (- j 1))
  )
  lst_modified
)
