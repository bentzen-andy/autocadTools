;; Command: HW Cut Mesh
;; Author: Andrew Bentzen 
;; Company: Haworth
;; Description: Click 2 points on a polygon mesh and the command will cut it along that line. 
;; Input: 2 adjacent vertexes of a polygon mesh entity. 
;; Output: Returns two adjacent polygon meshes. 

(defun c:hwcutmesh( / poly pt1 pt2)
  
  (setq poly (car (entsel "\nCUT MESH: Select mesh. ")))
  (setq pt1 (getpoint "\nCUT MESH: Select first point. "))
  (setq pt2 (getpoint "\nCUT MESH: Select second point. "))
  (cutmesh poly pt1 pt2)
  (setq *CUT_COMPLETE* nil)
  (princ)
)

(defun cutmesh(poly pt1 pt2 / lst1 doc index_full_m_section m_section lst_right 
               meshobj_lst1 poly_layer vtx index_lst_left lst_left 
               index_lst_right index_pt2 index_pt1 poly poly2 acadobj elst 
               index_cut n m)
  ;; error handler to reset user settings if they hit esc 
  (defun *error*(msg)
    (if (/= sel_prev nil) (setvar "selectionpreview" sel_prev))
    (setvar "cmdecho" 1)
    (if (not (member msg '("Function cancelled" "quit / exit abort" 
                           "bad argument type: VLA-OBJECT nil")))
      (princ (strcat "\nError: " msg))
    )
    
    ;; end of undo marker 
    (vla-EndUndoMark doc)
    (princ)
  )
    
  (vl-load-com)
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  
  ;; set an undo marker. the end undo marker will be at the end of the entire script
  (vla-StartUndoMark doc)
  
  (vl-cmdf "ucs" "world")
  
  (setvar "cmdecho" 0)
  
;  (setq poly (ssname (ssget pt1 '((0 . "POLYLINE"))) 0))
;  (setq poly (ssname (ssget "_C" pt1 (polar pt4 (* (/ 0.001 180.0) pi) 0.01) '((0 . "POLYLINE")))))
  
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
  
  (if (= (abs (- index_pt1 index_pt2)) n)
    (progn (setq poly2 (flip_mn_cutmesh poly)) (setq poly poly2) (cutmesh poly pt1 pt2))
  )

  (if (= (abs (- index_pt1 index_pt2)) 1)
    (progn 
      (setq m_section (/ (+ index_pt1 1) n))
      (setq index_cut (* m_section n))
      (setq index_full_m_section (mk_index_lst n))
      (setq index_full_m_section (mapcar '(lambda(x) (+ index_cut x)) index_full_m_section))
      
      (setq index_lst_left (mk_index_lst index_cut))
      (setq index_lst_left (append index_full_m_section index_lst_left))
      
      (setq index_lst_right (mk_index_lst (- (* m n) index_cut)))
      (setq index_lst_right (mapcar '(lambda(x) (+ index_cut x)) index_lst_right))
      
;      (princ "\nindex_lst_left ") (princ index_lst_left) (princ "\n") (princ (length index_lst_left))
;      (princ "\nindex_lst_right ") (princ index_lst_right) (princ "\n") (princ (length index_lst_right))
      
      (setq lst_left (index_to_pt_lst index_lst_left (reverse lst1)))
      (setq lst_right (index_to_pt_lst index_lst_right (reverse lst1)))
;      (princ "\nlst_left ") (princ lst_left) (princ "\n") (princ (length lst_left))
;      (princ "\nlst_right ") (princ lst_right) (princ "\n") (princ (length lst_right))
      (setq poly_layer (cdr (assoc 8 (entget poly))))
  
      (set3DMesh_cutmesh lst_left (+ m_section 1))
      (vl-cmdf "chprop" (entlast) "" "la" poly_layer "")
  
      (set3DMesh_cutmesh lst_right (- m m_section))
      (vl-cmdf "chprop" (entlast) "" "la" poly_layer "")
  
      (if (entget poly) (entdel poly))
      
      (setq *CUT_COMPLETE* T)
      (princ "\nCUT MESH: Complete. ")
      (princ)
    )
    (progn
      (if (= *CUT_COMPLETE* nil) (princ "\nCUT MESH: Unable to cut the mesh. "))
      (princ)
    )
  )
  (vl-cmdf "ucs" "prev")
  (setvar "cmdecho" 1)
  ;; end of undo marker 
  (vla-EndUndoMark doc)
  (princ)
  
)

(defun mk_index_lst(num / i lst)
  (setq i 0)
  (while (< i num)
    (setq lst (cons i lst))
    (setq i (+ i 1))
  )
  lst
)

(defun index_to_pt_lst(index_lst pt_lst / pt_lst_new)
  (while (/= index_lst nil)
    (setq pt_lst_new (cons (nth (car index_lst) pt_lst) pt_lst_new))
    (setq index_lst (cdr index_lst))
  )
  pt_lst_new
)

(defun removenth_cutmesh(n l / )
  (if (and l (< 0 n))
    (cons (car l) (removenth (1- n) (cdr l)))
    (cdr l)
  )
)

(defun set3DMesh_cutmesh(lst mSize / nSize new_pt_lst acadObj doc points modelSpace meshObj)
  (setq nSize (/ (length lst) mSize))
  
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


(defun flip_mn_cutmesh(poly_to_fix / lst vtx elst meshObj currVertexCount_M currVertexCount_N m_copy n_copy m n new_pt_lst acadObj doc points modelSpace poly_fixed)
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
  (setq lst (flippy_cutmesh lst m n))
  (setq n m_copy)
  (setq m n_copy)
  
  (setq new_pt_lst (apply 'append lst))

  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))

  ;; Create the matrix of points
  (setq points (vlax-make-safearray vlax-vbDouble (cons 0 (- (length new_pt_lst) 1))))

  (vlax-safearray-fill points new_pt_lst)
    
  ;; creates a 3Dmesh in model space
  (setq poly_layer (cdr (assoc 8 (entget poly_to_fix))))
  (setq modelSpace (vla-get-ModelSpace doc))
  (setq meshObj (vla-Add3DMesh modelSpace m n points))
  (vl-cmdf "chprop" (entlast) "" "la" poly_layer "")
  
  (setq poly_fixed (vlax-vla-object->ename meshObj))
  (entdel poly_to_fix)
  poly_fixed
)

(defun flippy_cutmesh(lst m n / y i j lst_modified lst_copy lst_reset)  
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
