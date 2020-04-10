;; Command: HW Draw Table Top
;; Author: Andrew Bentzen 
;; Company: Haworth
;; Description: Draws several 3D faces all at once. Useful if you have a big polygon mesh entity (like 
;;; for the perimeter of a table edge). This command will read in the perimeter points of the polygon 
;;; mesh and use those points as the connection points for the 3D faces. 

;;; Upon using the command, you will be prompted to click 3 adjacent points in and "L" shape the polygon mesh. 
;; Input: 3 adjacent vertexes of a polygon mesh entity. 
;; Output: A ton of 3D faces. 

(defun c:hwdrawtabletop_v1( / lst poly1 vtx elst meshObj_lst1 currVertexCount n m 
                              count poly_fixed meshObj_lst_fixed currVertexCount_fixed
                              pt1 pt2 pt3)
  (vl-load-com)
  (if (null setmesh) (load "C:\\hw_commands\\lisp\\hwsetmesh.lsp"))
  
  ;; set an undo marker. the end undo marker will be at the end of the entire script
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  (vla-StartUndoMark doc)

  (defun *error*(msg)
    (entdel my_surface)
    (if (/= sel_prev nil) (setvar "selectionpreview" sel_prev))
    (if (not (member msg '("Function cancelled" "quit / exit abort" 
                           "bad argument type: VLA-OBJECT nil")))
      (princ (strcat "\nError: " msg))
    )
    ;; end of undo marker 
    (vla-EndUndoMark doc)
    (princ)
  )
  
  (setq poly1 (car (entsel "\nDRAW TABLE TOP: Select a mesh to add 3D faces to it. ")))
  
  (setq pt1 (getpoint "\nDRAW TABLE TOP: Select 3 points on the mesh in an L-shape (1/3): "))
  (setq pt2 (getpoint "\nDRAW TABLE TOP: Select 3 points on the mesh in an L-shape (2/3): "))
  (setq pt3 (getpoint "\nDRAW TABLE TOP: Select 3 points on the mesh in an L-shape (3/3): "))
  (setq poly1 (setmesh poly1 pt1 pt2 pt3))
  
  (setq vtx (entnext poly1))
  (while (/= (cdr (assoc 0 (setq elst (entget vtx)))) "SEQEND")
    (setq lst (cons (cdr (assoc 10 elst)) lst)
      vtx (entnext vtx)
    )
  )
  
  (setq meshObj_lst1 (vlax-ename->vla-object poly1))
  (setq currVertexCount (vla-get-NVertexCount meshObj_lst1))
  
  (setq n currVertexCount)
  (setq m (/ (length lst) n))

;  (if (> n m)
;    (progn
;      (setq poly_fixed (flip_mn poly1))
;  
;      (setq meshObj_lst_fixed (vlax-ename->vla-object poly_fixed))
;      (setq currVertexCount_fixed (vla-get-NVertexCount meshObj_lst_fixed))
;  
;      (setq n currVertexCount_fixed)
;      (setq m (/ (length lst) n))
;      
;      (setq n m)
;      (setq m n)
;    )
;  )

  ;; the count variable needs to be the length of the list of perimeter points minus 4 divided by 2
  ;; this is probably more contrived than it needs to be but that's the result I need to make this
  ;; work with my preveously written code. 
  (setq count (/ (length lst) n))
  (setq count (- count 4))
  (setq count (/ count 2))
  (setq *original_count* count)
  
  (setq lst (correction_for_pt_lst lst))
  (set_3d_faces lst)
  (setq *original_count* nil)
  
  ;; end of undo marker 
  (vla-EndUndoMark doc)
  (princ "\nDRAW TABLE TOP: Complete.")
  (princ)
)

(defun correction_for_pt_lst(lst / lst2 i)
  (setq lst2 nil)
  (setq lst (reverse lst))
  (while (/= lst nil)
    (setq lst2 (cons (car lst) lst2))
    (setq i 0)
    (while (/= i n)
      (setq lst (cdr lst))
      (setq i (+ i 1))
    )
  )
  (setq lst2 (reverse lst2))
)

(defun set_3d_faces(lst / ent_lst acadObj doc modelSpace faceObj)
  (vl-load-com)
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  
  ;; Create the 3DFace object in model space
  (setq modelSpace (vla-get-ModelSpace doc))
  (setq faceObj (vla-Add3DFace modelSpace (vlax-3d-point (nth 0 lst))
                                          (vlax-3d-point (nth 1 lst))
                                          (vlax-3d-point (nth 1 (reverse lst)))
                                          (vlax-3d-point (nth 0 (reverse lst)))))
  (vla-put-VisibilityEdge2 faceObj :vlax-false)
  (vla-put-VisibilityEdge4 faceObj :vlax-false)
  
  (if (= (cdr(assoc 0 (entget (entlast)))) "3DFACE") (setq ent_lst (cons (entget (entlast)) ent_lst)))
  
  (if (= (rem (length lst) 2) 1) (setq count (+ count 1)))
  (while (> count 0)
    (setq faceObj (vla-Add3DFace modelSpace (vlax-3d-point (nth (+ count 0) lst))
                                            (vlax-3d-point (nth (+ count 1) lst))
                                            (vlax-3d-point (nth (+ count 1) (reverse lst)))
                                            (vlax-3d-point (nth (+ count 0) (reverse lst)))))
    (vla-put-VisibilityEdge2 faceObj :vlax-false)
    (vla-put-VisibilityEdge4 faceObj :vlax-false)

    (if (= (cdr(assoc 0 (entget (entlast)))) "3DFACE") (setq ent_lst (cons (entget (entlast)) ent_lst)))
    (setq count (- count 1))
  )
  (shift_3d_faces lst ent_lst)
)


(defun shift_3d_faces(lst ent_lst / response first_in_list count last_in_list)
  (setq response "n") 
  (initget 1 "r l n")
  (setq response (getkword "DRAW TABLE TOP: Shift 3D faces? Shift [R]ight; Shift [L]eft; or [N]o shift. "))
  
  (if (or (= response "l") (= response "L"))
    (progn
      (setq lst (cdr lst))
      (setq first_in_list (car lst))
      (setq lst (reverse lst))
      (setq lst (cons first_in_list lst))
      (setq lst (reverse lst))

      (setq count *original_count*)
      (while (/= ent_lst nil)
        (entdel (cdr (assoc -1 (car ent_lst))))
        (setq ent_lst (cdr ent_lst))
      )
      
      (set_3d_faces lst)
    )
  )
  
  (if (or (= response "r") (= response "R"))
    (progn
      (setq lst (reverse lst))
      (setq lst (cdr lst))
      (setq last_in_list (car lst))
      (setq lst (reverse lst))
      (setq lst (cons last_in_list lst))
      
      (setq count *original_count*)
      (while (/= ent_lst nil)
        (entdel (cdr (assoc -1 (car ent_lst))))
        (setq ent_lst (cdr ent_lst))
      )
      
      (set_3d_faces lst)
    )
  )
  
  (if (or (= response "n") (= response "N"))
    (princ "\n")
  )
)

(defun flip_mn(poly_to_fix / lst vtx elst meshObj currVertexCount_M currVertexCount_N m_copy n_copy m n new_pt_lst acadObj doc points modelSpace poly_fixed)
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
  (setq lst (flippy lst m n))
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
  (princ)
  (princ poly_fixed)
  poly_fixed
)

(defun flippy(lst m n / y i j lst_modified lst_copy lst_reset)  
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