;; Command: HW Check Mesh
;; Author: Andrew Bentzen 
;; Company: Haworth
;; Description: Function corrects 3D faces/polygon meshes that are facing the wrong direction. 
;;; Some rendering programs require 3D faces to all be defined with the same point-list rotations. 
;;; Haworth standardizes on on counter-clockwise rotations for 3D faces that are meant to outward 
;;; facing. 

;;; This function reads in 3D face or polygon mesh entities and tests to see if they are 
;;; counter-clockwise (from the perspective of the AutoCAD camera view). If they're properly set
;;; to a counter-clockwise rotation, the function dose nothing to fix it. Otherwise it will correct
;;; the rotation of the 3D face/polygon mesh.
 
;;; All entities read in by this command are moved 200 units (usually inches) to the right. This gives
;;; the user a visual cue to which entities have already been quality-checked by the command (It's sort
;;; of a "done pile"). 

;; Input: 1 or more 3D faces or polygon meshes from the AutoCAD environment. Entities are read in 
;;; by the user via AutoCAD's "rubber-band" line. Function's author recommends using this command on 
;;; only a few entities at a time to ensure accuracy on the part of the user. 

;; Output: Void function. 

(defun c:hwcheckmesh_v2 ( / acadObj doc snap ortho_mode input pt1 pt2 ss i poly_layer flag
                            vtx vtx1 vtx2 vtx3 vtx4 need_flip ent_3dface move_dist)
  ;; set an undo marker. the end undo marker will be at the end of the entire script
  (vl-load-com)
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  (vla-StartUndoMark doc)
  ;; error handler to reset user settings if they hit esc 
  (defun *error*(msg)
    (setvar "osmode" snap)
    ;; end of undo marker 
    (vla-EndUndoMark doc)
    (princ)
  )
  
  (setq ortho_mode (getvar "orthomode"))
  (setvar "orthomode" 0)
  (setvar "cmdecho" 0)
  (setvar "meshtype" 0)
  (setq snap (getvar "osmode"))
  (setvar "osmode" 0)
  (vl-cmdf "ucs" "world")
  (setq move_dist 200)
;  (vl-cmdf "ucs" "view")
;  (if (= move_dist nil) (setq move_dist 0))
  
;  (setq pt1 (getpoint "\nCHECK MESH: Select first point: or [Set move distance]:"))
;  (princ "\nCHECK MESH: Select first point: or [Set move distance]:")
;  (setq input (grread nil 2))
;  (cond 
;    ((and (= (nth 0 input) 2) (= (nth 1 input) 115)) (setq move_dist (getreal "\nCHECK MESH: Specify new movement distance: ")))
;    ((= (nth 0 input) 3) (setq pt1 (nth 0 (cdr input))))
;  )
  (setq pt1 (getpoint "\nSelect first point:"))
  (setq pt2 (getpoint pt1 "\nCHECK MESH: Select next point: "))
  (setq ss (ssget "_F" (list pt1 pt2) '((-4 . "<OR") (0 . "POLYLINE") (0 . "3DFACE") (-4 . "OR>"))))

  (setq i 0)

  (while (< i (sslength ss))
  
    (if (= (cdr (assoc 0 (entget (ssname ss i)))) "POLYLINE") 
      (setq flag (expld_check (ssname ss i) pt1 pt2))
    )
    
    (if (= (cdr (assoc 0 (entget (ssname ss i)))) "3DFACE")
      (progn
        (setq ent_3dface (ssadd))
        (ssadd (ssname ss i) ent_3dface)
        (setq flag (ent_3dface_clockwise-p ent_3dface))
;        (hwedge ss)       
      )
    )
    
    (if (= flag T)
      (progn
;        (acet-sys-beep 1)
        (princ "\nCHECK MESH: Mesh/3D face needs to be fixed. ")
        (setq poly_layer (cdr (assoc 8 (entget (ssname ss i)))))
        (if (= (cdr (assoc 0 (entget (ssname ss i)))) "POLYLINE") (zigzag (ssname ss i)))
        (if (= (cdr (assoc 0 (entget (ssname ss i)))) "3DFACE") (flipface (ssname ss i)))
        (vl-cmdf "chprop" (entlast) "" "la" poly_layer "")
        (princ "\nCHECK MESH: Complete. ")
        (vl-cmdf "ucs" "world")
        (vl-cmdf "move" (entlast) "" '(0 0 0) (list move_dist 0 0))
;        (vl-cmdf "ucs" "view")
      )
      (progn 
        (princ "\nCHECK MESH: Mesh/3D is already in its correct orientation. ")
        (princ "\nCHECK MESH: Complete. ")
        (vl-cmdf "ucs" "world")
        (vl-cmdf "move" (ssname ss i) "" '(0 0 0) (list move_dist 0 0))
;        (vl-cmdf "ucs" "view")
      )
    )
    (setq i (+ i 1))
  )
  
  (vl-cmdf "ucs" "world")
  (setvar "cmdecho" 1)
  (setvar "orthomode" ortho_mode)
  (setvar "osmode" snap)
  ;; end of undo marker 
  (vla-EndUndoMark doc)
  (princ)
)

(defun expld_check (ent pt1 pt2 / ss ss2 vtx1 vtx2 vtx3 flag i ent_3dface need_flip)
  (setq ent (vla-copy (vlax-ename->vla-object ent)))
  (vl-cmdf "explode" (entlast))
  (setq ss (ssget "_p"))
  (setq ss2 (ssget "_F" (list pt1 pt2) '((0 . "3DFACE"))))
  (setq ent (ssname ss2 0))

  (setq vtx1 (trans (cdr (assoc 10 (entget ent))) 0 1))
  (setq vtx2 (trans (cdr (assoc 11 (entget ent))) 0 1))
  (setq vtx3 (trans (cdr (assoc 12 (entget ent))) 0 1))
;  (setq flag (gc:clockwise-p vtx1 vtx2 vtx3))

  (setq ent_3dface (ssadd))
  (ssadd ent ent_3dface)
  (setq flag (ent_3dface_clockwise-p ent_3dface))


  (setq i 0)
  (while (< i (sslength ss))
    (entdel (ssname ss i))
    (setq i (+ i 1))
  )
  flag
)


;; Clockwise-p  -  gile
;; Returns T if p1,p2,p3 are clockwise oriented
(defun gc:clockwise-p ( p1 p2 p3 )
    (< (sin (- (angle p1 p3) (angle p1 p2))) -1e-14)
)

(defun flipface (ent / vtx1 vtx2 vtx3 vtx4 face_viz)
  (setq face_viz nil)
  (setq face_viz (cons (vla-get-VisibilityEdge1 (vlax-ename->vla-object ent)) face_viz))
  (setq face_viz (cons (vla-get-VisibilityEdge2 (vlax-ename->vla-object ent)) face_viz))
  (setq face_viz (cons (vla-get-VisibilityEdge3 (vlax-ename->vla-object ent)) face_viz))
  (setq face_viz (cons (vla-get-VisibilityEdge4 (vlax-ename->vla-object ent)) face_viz))

  (setq vtx1 (trans (cdr (assoc 10 (entget ent))) 0 1))
  (setq vtx2 (trans (cdr (assoc 11 (entget ent))) 0 1))
  (setq vtx3 (trans (cdr (assoc 12 (entget ent))) 0 1))
  (setq vtx4 (trans (cdr (assoc 13 (entget ent))) 0 1))
  (vl-cmdf "3dface" vtx4 vtx3 vtx2 vtx1 "")
  (vla-put-VisibilityEdge4 (vlax-ename->vla-object (entlast)) (nth 0 face_viz))
  (vla-put-VisibilityEdge3 (vlax-ename->vla-object (entlast)) (nth 3 face_viz))
  (vla-put-VisibilityEdge2 (vlax-ename->vla-object (entlast)) (nth 2 face_viz))
  (vla-put-VisibilityEdge1 (vlax-ename->vla-object (entlast)) (nth 1 face_viz))
  (entdel ent)
  (princ "\nFLIP FACE: Complete. ")
  (princ)
)

(defun adjoin (item data)
  ;; returns a list with the new item added to the list unless it already is included 
  (if (member item data)
      data
      (cons item data)))
      


(defun ent_3dface_clockwise-p(ss / oldvar ent_3dface ent_info pt_lst pt flag_colocated_pt flag)
;  (setq olderr *error*)
;  (setq *error* newerr)
  (setq oldvar (mapcar 'getvar '("UCSFOLLOW" "GRIDMODE" "CMDECHO" "UCSICON" "BLIPMODE")))
  (mapcar 'setvar '("UCSFOLLOW" "GRIDMODE" "CMDECHO" "UCSICON" "BLIPMODE") '(0 0 0 0 0))  
  
  (setq ent_3dface (ssname ss 0))
     
  (setq ent_info (entget ent_3dface))
  (setq pt_lst (list (cdr (assoc 10 ent_info))))
      
  ;; determines if any of the points on the 3d face are 
  ;; co-located and excludes those co-located coordinates from the point list.
  (foreach dxf_group_code '(11 12 13)
    (setq flag_colocated_pt nil)
    (foreach pt pt_lst
      (setq flag_colocated_pt (or flag_colocated_pt (equal pt (cdr (assoc dxf_group_code ent_info)) 0.000001)))
    )
        
    (if (not flag_colocated_pt)
      (setq pt_lst (append pt_lst (list (cdr (assoc dxf_group_code ent_info)))))
    )
  )
      
  ;; uses the difference between the 3-point ucs (i.e., the 3-point ucs of
  ;; the 3dface) and the ucs of the user's current view (obtained by using 
  ;; the value of the 2 with the trans function. 
  (vl-cmdf ".ucs" "w")
  (if (> (length pt_lst) 2)
    (progn
      (vl-cmdf ".ucs" "3p" (nth 0 pt_lst) (nth 1 pt_lst) (nth 2 pt_lst))
      (if (>= (nth 2 (mapcar '- (trans (list 0 0 0) 1 2) (trans (list 0 0 1) 1 2))) 0.0)
        (setq flag T)
        (setq flag nil)
      )
    )
  )
  (vl-cmdf "ucs" "w")
      
  (mapcar 'setvar '("UCSFOLLOW" "GRIDMODE" "CMDECHO" "UCSICON" "BLIPMODE") oldvar)
  (princ)
  flag
)