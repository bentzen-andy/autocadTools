;; Command: HW 3D Face
;; Author: Andrew Bentzen 
;; Company: Haworth
;; Description: Creates 3-sided 3D faces. Designed to have the user click points around the perimeter 
;;; of a flat region that needs to be populated by 3D faces. First click defines the "anchor point".
;;; Subsequent clicks define the 3D face points that extend from the anchor point. 
;; Input: 3 or more click-points from the user 
;; Output: 1 or more 3D face entities 

(defun c:hw3dface_spiderweb( / user_input user_input_list count)

  (vl-cmdf "ucs" "world")
  
  (princ "\nClick your first point: ")
  (setq user_input (getpoint))
  (setq user_input_list (cons user_input user_input_list))
  (princ "\nClick your second point: ")
  (setq user_input (getpoint))
  (setq user_input_list (cons user_input user_input_list))
  (princ "\nClick your third point: ")
  (setq user_input (getpoint))
  (setq user_input_list (cons user_input user_input_list))
  (princ "\nClick your next point or <Enter>: ")
  
  (if (/= user_input nil) (setq user_input (getpoint)))
  
  (setq count 0)
  (while (/= user_input nil)
    (if (> count 0) (setq user_input (getpoint)))
    (setq user_input_list (cons user_input user_input_list))
    (princ "\nClick your next point or <Enter>: ")
    (setq count (+ count 1))
  )
  (if (= (car user_input_list) nil) (setq user_input_list (cdr user_input_list )))
  (setq user_input_list (reverse user_input_list ))



  (hw_3dface_spiderweb user_input_list count)
)



(defun hw_3dface_spiderweb(user_input_list count / iteration user_input acadObj 
                           doc modelSpace faceObj faces)
  (vl-load-com)
  ;; set an undo marker. the end undo marker will be at the end of the entire script
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  (vla-StartUndoMark doc)
  
  ;; error handler to reset user settings if they hit esc 
  (defun *error*(msg)
    (if (/= sel_prev nil) (setvar "selectionpreview" sel_prev))
    (if (not (member msg '("Function cancelled" "quit / exit abort" 
                           "bad argument type: VLA-OBJECT nil")))
      (princ (strcat "\nError: " msg))
    )
    (setq *face_ent_lst* nil)
    ;; end of undo marker 
    (vla-EndUndoMark doc)
    (princ)
  )
  
;  (setq faces (ssadd))




  (vl-load-com)
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  ;; Create the 3DFace object in model space
  (setq modelSpace (vla-get-ModelSpace doc))
  (setq faceObj (vla-Add3DFace modelSpace (vlax-3d-point (nth 0 user_input_list))
                                          (vlax-3d-point (nth 1 user_input_list))
                                          (vlax-3d-point (nth 2 user_input_list))
                                          (vlax-3d-point (nth 2 user_input_list))))
;  (vla-put-VisibilityEdge1 faceObj :vlax-false)
;  (vla-put-VisibilityEdge2 faceObj :vlax-false)
;  (vla-put-VisibilityEdge3 faceObj :vlax-false)
;  (vla-put-VisibilityEdge4 faceObj :vlax-false)
;  (ssadd (entlast) faces)
  (setq *face_ent_lst* (cons (entlast) *face_ent_lst*))
  
  (setq iteration 0)
  (while (> count 0)
    (setq faceObj (vla-Add3DFace modelSpace (vlax-3d-point (nth 0 user_input_list))
                                            (vlax-3d-point (nth (+ iteration 2) user_input_list))
                                            (vlax-3d-point (nth (+ iteration 3) user_input_list))
                                            (vlax-3d-point (nth (+ iteration 3) user_input_list))))
;    (vla-put-VisibilityEdge1 faceObj :vlax-false)
;    (vla-put-VisibilityEdge2 faceObj :vlax-false)
;    (vla-put-VisibilityEdge3 faceObj :vlax-false)
;    (vla-put-VisibilityEdge4 faceObj :vlax-false)
;    (ssadd (entlast) faces)


    (setq *face_ent_lst* (cons (entlast) *face_ent_lst*))
    (if *combine_faces*
      (if (= (rem iteration 2) 0) (join_faces (nth 1 *face_ent_lst*) (nth 0 *face_ent_lst*)))
    )
    
    (setq count (- count 1))
    (setq iteration (+ iteration 1))
;    (if (/= count 1) (progn (vla-put-VisibilityEdge3 faceObj :vlax-false) (vla-put-VisibilityEdge4 faceObj :vlax-false)))
  )
;  (vla-put-VisibilityEdge3 faceObj :vlax-false)
;  (vla-put-VisibilityEdge4 faceObj :vlax-false)

;  (edge (list faces))
  ;; end of undo marker 
  (vla-EndUndoMark doc)
  (setq *combine_faces* nil)

  (princ "\n3D FACE SPIDERWEB: Complete. ")
  (princ)
)







(defun join_faces(ent1 ent2 / ent_lst ptlist i )
  (setq ent_lst (list ent1 ent2))
  (setq ptlist nil)
  (setq i 0)
  (while (< i (length ent_lst))
    (setq ptlist (adjoin (cdr (assoc 10 (entget (nth i ent_lst)))) ptlist))
    (setq ptlist (adjoin (cdr (assoc 11 (entget (nth i ent_lst)))) ptlist))
    (setq ptlist (adjoin (cdr (assoc 12 (entget (nth i ent_lst)))) ptlist))
    (setq ptlist (adjoin (cdr (assoc 13 (entget (nth i ent_lst)))) ptlist))
    (setq i (+ i 1))
  )

  (
  (lambda ( ref 2pi )
    (vl-sort ptlist
      (function
        (lambda ( a b ) (> (2pi (angle ref a)) (2pi (angle ref b))))
      )
    )
  )
  (
    (lambda ( n )
      (mapcar '/ (apply 'mapcar (cons '+ ptlist)) (list n n n))
    )
    (float (length ptlist))
  )
  (lambda ( a ) (rem (+ pi pi a) (+ pi pi)))
  )

  (entdel ent1)
  (entdel ent2)
  (entmakex (list (cons 0 "3DFACE")
                  (cons 10 (nth 0 ptlist))
                  (cons 11 (nth 1 ptlist))
                  (cons 12 (nth 2 ptlist))
                  (cons 13 (nth 3 ptlist))))
)

(defun adjoin (item data)
  ;; returns a list with the new item added to the list unless it already is included 
  (if (member item data)
      data
      (cons item data))
)