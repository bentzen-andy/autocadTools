;; Command: HW 3D Face
;; Author: Andrew Bentzen 
;; Company: Haworth
;; Description: Creates 3D faces. Designed to have the user click points around the perimeter 
;;; of a flat region that needs to be populated by 3D faces. 
;; Input: 4 or more click-points from the user 
;; Output: 1 or more 3D face entities 

(defun c:hw3dface( / user_input_list user_input acadObj doc count modelSpace faceObj)
  (vl-load-com)
  ;; set an undo marker. the end undo marker will be at the end of the entire script
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  (vla-StartUndoMark doc)
  
  ;; error handler to reset user settings if they hit esc 
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
  (princ "\nClick your fourth point: ")
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
  
  (setq count (/ count 2))
  (if (= (car user_input_list) nil) (setq user_input_list (cdr user_input_list )))
  (setq user_input_list (reverse user_input_list ))
  
  (vl-load-com)
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  ;; Create the 3DFace object in model space
  (setq modelSpace (vla-get-ModelSpace doc))
  (setq faceObj (vla-Add3DFace modelSpace (vlax-3d-point (nth 0 user_input_list))
                                          (vlax-3d-point (nth 1 user_input_list))
                                          (vlax-3d-point (nth 1 (reverse user_input_list)))
                                          (vlax-3d-point (nth 0 (reverse user_input_list)))))
;  (vla-put-VisibilityEdge2 faceObj :vlax-false)
;  (vla-put-VisibilityEdge4 faceObj :vlax-false


;  (vla-put-VisibilityEdge1 faceObj :vlax-false)
;  (vla-put-VisibilityEdge3 faceObj :vlax-false)
;  (vla-put-VisibilityEdge4 faceObj :vlax-false)


  
  (while (> count 0)
    (setq faceObj (vla-Add3DFace modelSpace (vlax-3d-point (nth (+ count 0) user_input_list))
                                            (vlax-3d-point (nth (+ count 1) user_input_list))
                                            (vlax-3d-point (nth (+ count 1) (reverse user_input_list)))
                                            (vlax-3d-point (nth (+ count 0) (reverse user_input_list)))))
;    (vla-put-VisibilityEdge2 faceObj :vlax-false)
;    (vla-put-VisibilityEdge4 faceObj :vlax-false)
    (setq count (- count 1))
  )
  ;; end of undo marker 
  (vla-EndUndoMark doc)

  (princ "\n3D FACE: Complete. ")
  (princ)
)