;; Command: HW Edge
;; Author: Andrew Bentzen 
;; Company: Haworth
;; Description: Read a group of 3D faces and hides edges of adjacent faces. Note: this command was 
;;; having speed issues when you select many faces at one time. I mitigated that by allowing you 
;;; to break your selection set into smaller subsets. The command will loop through asking you to input
;;; more and more selection sets of 3D faces. When you have selected everything you need, press ENTER and it will 
;;; process everything you selected. 
;; Input: 3D faces that are adjacent to each other. 
;; Output: Returns corrected 3D faces with hidden edges. 

(defun c:hwedge( / ss_group snap i)
  (setq snap (getvar "osmode"))
  (setvar "osmode" 0)
  (command "shademode" "2d")
  
  
;;;;; single selection method 
;  (setq pt1 (getpoint "\nEDGE: Select first point for fence line:"))
;  (setq pt2 (getpoint pt1 "\nEDGE: Select second point for fence line: "))
;  (setq ss (ssget "_F" (list pt1 pt2) '((0 . "3DFACE"))))
;  (setq ss_group (cons ss ss_group))
;;;;; END single selection method 


;;;;; Multi selection method 
  (setq i 0)
  (princ "\nEDGE: Select group of 3D faces: ")
  (while (/= (setq ss (ssget '((0 . "3DFACE")))) nil)
    (princ "\nEDGE: Select group of 3D faces: ")
    (setq ss_group (cons ss ss_group))
  )
;;;;;; END  Multi selection method 
  (hw_edge ss_group)
  (setvar "osmode" snap)
  (princ)
)  
  
(defun hw_edge(ss_group / doc location nbr_of_hits node4 node3 node2 node1 
                   ss snap pointobj acadobj doc modelspace z y x k j i pt2 pt1 flag)
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  (setq i 0)
  (while (< i (length ss_group))
  (princ "\n") (princ i) (princ " of ") (princ (length ss_group)) (princ " complete.")
  (setq ss (nth i ss_group))
  (setq j 0)
  (while (< j (sslength ss))
    (setq node1 (cdr (assoc 10 (entget (ssname ss j)))))
    (setq node2 (cdr (assoc 11 (entget (ssname ss j)))))
    (setq node3 (cdr (assoc 12 (entget (ssname ss j)))))
    (setq node4 (cdr (assoc 13 (entget (ssname ss j)))))
        

    ;; Check edge 1
    (setq x (/ (+ (nth 0 node1) (nth 0 node2)) 2))
    (setq y (/ (+ (nth 1 node1) (nth 1 node2)) 2))
    (setq z (/ (+ (nth 2 node1) (nth 2 node2)) 2))
      
    ;; Define the location of the point
    (setq location (vlax-3d-point x y z))
    (setq modelSpace (vla-get-ModelSpace doc))
    (setq pointObj (vla-AddPoint modelSpace location))
        
    (setq flag nil)
    (setq nbr_of_hits 0)
    (setq k 0)
    (while (and (< k (sslength ss)) (= flag nil))
      (if (> (length (LM:intersections (vlax-ename->vla-object (ssname ss k)) (vlax-ename->vla-object (entlast)) acextendnone)) 0)
        (setq nbr_of_hits (+ nbr_of_hits 1))
      )
      (if (> nbr_of_hits 1) (setq flag T))
      (setq k (+ k 1))
    )
    
    (if (> nbr_of_hits 1) (setpropertyvalue (ssname ss j) "VisibilityEdge1" 0))
    (if (equal node1 node2) (setpropertyvalue (ssname ss j) "VisibilityEdge1" 1))
    (entdel (entlast))
    
    ;; Check edge 2
    (setq x (/ (+ (nth 0 node2) (nth 0 node3)) 2))
    (setq y (/ (+ (nth 1 node2) (nth 1 node3)) 2))
    (setq z (/ (+ (nth 2 node2) (nth 2 node3)) 2))
      
    ;; Define the location of the point
    (setq location (vlax-3d-point x y z))
    (setq modelSpace (vla-get-ModelSpace doc))
    (setq pointObj (vla-AddPoint modelSpace location))
        
    (setq flag nil)
    (setq nbr_of_hits 0)
    (setq k 0)
    (while (and (< k (sslength ss)) (= flag nil))
      (if (> (length (LM:intersections (vlax-ename->vla-object (ssname ss k)) (vlax-ename->vla-object (entlast)) acextendnone)) 0)
        (setq nbr_of_hits (+ nbr_of_hits 1))
      )
      (if (> nbr_of_hits 1) (setq flag T))
      (setq k (+ k 1))
    )
    
    (if (> nbr_of_hits 1) (setpropertyvalue (ssname ss j) "VisibilityEdge2" 0))
    (if (equal node2 node3) (setpropertyvalue (ssname ss j) "VisibilityEdge2" 1))
    (entdel (entlast))
    
    
    ;; Check edge 3
    (setq x (/ (+ (nth 0 node3) (nth 0 node4)) 2))
    (setq y (/ (+ (nth 1 node3) (nth 1 node4)) 2))
    (setq z (/ (+ (nth 2 node3) (nth 2 node4)) 2))
      
    ;; Define the location of the point
    (setq location (vlax-3d-point x y z))
    (setq modelSpace (vla-get-ModelSpace doc))
    (setq pointObj (vla-AddPoint modelSpace location))
        
    (setq flag nil)
    (setq nbr_of_hits 0)
    (setq k 0)
    (while (and (< k (sslength ss)) (= flag nil))
      (if (> (length (LM:intersections (vlax-ename->vla-object (ssname ss k)) (vlax-ename->vla-object (entlast)) acextendnone)) 0)
        (setq nbr_of_hits (+ nbr_of_hits 1))
      )
      (if (> nbr_of_hits 1) (setq flag T))
      (setq k (+ k 1))
    )
    
    (if (> nbr_of_hits 1) (setpropertyvalue (ssname ss j) "VisibilityEdge3" 0))
    (if (equal node3 node4) (setpropertyvalue (ssname ss j) "VisibilityEdge3" 1))
    (entdel (entlast))
    
    
    ;; Check edge 4    
    (setq x (/ (+ (nth 0 node4) (nth 0 node1)) 2))
    (setq y (/ (+ (nth 1 node4) (nth 1 node1)) 2))
    (setq z (/ (+ (nth 2 node4) (nth 2 node1)) 2))
      
    ;; Define the location of the point
    (setq location (vlax-3d-point x y z))
    (setq modelSpace (vla-get-ModelSpace doc))
    (setq pointObj (vla-AddPoint modelSpace location))
        
    (setq flag nil)
    (setq nbr_of_hits 0)
    (setq k 0)
    (while (and (< k (sslength ss)) (= flag nil))
      (if (> (length (LM:intersections (vlax-ename->vla-object (ssname ss k)) (vlax-ename->vla-object (entlast)) acextendnone)) 0)
        (setq nbr_of_hits (+ nbr_of_hits 1))
      )
      (if (> nbr_of_hits 1) (setq flag T))
      (setq k (+ k 1))
    )
    
    (if (> nbr_of_hits 1) (setpropertyvalue (ssname ss j) "VisibilityEdge4" 0))
    (if (equal node4 node1) (setpropertyvalue (ssname ss j) "VisibilityEdge4" 1))
    (entdel (entlast))
    
    (if (= 
          (getpropertyvalue (ssname ss j) "VisibilityEdge1")
          (getpropertyvalue (ssname ss j) "VisibilityEdge2")
          (getpropertyvalue (ssname ss j) "VisibilityEdge3")
          (getpropertyvalue (ssname ss j) "VisibilityEdge4")
          0
        )
      (progn
        (setpropertyvalue (ssname ss j) "VisibilityEdge1" 1)
        (setpropertyvalue (ssname ss j) "VisibilityEdge2" 1)
        (setpropertyvalue (ssname ss j) "VisibilityEdge3" 1)
        (setpropertyvalue (ssname ss j) "VisibilityEdge4" 1)
      )
    )
    
    (setq j (+ j 1))
  )
  (setq i (+ i 1))
  )
  (princ "\nEDGE: Complete. ")
  (princ)
)

;; Intersections  -  Lee Mac
;; Returns a list of all points of intersection between two objects
;; for the given intersection mode.
;; ob1,ob2 - [vla] VLA-Objects
;;     mod - [int] acextendoption enum of intersectwith method
(defun LM:intersections ( ob1 ob2 mod / lst rtn )
    (if (and (vlax-method-applicable-p ob1 'intersectwith)
             (vlax-method-applicable-p ob2 'intersectwith)
             (setq lst (vlax-invoke ob1 'intersectwith ob2 mod))
        )
        (repeat (/ (length lst) 3)
            (setq rtn (cons (list (car lst) (cadr lst) (caddr lst)) rtn)
                  lst (cdddr lst)
            )
        )
    )
    (reverse rtn)
)

