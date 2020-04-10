;; Command: HW Macro Stretch
;; Author: Andrew Bentzen 
;; Company: Haworth
;; Description: Command only works in conjunction with Haworth's Matrix DWG files. Command works
;;; similarly to AutoCAD's STRETCH command. The Command allows you to apply a stretch's selection set
;;; to many CAD symbols at once. Follow prompts of the command be sure to make the selection set large
;;; enough to capture all the target matrix cells that you want to apply the stretch to. Please quality- 
;;; check your results! 


(defun c:hwmacro_stretch( / dist_stretch pt2_trans doc window_ur delta_z delta_y delta_x 
                            base_pt window_ll pt1_delta pt1_trans snap pt2_dir acadobj 
                            pt1_target base_cell pt1_dir ss_target ss_to_stretch dist_stretch_z 
                            dist_stretch_y dist_stretch_x cell_size info i pt2 pt1 pt2_target pt2_delta)
  ;; set an undo marker. the end undo marker will be at the end of the entire script
  (vl-load-com)
  (setq acadObj (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadObj))
  (vla-StartUndoMark doc)
  ;; error handler to reset user settings if they hit esc 
  (defun *error*(msg)
    ;; end of undo marker 
    (vla-EndUndoMark doc)
    (setvar "osmode" snap)
    (princ)
  )
    

  (setq snap (getvar "osmode"))
  (setvar "osmode" 0)
  (setvar "orthomode" 1)
  
  ;; get points / selections sets for the stretch 
  (setq pt1 (getpoint "\nSTRETCH MACRO: Select stretch selection. "))
  (setq pt2 (getcorner pt1 "\nSTRETCH MACRO: Pick opposite corner. "))
  (setq ss_to_stretch (ssget "C" pt1 pt2))
  
  (setq pt1_dir (getpoint "\nSTRETCH: Stretch direction (pt1): "))
  (setq pt2_dir (getpoint pt1_dir "\nSTRETCH: Stretch direction (pt2): "))


  (setq dist_stretch_x nil)
  (setq dist_stretch_y nil)
  (setq dist_stretch_z nil)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for pt-to-pt distance selection 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  (setvar "osmode" snap)
;  (setq dist_stretch_pt1 (getpoint "\nSTRETCH MACRO: Select points for distance (1 of 2): "))
;  (setq dist_stretch_pt2 (getpoint  "\nSTRETCH MACRO: Select points for distance (2 of 2): "))
;  (setvar "osmode" 0)
;  
;  (setq dist_stretch_x (abs (- (nth 0 dist_stretch_pt2)  (nth 0 dist_stretch_pt1))))
;  (setq dist_stretch_y (abs (- (nth 1 dist_stretch_pt2)  (nth 1 dist_stretch_pt1))))
;  (setq dist_stretch_z (abs (- (nth 2 dist_stretch_pt2)  (nth 2 dist_stretch_pt1))))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END for pt-to-pt distance selection 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for real number distance selection 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  (setq dist_stretch (getreal "\nSTRETCH MACRO: Stretch distance: "))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END for real number distance selection 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

  (if (= dist_stretch_x dist_stretch_y dist_stretch_z nil)
    (progn 
      (setq dist_stretch_x dist_stretch)
      (setq dist_stretch_y dist_stretch)
      (setq dist_stretch_z dist_stretch)))

  (setq delta_x 0.0)
  (setq delta_y 0.0)
  (setq delta_z 0.0)
      
  (setq delta_x (- (nth 0 pt2_dir) (nth 0 pt1_dir)))
  (setq delta_y (- (nth 1 pt2_dir) (nth 1 pt1_dir)))
  (setq delta_z (- (nth 2 pt2_dir) (nth 2 pt1_dir)))
      
  (if (not (equal delta_x 0.0 0.001)) (if (< delta_x 0) (setq dist_stretch_x (* dist_stretch_x -1))))
  (if (not (equal delta_y 0.0 0.001)) (if (< delta_y 0) (setq dist_stretch_y (* dist_stretch_y -1))))
  (if (not (equal delta_x 0.0 0.001)) (if (< delta_z 0) (setq dist_stretch_z (* dist_stretch_z -1))))

  ;; get base cell 
  (setq base_cell (car (entsel "\nSTRETCH MACRO: Pick your base matrix cell. ")))
  (while (not base_cell) (setq base_cell (car (entsel "\nSTRETCH MACRO: Pick your base matrix cell. "))))
  
  ;; get deltas for points 
  (setq base_pt (cdr (assoc 10 (entget (entnext (cdr (assoc -1 (entget base_cell))))))))
  (setq pt1_delta (list (- (nth 0 pt1) (nth 0 base_pt))
                        (- (nth 1 pt1) (nth 1 base_pt))
                        (- (nth 2 pt1) (nth 2 base_pt))))
  (setq pt2_delta (list (- (nth 0 pt2) (nth 0 base_pt))
                        (- (nth 1 pt2) (nth 1 base_pt))
                        (- (nth 2 pt2) (nth 2 base_pt))))
  
  
  
  
  
  ;; get selection for target cells
  (setq pt1_target (getpoint "\nSTRETCH MACRO: Select target matrix cells for the macro. "))
  (setq pt2_target (getcorner pt1_target "\nSTRETCH MACRO: Pick opposite corner. "))
  (setq ss_target (ssget "C" pt1_target pt2_target '( (-4 . "<AND") 
                                                        (8 . "block_layer")
                                                        (0 . "INSERT")
                                                      (-4 . "AND>")
                                                    )))
  (command "-view" "save" "view_main")
  
  
  ;; run the stretch for the target cells
  (setq i 0)
  (while (< i (sslength ss_target))
    (setq cell_size (atoi (substr (cdr (assoc 1 (entget (entnext (entnext (cdr (assoc -1 (entget (ssname ss_target i))))))))) 1 3)))
    (setq info (entget (entnext (cdr (assoc -1 (entget (ssname ss_target i)))))))
    (setq window_ll (cdr (assoc 10 info)))
    (setq window_ll (list (- (nth 0 window_ll) 1)
                          (- (nth 1 window_ll) 1)
                          (nth 2 window_ll)))
    (setq window_ur (list (+ (nth 0 window_ll) cell_size)
                          (+ (nth 1 window_ll) cell_size)
                          (nth 2 window_ll)))
    
    (setq base_pt (cdr (assoc 10 (entget (entnext (cdr (assoc -1 (entget (ssname ss_target i)))))))))
    (setq pt1_trans (list (+ (nth 0 pt1_delta) (nth 0 base_pt))
                          (+ (nth 1 pt1_delta) (nth 1 base_pt))
                          (+ (nth 2 pt1_delta) (nth 2 base_pt))))
    (setq pt2_trans (list (+ (nth 0 pt2_delta) (nth 0 base_pt))
                          (+ (nth 1 pt2_delta) (nth 1 base_pt))
                          (+ (nth 2 pt2_delta) (nth 2 base_pt))))
    (command "zoom" "window" pt1_trans pt2_trans)
    (setq ss_to_stretch (ssget "C" pt1_trans pt2_trans))
    
    
    (if (not (equal delta_x 0.0 0.001)) (command "stretch" ss_to_stretch "" '(0 0 0) (list dist_stretch_x 0 0)))
    (if (not (equal delta_y 0.0 0.001)) (command "stretch" ss_to_stretch "" '(0 0 0) (list 0 dist_stretch_y 0)))
    (if (not (equal delta_z 0.0 0.001)) (command "stretch" ss_to_stretch "" '(0 0 0) (list 0 0 dist_stretch_z)))

    
    (setq i (+ i 1))
  )
  (command "-view" "restore" "view_main")
  (setvar "osmode" snap)
  
  ;; end of undo marker 
  (vla-EndUndoMark doc)
  (princ "\nSTRETCH MACRO: Complete. ")
  (princ)
)


(defun adjoin (item data)
  ;; returns a list with the new item added to the list unless it already is included 
  (if (member item data)
      data
      (cons item data))
)
