;; Command: HW Edge Surf
;; Author: Andrew Bentzen 
;; Company: Haworth
;; Description: Same as AutoCAD's EDGESURF command, accept this lets you edit your SURFTAB counts 
;;; after creating the mesh. 

(if (null mesh_it) (load "C:\\hw_commands\\lisp\\hwmesh_it.lsp"))
(if (null fix_spline_gap) (load "C:\\hw_commands\\lisp\\fix_spline_gap.lsp"))

(defun c:es( / ent1 ent2 ent3 ent4)
  (defun *error*(msg)
    (vla-Highlight (vlax-ename->vla-object ent1) :vlax-false)
    (vla-Highlight (vlax-ename->vla-object ent2) :vlax-false)
    (vla-Highlight (vlax-ename->vla-object ent3) :vlax-false)
    (vla-Highlight (vlax-ename->vla-object ent4) :vlax-false)
    (princ)
  )
  (setvar "meshtype" 0)

  (princ "\nSelect object 1 for surface edge:  ")
  (setq ent1 (ssname (ssget "_+.:E:S" '((-4 . "<OR") (0 . "SPLINE") (0 . "LINE") (0 . "ARC") (0 . "ELLIPSE") (0 . "POINT") (-4 . "<AND") (0 . "POLYLINE") (71 . 0) (-4 . "AND>") (0 . "CIRCLE") (-4 . "OR>"))) 0))
  (vla-Highlight (vlax-ename->vla-object ent1) :vlax-true)
  (princ "\nSelect object 2 for surface edge:  ")
  (setq ent2 (ssname (ssget "_+.:E:S" '((-4 . "<OR") (0 . "SPLINE") (0 . "LINE") (0 . "ARC") (0 . "ELLIPSE") (0 . "POINT") (-4 . "<AND") (0 . "POLYLINE") (71 . 0) (-4 . "AND>") (0 . "CIRCLE") (-4 . "OR>"))) 0))
  (vla-Highlight (vlax-ename->vla-object ent2) :vlax-true)
  (princ "\nSelect object 3 for surface edge:  ")
  (setq ent3 (ssname (ssget "_+.:E:S" '((-4 . "<OR") (0 . "SPLINE") (0 . "LINE") (0 . "ARC") (0 . "ELLIPSE") (0 . "POINT") (-4 . "<AND") (0 . "POLYLINE") (71 . 0) (-4 . "AND>") (0 . "CIRCLE") (-4 . "OR>"))) 0))
  (vla-Highlight (vlax-ename->vla-object ent3) :vlax-true)
  (princ "\nSelect object 4 for surface edge:  ")
  (setq ent4 (ssname (ssget "_+.:E:S" '((-4 . "<OR") (0 . "SPLINE") (0 . "LINE") (0 . "ARC") (0 . "ELLIPSE") (0 . "POINT") (-4 . "<AND") (0 . "POLYLINE") (71 . 0) (-4 . "AND>") (0 . "CIRCLE") (-4 . "OR>"))) 0))
  (vla-Highlight (vlax-ename->vla-object ent4) :vlax-true)
  
;  (setq ent1 (fix_spline_gap ent1 ent2))
;  (setq ent2 (fix_spline_gap ent2 ent3))
;  (setq ent3 (fix_spline_gap ent3 ent4))
;  (setq ent4 (fix_spline_gap ent4 ent1))
  
  (fix_spline_gap ent1 ent2)
  (fix_spline_gap ent2 ent3)
  (fix_spline_gap ent3 ent4)
  (fix_spline_gap ent4 ent1)
  
  (if *hwcountestimator*
    (progn
      (mesh_it ent1 ent2 ent3 ent4 "edgesurf" T)
      (set_surftab_v2 ent1 ent2 ent3 ent4 "edgesurf")
    )
    (progn
      (vl-cmdf "edgesurf" ent1 ent2 ent3 ent4)
      (set_surftab ent1 ent2 ent3 ent4 "edgesurf")
    )
  )
  
  (vla-Highlight (vlax-ename->vla-object ent1) :vlax-false)
  (vla-Highlight (vlax-ename->vla-object ent2) :vlax-false)
  (vla-Highlight (vlax-ename->vla-object ent3) :vlax-false)
  (vla-Highlight (vlax-ename->vla-object ent4) :vlax-false)
  (princ "\nEDGE SURF: Complete. ")
  (princ)

)

;; Command: HW Rule Surf
;; Author: Andrew Bentzen 
;; Company: Haworth
;; Description: Same as AutoCAD's RULESURF command, accept this lets you edit your SURFTAB counts 
;;; after creating the mesh. 

(defun c:rs( / ent1 ent2)
  (setvar "meshtype" 0)
  (princ "\nSelect first defining curve: ")
  (setq ent1 (ssname (ssget "_+.:E:S" '((-4 . "<OR") (0 . "SPLINE") (0 . "LINE") (0 . "ARC") (0 . "ELLIPSE") (0 . "POINT") (-4 . "<AND") (0 . "POLYLINE") (71 . 0) (-4 . "AND>") (0 . "CIRCLE") (-4 . "OR>"))) 0))
  (princ "\nSelect second defining curve: ")
  (setq ent2 (ssname (ssget "_+.:E:S" '((-4 . "<OR") (0 . "SPLINE") (0 . "LINE") (0 . "ARC") (0 . "ELLIPSE") (0 . "POINT") (-4 . "<AND") (0 . "POLYLINE") (71 . 0) (-4 . "AND>") (0 . "CIRCLE") (-4 . "OR>"))) 0))
  
  (vl-cmdf "rulesurf" ent1 ent2)
  (set_surftab ent1 ent2 ent1 ent2 "rulesurf")
  (princ "\nRULE SURF: Complete. ")
  (princ)
)

(defun set_surftab(ent1 ent2 ent3 ent4 command_option / response st1 st2)
  (setq response "u")
  (while (/= response "n")
    (princ "\nChange SURFTAB1?<")
    (princ (getvar "surftab1"))
    (princ ">: [U]p; [D]own; [A]dd by 4; [L]ess by 4; [S]wap counts; [N]o change. ")
  
    (initget "u d n a l s ")
    (setq response (getkword))
    (if (= response "u") (setvar "surftab1" (+ (getvar "surftab1") 1)))
    (if (= response "d") (setvar "surftab1" (- (getvar "surftab1") 1)))
    (if (= response "a") (setvar "surftab1" (+ (getvar "surftab1") 4)))
    (if (= response "l") (setvar "surftab1" (- (getvar "surftab1") 4)))
    (if (= response "s") (progn (setq st1 (getvar "surftab1")) (setq st2 (getvar "surftab2")) (setvar "surftab1" st2) (setvar "surftab2" st1)))
    (if (= response nil) (setq response "n"))
    
    (if (/= response "n")
      (progn
        (entdel (entlast))
        (cond
          ((= command_option "edgesurf") (vl-cmdf command_option ent1 ent2 ent3 ent4))
          ((= command_option "revsurf") (ts ent1 ent2))
          (t (vl-cmdf command_option ent1 ent2))
        )
      )
    )
  )
  (princ)
  
  
  (setq response "u")
  (while (/= response "n")
    (princ "\nChange SURFTAB2?<")
    (princ (getvar "surftab2"))
    (princ ">: [U]p; [D]own; [A]dd by 4; [L]ess by 4; [S]wap counts; [N]o change. ")
  
    (initget "u d n a l s")
    (setq response (getkword))
    (if (= response "u") (setvar "surftab2" (+ (getvar "surftab2") 1)))
    (if (= response "d") (setvar "surftab2" (- (getvar "surftab2") 1)))
    (if (= response "a") (setvar "surftab2" (+ (getvar "surftab2") 4)))
    (if (= response "l") (setvar "surftab2" (- (getvar "surftab2") 4)))
    (if (= response "s") (progn (setq st1 (getvar "surftab1")) (setq st2 (getvar "surftab2")) (setvar "surftab1" st2) (setvar "surftab2" st1)))
    (if (= response nil) (setq response "n"))
    
    (if (/= response "n")
      (progn
        (entdel (entlast))
        (cond
          ((= command_option "edgesurf") (vl-cmdf command_option ent1 ent2 ent3 ent4))
          ((= command_option "revsurf") (ts ent1 ent2))
          (t (vl-cmdf command_option ent1 ent2))
        )
      )
    )
  )
  (princ)
)

(defun set_surftab_v2(ent1 ent2 ent3 ent4 command_option / response st1 st2)
  (setq response "u")
  (while (/= response "n")
    (princ "\nChange SURFTAB1?<")
    (princ (getvar "surftab1"))
    (princ ">: [U]p; [D]own; [A]dd by 4; [L]ess by 4; [S]wap counts; [N]o change. ")
  
    (initget "u d n a l s ")
    (setq response (getkword))
    (if (= response "u") (setvar "surftab1" (+ (getvar "surftab1") 1)))
    (if (= response "d") (setvar "surftab1" (- (getvar "surftab1") 1)))
    (if (= response "a") (setvar "surftab1" (+ (getvar "surftab1") 4)))
    (if (= response "l") (setvar "surftab1" (- (getvar "surftab1") 4)))
    (if (= response "s") (progn (setq st1 (getvar "surftab1")) (setq st2 (getvar "surftab2")) (setvar "surftab1" st2) (setvar "surftab2" st1)))
    (if (= response nil) (setq response "n"))
    
    (if (/= response "n")
      (progn
        (entdel (entlast))
        (cond
          ((= command_option "edgesurf") (vl-cmdf command_option ent1 ent2 ent3 ent4))
          ((= command_option "revsurf") (ts ent1 ent2))
          (t (vl-cmdf command_option ent1 ent2))
        )
      )
    )
  )
  (princ)
  
  
  (setq response "u")
  (while (/= response "n")
    (princ "\nChange SURFTAB2?<")
    (princ (getvar "surftab2"))
    (princ ">: [U]p; [D]own; [A]dd by 4; [L]ess by 4; [S]wap counts; [N]o change. ")
  
    (initget "u d n a l s")
    (setq response (getkword))
    (if (= response "u") (setvar "surftab2" (+ (getvar "surftab2") 1)))
    (if (= response "d") (setvar "surftab2" (- (getvar "surftab2") 1)))
    (if (= response "a") (setvar "surftab2" (+ (getvar "surftab2") 4)))
    (if (= response "l") (setvar "surftab2" (- (getvar "surftab2") 4)))
    (if (= response "s") (progn (setq st1 (getvar "surftab1")) (setq st2 (getvar "surftab2")) (setvar "surftab1" st2) (setvar "surftab2" st1)))
    (if (= response nil) (setq response "n"))
    
    (if (/= response "n")
      (progn
        (entdel (entlast))
        (cond
          ((= command_option "edgesurf") (vl-cmdf command_option ent1 ent2 ent3 ent4))
          ((= command_option "revsurf") (ts ent1 ent2))
          (t (vl-cmdf command_option ent1 ent2))
        )
      )
    )
  )
  (princ)
)