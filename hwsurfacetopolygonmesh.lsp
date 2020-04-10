(if (null mesh_it) (load "C:\\hw_commands\\lisp\\hwmesh_it.lsp"))
(if (null fix_spline_gap) (load "C:\\hw_commands\\lisp\\fix_spline_gap.lsp"))

;; Command: HW Surface to Polygon Mesh
;; Author: Andrew Bentzen 
;; Company: Haworth
;; Description: Turns a surface into a polygon mesh and allows you to adjust the 
;;; SURFTAB counts afterwards.  
;; Input: 1 surface entity
;; Output: 1 polygon mesh entity
(defun c:hwsurfacetopolygonmesh( / ent ss tier1 tier2 tier3 tier4 len_curve0 len_curve1 len_curve2
                                     len_curve3 dist_end_to_end0 dist_end_to_end1 dist_end_to_end2
                                     dist_end_to_end3 curve_factor1 curve_factor2 surftab1_is_1
                                     surftab2_is_1)
  (vl-load-com)
  (if (null mesh_it) (load "C:\\hw_commands\\lisp\\hwmesh_it.lsp"))
  (setvar "meshtype" 0)
  (vl-cmdf "shademode" "2d")

  (while (= (setq ent (car (entsel "\nSURFACE->POLYGON MESH: Pick surface: "))) nil))
  (vla-copy (vlax-ename->vla-object ent))
  
  (vl-cmdf "explode" (entlast))
  (setq ss (ssget "_p"))
  (if (and
        (not (equal (ssname ss 2) (ssname ss (sslength ss))))
        (= (sslength ss) 4)
      )
    (progn 
      (fix_spline_gap (ssname ss 0) (ssname ss 1))
      (fix_spline_gap (ssname ss 1) (ssname ss 2))
      (fix_spline_gap (ssname ss 2) (ssname ss 3))
      (fix_spline_gap (ssname ss 3) (ssname ss 0))
        
      (mesh_it (ssname ss 0) (ssname ss 1) (ssname ss 2) (ssname ss 3) "edgesurf" T)
;      (command "edgesurf" (ssname ss 0) (ssname ss 1) (ssname ss 2) (ssname ss 3))
      (set_surftab_v2 (ssname ss 0) (ssname ss 1) (ssname ss 2) (ssname ss 3) "edgesurf")
    )
  )
  (princ "\nSURFACE->POLYGON MESH: Complete. ")
  (princ)
)


(defun surfacetopolygonmesh(ent1 ent2 ent3 ent4 / )
  (vl-cmdf "edgesurf" ent1 ent2 ent3 ent4)
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
