(if (null adjoin) (load "C:\\hw_commands\\lisp\\adjoin.lsp"))
(command "undefine" "layiso")

;; Command: Layer Isolate:      (laiso)
;; Command: Layer Isolate Lock: (laiso_lock)
;; Command: Layer Lock:         (lalo)
;; Command: Layer Unlock:       (laun)
;; Command: Toggle Cells:       (cell)
;; Command: Layer Thaw:         (lath)
;; Command: Layer Freeze:       (lafr)
;; Author: Andrew Bentzen 
;; Company: Haworth
;; Description: Suite of commands that aid in layer manipulation. All commands can all be called
;;; from the command line by typing their function name. 


;; freezes all but selected layers. Works similarly to AutoCAD's LAYISO 
;;; command except this one freezes layers rather then turning them off. 
(defun c:laiso( / ss i lst item data layr)
  (setq ss (ssget))
  (setq i 0)
  (setq lst '())
  (if ss
    (repeat (sslength ss)
      (setq item (ssname ss i))
      (setq data (entget item))
      (setq layr (cdr (assoc 8 data)))
      (setq lst (adjoin layr lst))
      (setq i (1+ i))
    )
  )
  
  (setvar "clayer" (nth 0 lst))
  (command "layer" "freeze" "*" "")

  (setq i 0)
  (while (< i (length lst))
    (command "layer" "thaw" (nth i lst) "")
    (setq i (+ i 1))
  )
  (princ)
)

;; lock all but selected layers
(defun c:laiso_lock( / ss i lst item data layr)
  (setq ss (ssget))
  (setq i 0)
  (setq lst '())
  (if ss
    (repeat (sslength ss)
      (setq item (ssname ss i))
      (setq data (entget item))
      (setq layr (cdr (assoc 8 data)))
      (setq lst (adjoin layr lst))
      (setq i (1+ i))
    )
  )
  
  (setvar "clayer" (nth 0 lst))
  (command "layer" "lock" "*" "")

  (setq i 0)
  (while (< i (length lst))
    (command "layer" "unlock" (nth i lst) "")
    (setq i (+ i 1))
  )
  (princ)
)

;; locks all but the current layer
(defun c:lalo()
  (vl-cmdf "-layer" "lo" "*" "")
  (vl-cmdf "-layer" "un" (getvar "clayer") "")
)

;; unlocks all layers
(defun c:laun()
  (vl-cmdf "-layer" "un" "*" "")
)

;; toggles matrix cell and pointer layers 
(defun c:cell()
  (repeat 3
  (if (IsLayerLocked "block_layer")
    (progn (vl-cmdf "-layer" "un" "block_layer" "")
           (vl-cmdf "-layer" "un" "block_pointer" "")
           (vl-cmdf "-layer" "th" "block_layer" "")
           (vl-cmdf "-layer" "th" "block_pointer" ""))
    (progn (vl-cmdf "-layer" "lo" "block_layer" "")
           (vl-cmdf "-layer" "lo" "block_pointer" ""))))
)

;; thaws all layers
(defun c:lath()
  (vl-cmdf "-layer" "th" "*" "")
)

;; freezes all but the current layer
(defun c:lafr()
  (vl-cmdf "-layer" "fr" "*" "")
)
