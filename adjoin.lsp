(defun adjoin (item data)
  ;; returns a list with the new item added to the list unless it already is included 
  (if (member item data)
      data
      (cons item data))
)