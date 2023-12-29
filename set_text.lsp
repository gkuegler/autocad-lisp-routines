(defun c:setxt (/ ss new_text ssl idx)
  (setq ss (ssget)  ; select objects
        new_text (getstring "new text: ")
        ssl (sslength ss)
        idx 0
  )
  (repeat ssl
    (setq ename (ssname ss idx)
      	  elist (entget ename)
          etype (cdr (assoc 0 elist))                    
    )
    (if (or (equal etype "TEXT") (equal etype "MTEXT"))
      (progn 
        (setq object (vlax-ename->vla-object ename)) ; Get the ActiveX object of the entity
              ; text (vla-get-TextString object)
              ; new_text (PostfixText text new_text)
        (if new_text (vla-put-TextString object new_text) (princ "The text could not be replaced.\n")) ; Change the text inside the MTex
      )
      (princ "The object was not a text object.\n")
    )
    (setq idx (1+ idx))
  )
  (princ)
)

