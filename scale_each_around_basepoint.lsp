;* Scale Multiple
;* Rotates many entities around their respective basepoints
;* allows selection by AUTOCAD selection sets or SSX.
;* Written by David Husch, January 1991

; elist = enitity list, the 'e' stands for entity
; 10 index center point object
; ARC
; CIRCLE
; ELLIPSE
; HATCH => this one is weird it's the elevation point
; LEADER => vertex points
; MLINE => same as line
; POLYLINE => more complicated
; SHAPE
; BLOCK => basepoint
; LWPOLYLINE
; AECB_DEVICE

(defun c:scalebase (/ ss ssl factor idx ename elist etype x1 x2 y1 y2 center)
  (prompt "Select Entities to Rotate, <ENTER> for SSX.")
  (setq ss (ssget))
  (setq factor (getreal "Scale Factor: "))
  ; (setq mode (getstring t "Copy or Rotate: "))
  (if (not ss) (setq ss (ssx)))
  (setq ssl (sslength ss))
  (setq idx 0)
  (if ss 
    (repeat ssl
      (setq ename (ssname ss idx)
            elist (entget ename)
            etype (cdr (assoc 0 elist))
            center (cond ((equal etype "LINE")
                            (setq x1 (nth 1 (assoc 10 elist))
                                  y1 (nth 2 (assoc 10 elist))
                                  z1 (nth 3 (assoc 10 elist))
                                  x2 (nth 1 (assoc 11 elist))
                                  y2 (nth 2 (assoc 11 elist))
                                  z2 (nth 3 (assoc 11 elist))
                            )
                            (list (/ (+ x2 x1) 2.0) (/ (+ y2 y1) 2.0) (/ (+ z2 z1) 2.0))  ; use 'z' coordinates in the future       
                          )
                          ((equal etype "AECB_DEVICE")
                            (vlax-get (vlax-ename->vla-object ename) 'Location)
                          )
                          ((or (equal etype "LWPOLYLINE") (equal etype "ARC") (equal etype "ELLIPSE") (equal etype "SHAPE") (equal etype "BLOCK"))
                            (cdr (assoc 10 elist))
                          )
                          (t nil)
                    )
      )
      (if center (command "scale" ename "" center factor) (princ "the object was not supported by this command. did not scale the object"))     
      (setq idx (1+ idx))
    )
  )
)