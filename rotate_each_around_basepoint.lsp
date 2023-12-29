;* Rotate Multiple
;* Rotates many entities around their respective basepoints
;* allows selection by AUTOCAD selection sets or SSX.
;* Written by David Husch, January 1991

; SUPPORTED ENTITIES:
; note: default for objects is entity list, key=10, is basepoint
; ARC
; BLOCK => yes, basepoint.
; CIRCLE => rotating a circle is pointless, not supporting.
; ELLIPSE
; LEADER => '10' is vertex points, not supporting for now.
; LINE => '10' is start point, '11' is end point.
; LWPOLYLINE => yes, this is what a rectangle is labeled as.
; HATCH => this one is weird '10' is the elevation point, not supporting for nowj
; MLINE => same as line, supporting.
; POLYLINE => more complicated, not supporting for now.
; SHAPE
; AECB_DEVICE => all mep devices, complicated, need to get property from vl library, supporting, see below.

(defun c:rotatebase (/ ss ssl ang idx ename elist etype x1 x2 y1 y2 center)
  (prompt "Select Entities to Rotate, <ENTER> for SSX.")
  (setq ss (ssget)  ; select objects
        ang (getreal "Angle: ")  ; prompt user for rotation angle
        ; mode (getstring t "Copy or Rotate: "))  ; prompt user for copy or no
        ssl (sslength ss)
        idx 0
  )
  (if ss 
    (repeat ssl
      (setq ename (ssname ss idx)
            elist (entget ename)
            etype (cdr (assoc 0 elist))
            center (cond ((equal etype "LINE")  ; get center point (eg. (x y z)) depending on entity type
                            (setq x1 (nth 1 (assoc 10 elist))
                                  y1 (nth 2 (assoc 10 elist))
                                  z1 (nth 3 (assoc 10 elist))
                                  x2 (nth 1 (assoc 11 elist))
                                  y2 (nth 2 (assoc 11 elist))
                                  z2 (nth 3 (assoc 11 elist))
                            )
                            (list (/ (+ x2 x1) 2.0) (/ (+ y2 y1) 2.0) (/ (+ z2 z1) 2.0))
                          )
                          ((equal etype "AECB_DEVICE")
                            (vlax-get (vlax-ename->vla-object ename) 'Location)
                          )
                          ((or (equal etype "LWPOLYLINE") (equal etype "ARC") (equal etype "ELLIPSE") (equal etype "SHAPE") (equal etype "BLOCK"))
                            (cdr (assoc 10 elist))
                          )
                          (t nil)  ; nonsupported entities return nil
                    )
      )
      (if center (command "rotate" ename "" center ang) (princ "the object was not supported by this command. did not rotate the object"))     
      (setq idx (1+ idx))
    )
  )
)