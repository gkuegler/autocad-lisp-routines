;; Sourcefile for the following lisp macros:
;; 1. Postfix Selected
;; 2. Prefix Selected
;; 3. Increment Selected
;;
;; Prefix & postfix sets the the first occurring or last occurring number in a text or mtext objects.
;; Increment Selected increments the last occurring number by 1.
;; Useful for renaming or setting circuit homeruns.
;; 
;; Author: George Kugler
;; Date: 2022-08-19


(vl-load-com)

;; RegExpSet
;; Returns the current VBScript.RegExp instance after defining its properties.
;;
;; Arguments
;; pattern    : Pattern to search.
;; ignoreCase : If non nil, the search is done ignoring the case.
;; global     : If non nil, search all occurences of the pattern;
;;              if nil, only searches the first occurence.

(defun RegExpSet (pattern ignoreCase global / regex)
  (setq regex
         (cond
           ((vl-bb-ref '*regexp*))
           ((vl-bb-set '*regexp* (vlax-create-object "VBScript.RegExp")))
         )
  )
  (vlax-put regex 'Pattern pattern)
  (if ignoreCase
    (vlax-put regex 'IgnoreCase acTrue)
    (vlax-put regex 'IgnoreCase acFalse)
  )
  (if global
    (vlax-put regex 'Global acTrue)
    (vlax-put regex 'Global acFalse)
  )
  regex
)

;; RegexpTest
;; Return T if a match with the pattern is found in the string; otherwise, nil.
;;
;; Arguments
;; string     : String in which the pattern is searched.
;; pattern    : Pattern to search.
;; ignoreCase : If non nil, the search is done ignoring the case.
;;
;; Examples :
;; (RegexpTest "foo bar" "Ba" nil)  ; => nil
;; (RegexpTest "foo bar" "Ba" T)    ; => T
;; (RegExpTest "42C" "[0-9]+" nil)  ; => T

(defun RegexpTest (string pattern ignoreCase)
  (= (vlax-invoke (RegExpSet pattern ignoreCase nil) 'Test string) -1)  ; sets and returns the regex engine to invoke
)

;; RegExpExecute
;; Returns the list of matches with the pattern found in the string.
;; Each match is returned as a sub-list containing:
;; - the match value
;; - the index of the first character (0 based)
;; - a list of sub-groups.
;;
;; Arguments
;; string     : String in which the pattern is searched.
;; pattern    : Pattern to search.
;; ignoreCase : If non nil, the search is done ignoring the case.
;; global     : If non nil, search all occurences of the pattern;
;;              if nil, only searches the first occurence.

;;
;; Examples
;; (RegExpExecute "foo bar baz" "ba" nil nil)               ; => (("ba" 4 nil))
;; (RegexpExecute "12B 4bis" "([0-9]+)([A-Z]+)" T T)        ; => (("12B" 0 ("12" "B")) ("4bis" 4 ("4" "bis")))
;; (RegexpExecute "-12 25.4" "(-?\\d+(?:\\.\\d+)?)" nil T)  ; => (("-12" 0 ("-12")) ("25.4" 4 ("25.4")))

(defun RegExpExecute (string pattern ignoreCase global / sublst lst)
  (vlax-for match (vlax-invoke (RegExpSet pattern ignoreCase global) 'Execute string)
    (setq sublst nil)
    (vl-catch-all-apply
      '(lambda ()
   (vlax-for submatch (vlax-get match 'SubMatches)
     (if submatch
       (setq sublst (cons submatch sublst))
     )
   )
       )
    )
    (setq lst (cons (list (vlax-get match 'Value)
        (vlax-get match 'FirstIndex)
        (reverse sublst)
        )
        lst
        )
    )
  )
  (reverse lst)
)

;; RegExpReplace
;; Returns the string after replacing matches with the pattern
;;
;; Arguments
;; string     : String in which the pattern is searched.
;; pattern    : Pattern to search.
;; newStr     : replacement string.
;; ignoreCase : If non nil, the search is done ignoring the case.
;; global     : If non nil, search all occurences of the pattern;
;;              if nil, only searches the first occurence.
;;
;; Examples :
;; (RegexpReplace "foo bar baz" "a" "oo" nil T)                  ; => "foo boor booz"
;; (RegexpReplace "foo bar baz" "(\\w)\\w(\\w)" "$1_$2" nil T)   ; => "f_o b_r b_z"
;; (RegexpReplace "$ 3.25" "\\$ (\\d+(\\.\\d+)?)" "$1 €" nil T)  ; => "3.25 €"

(defun RegExpReplace (string pattern newStr ignoreCase global)
  (vlax-invoke (RegExpSet pattern ignoreCase global) 'Replace string newStr)
)

(defun IncrementText (text increment_value / result start number)
  ; (setq text "hello LV1-20 hh")
  (setq result (car (RegExpExecute text "(\\d+)[^0-9]*$" nil nil)))   ; => (("20 hh" 10 ("20")))
  (if result
    (progn 
      (setq start (nth 1 result) number (itoa (+ increment_value (atoi (car (nth 2 result))))))
  	  (strcat (substr text 1 start) number (substr text (+ start (strlen number) 1) (strlen text)))
    ) ; return the incremented string
    nil  ; return nothing
  )
)

(defun PostfixText (text new_number / result start text_length)  ; text and new_number should be strings
  (setq result (car (RegExpExecute text "(\\d+)[^0-9]*$" nil nil)))   ; => (("20 hh" 10 ("20")))
  (if result
    (progn 
      (setq start (nth 1 result) text_length (strlen (car (nth 2 result))))
      (strcat (substr text 1 start) new_number (substr text (+ start text_length 1) (strlen text)))
    ) ; return the post fixed string
    nil  ; return nothing, couldn't find a number to replace
  )
)

; there is no way to get the start position of a subgroup
; I have to serialize the searches until my desired subgroup ends up first
(defun PrefixText (text new_number / result start text_length)  ; text and new_number should be strings
  (setq result (car (RegExpExecute text "^[^0-9]*\\d+" nil nil)))   ; => (("20 hh" 10 ("20")))
  (if result
    (progn                                                                                                                                                                                                          
      (setq subgroup (car (RegExpExecute text "\\d+" nil nil))
            start (+ (nth 1 result) (nth 1 subgroup))
            text_length (strlen (car (nth 2 subgroup)))
      )
      (strcat (substr text 1 start) new_number (substr text (+ start text_length 1) (strlen text)))
    ) ; return the post fixed string
    nil  ; return nothing, couldn't find a number to replace
  )
)

;; Postfix Selected Items
(defun c:pfix (/ out)
  (setq ss (ssget)  ; select objects
        new_number (getstring "new number: ")
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
        (setq object (vlax-ename->vla-object ename) ; Get the ActiveX object of the entity
              text (vla-get-TextString object)
              new_text (PostfixText text new_number)
        )
        (if new_text (vla-put-TextString object new_text) (princ "The text could not be post fixed.\n")) ; Change the text inside the MTex
      )
      (princ "The object was not a text object.\n")
    )
    (setq idx (1+ idx))
  )
  (princ)
)

;; Prefix Selected Items
(defun c:lfix (/ out)
  (setq ss (ssget)  ; select objects
        new_number (getstring "new number: ")
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
        (setq object (vlax-ename->vla-object ename) ; Get the ActiveX object of the entity
              text (vla-get-TextString object)
              new_text (PrefixText text new_number)
        )
        (if new_text (vla-put-TextString object new_text) (princ "The text could not be pre-fixed.\n")) ; Change the text inside the MTex
      )
      (princ "The object was not a text object.\n")
    )
    (setq idx (1+ idx))
  )
  (princ)
)

;; Increment Selected Items
(defun c:incr (/ out)
  (setq ss (ssget)  ; select objects
        increment_value (getint "increment value: ")
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
        (setq object (vlax-ename->vla-object ename) ; Get the ActiveX object of the entity
              text (vla-get-TextString object)
              new_text (IncrementText text increment_value)
        )
        (if new_text (vla-put-TextString object new_text) (princ "The text could not be incremented.\n")) ; Change the text inside the MTex
      )
      (princ "The object was not a text object.\n")
    )
    (setq idx (1+ idx))
  )
  (princ)
)