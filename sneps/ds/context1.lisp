;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2007
;; Research Foundation of State University of New York

;; Version: context1.lisp,v 1.5 1994/08/19 22:52:59 snwiz Exp

;; This file is part of SNePS.

;; $BEGIN LICENSE$

;; 
;; The contents of this file are subject to the University at
;; Buffalo Public License Version 1.0 (the "License"); you may
;; not use this file except in compliance with the License. You
;; may obtain a copy of the License at http://www.cse.buffalo.
;; edu/sneps/Downloads/ubpl.pdf.
;; 
;; Software distributed under the License is distributed on an
;; "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
;; or implied. See the License for the specific language gov
;; erning rights and limitations under the License.
;; 
;; The Original Code is SNePS 2.7.
;; 
;; The Initial Developer of the Original Code is Research Foun
;; dation of State University of New York, on behalf of Univer
;; sity at Buffalo.
;; 
;; Portions created by the Initial Developer are Copyright (C)
;; 2007 Research Foundation of State University of New York, on
;; behalf of University at Buffalo. All Rights Reserved.
;; 
;;  
;; 
;; 


;; $END LICENSE$


(in-package :sneps)


; =============================================================================
;
; print.ct
; --------
;
;       arguments     : context - <context>
;                       outunit - <unit>
;
;       returns       : nil
;
;       description   : It Prints <context> to <outunit> so that it can later
;                       be reconstructed with read.ct.
;
;       side-effects  : It prints the <context>
;
;                                        written :  njm  09/26/88  
;                                        modified:  hc   06/29/93
;                                        modified:  scs  04/23/96
;                                        modified:  hi   03/24/99
;
(defun print.ct (context outunit)
  (with-readable-nodes-and-contexts
      (format outunit "~%~S~%~S~%~S~%~S~%~S"
	      (context-ca context)
	      ;(context-order context) ;HI 3/24/99
	      (context-names context)
	      (context-hyps context)
        (context-gprops context) ; nea 7/15
        (context-telprops context) ; nea 7/15
	      (context-restriction context)
	      (context-kinconsistent context))))

;
; =============================================================================
;
; read.ct
; -------
;
;       arguments     : inunit - <unit>
;
;       returns       : nil
;
;       description   : It reads and reconstructs a context from <inunit>,
;                       assuming that the information was printed by print.ct.
;                       Right now it does NOT update any system variables
;                       (more efficient, it's only used in in/outnet).
;
;       side-effects  : It constructs the <context>.
;
;                                        written :  njm  09/27/88  
;                                        modified:  njm  10/13/88
;                                                    hc  06/29/93
;                                                    hi  03/24/99
;
(defun read.ct (inunit)
  (let* ((name (read inunit))
	 (context (c^ name)))
    ;; Reconstruct context:
    (setf (context-ca context) name)
    ;(setf (context-order context) (read inunit))
    (setf (context-order context) (incridctcounter)) ;HI 3/24/99
    (setf (context-names context) (read inunit))
    (setf (context-hyps context) (read inunit))
    (setf (context-gprops context) (read inunit)) ; nea 7/15
    (setf (context-telprops context) (read inunit))
    (setf (context-restriction context) (read inunit))
    (setf (context-kinconsistent context) (read inunit))
    ;; Hash it:
    (setf (gethash (context-hyps context) (value.sv 'contexts)) context)
    ;; Assume that this is only used for in/outnet, hence,
    ;; we don't have to handle system variables such as context names.
    ))



    
    




