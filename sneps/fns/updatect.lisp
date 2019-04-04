;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2007 Research Foundation of 
;;                          State University of New York

;; Version: $Id: updatect.lisp,v 1.3 2007/08/21 01:54:31 mwk3 Exp $

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
;
; set-rs-union
; ------------
;
;
;       arguments     : ct - <context>
;                       cts - <context set>
;
;       returns       : <context>
;
;       description   : Updates the restriction set of the context 'ct',
;                       which was formed by the union of the contexts in
;                       'cts'. Returns 'ct'.
;                       
;       
;
;                                  written:   mrc  10/17/88
; 
;
(defun set-rs-union (ct cts)
  (setf (context-restriction ct)
	(rs-union cts ct))
  ct)

; =============================================================================
;
;
; rs-union
; --------
;
;
;       arguments     : cts - <context set>
;                       ct  - <context>
;                       
;
;       returns       : <context set>
;
;       description   : Implements the mu function of the SWM logic.ALMOST...
;                       
;       
;
;                                  written:   mrc  10/17/88
; 
;
;
(defun rs-union (cts ct)
  (sigma (psi (flatten-lcts
	       (mapcar #'(lambda (ct) (context-restriction ct)) cts))
	      ct)))

; =============================================================================
;
;
; flatten-lcts
; ------------
;
;
;       arguments     : lcts - list of <context set>                       
;
;       returns       : <context set>
;
;       description   : Flattens the list 'lcts' to a context set.
;                              
;
;                                  written:   mrc  10/17/88
; 
;
(defun flatten-lcts (lcts)
  (cond ((null lcts) (new.cts))
	(t (union.cts (first lcts)
		      (flatten-lcts (rest lcts))))))
; =============================================================================
;
;
; update-contexts 
; ---------------
;
;
;       arguments     : cts1 - <context set>
;                       cts2 - <context set>
;
;       returns       : NIL
;
;       description   : Updates the restriction ......
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;                                  modified:  mrc  10/14/88
; 
;
;(defun update-contexts (cts1 cts2)
;  (let ((newcontexts (new.cts)))
;    (declare (special newcontexts))
;    (cond ((isnew.cts cts1) nil) 
;	  (t (update-contexts-1 (choose.cts cts1) cts2)
;	     (update-contexts   (others.cts cts1) cts2)))
;    (mapc #'(lambda (c) (set-rs-union c (make-ct-set (context-hyps c) (new.cts)))
;;		    (if (isinconsis.ct c) (updateall c))
;		    )
;	    newcontexts)))

(defun update-contexts (cts1 cts2)
  (let ((newcontexts (new.cts)))
    (declare (special newcontexts))
    (dolist (ct cts1)
      (update-contexts-1 ct cts2))
    (mapc #'(lambda (c) (set-rs-union c (make-ct-set (context-hyps c) (new.cts)))
		    (if (isinconsis.ct c) (updateall c)))
	  newcontexts)))

(defun update-contexts-1 (ct cts) 
  (cond ((isnew.cts cts) nil) 
	(t (cond ((eq ct (getcontext (new.ns))) (mapcar #'mark-inconsistent cts) 
						(mapcar #'updateall cts)
						)
		 ((eq (choose.cts cts) (getcontext (new.ns))) (mark-inconsistent ct)
							      (updateall ct))
		 ((not (known-incompat ct (context-restriction (choose.cts cts)))) 
		  (update-contexts-2 (union.ns (context-hyps ct)
					       (context-hyps (choose.cts cts)))) 
		  (update-contexts-1 ct (others.cts cts)))))))

(defun mark-inconsistent (ct)
  (setf (context-restriction ct) (makeone.cts(getcontext (new.ns)))))

(defun update-contexts-2 (hypset) (update-hypotheses hypset hypset))


;
; =============================================================================
;
;
; update-hypotheses  
; -----------------
;
;
;       arguments     : hypset  - <node set>
;                       waiting - <node set>
;
;       returns       : NIL
;
;       description   : 
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;                                  modified:  mrc  10/14/88
;
;
;
(defun update-hypotheses (hypset waiting) 
  (cond ((isnew.ns waiting) (insert-rs (build-and-memorize hypset) (getcontext (new.ns))))
	(t (update-this-hyp (choose.ns waiting) 
			    (remove.ns (choose.ns waiting) hypset))
	   (update-hypotheses hypset (others.ns waiting)))))


;
; =============================================================================
;
;
; update-this-hyp  
; ---------------
;
;
;       arguments     : thishyp   - <node>
;                       otherhyps - <node set>
;
;       returns       : NIL
;
;       description   : 
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;                                  modified:  mrc  10/14/88
;
;
(defun update-this-hyp (thishyp otherhyps) 
  (if otherhyps (update-this-hyp-1 (node-contexts thishyp)
				   otherhyps)))


;(defun update-this-hyp-1 (cts otherhyps) 
;  (cond ((isnew.cts cts) nil)  
;	(t (update-rs-of (choose.cts cts)
;			 (build-and-memorize
;			   (compl.ns otherhyps
;				     (context-hyps (choose.cts cts))))) 
;	   (update-this-hyp-1 (others.cts cts) otherhyps))))

(defun update-this-hyp-1 (cts otherhyps) 
  (dolist (ct cts)
    (update-rs-of ct (build-and-memorize
			   (compl.ns otherhyps
				     (context-hyps ct)))))) 
	   
; =============================================================================
;
;
; build-and-memorize  
; ------------------
;
;
;       arguments     : hyps   - <node set>
;
;       returns       : <context>
;
;       nonlocal-vars : newcontexts
;
;       description   : If the context with hypotheses 'hyps' does not exist
;                       yet, it is created, and inserted in the variable
;                       'newcontexts'. In any case the context with hypotheses 
;                       'hyps' is returned.
;
;       
;
;                                  written:   mrc  10/14/88
;

(defun build-and-memorize (hyps)
  (declare (special newcontexts))
  (let ((ct (getcontext hyps)))
    (unless ct
      (setq ct (buildcontext hyps))
      (setq newcontexts (insert.cts ct newcontexts)))
    ct))

; =============================================================================
;
;
; ishyp  
; -----
;
;
;       arguments     : ct - <context>
;
;       returns       : <boolean>
;
;       description   : 
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun ishyp (ct)
  (eql (cardinality.ns (context-hyps ct)) 1))





;
; =============================================================================
;
;
; insert-rs 
; ---------
;
;
;       arguments     : newrs - <context>
;                       ct    - <context>
;
;       returns       : <context>
;
;       description   : returns CT
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun insert-rs (newrs ct) 
  (setf (context-restriction ct)
	(sigma (insert.cts newrs (context-restriction ct))))
  ct)

;
; =============================================================================
;
;
; remove-rs 
; ---------
;
;
;       arguments     : newrs - <context>
;                       ct    - <context>
;
;       returns       : <context>
;
;       description   : returns CT
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun remove-rs (newrs ct) 
  (setf (context-restriction ct)
	(remove.cts newrs (context-restriction ct)))
  ct)



;
; =============================================================================
;
;
; update-rs-of 
; ------------
;
;
;       arguments     : ct    - <context>
;                       newrestr - <context>
;
;       returns       : 
;
;       description   : restr = new restriction to be added to `ct'
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun update-rs-of (ct newrestr)
  (insert-rs newrestr ct))



 
;  (let ((ctrestr (context-restriction ct)))
;    (cond ((ismemb.cts newrestr ctrestr))
;	  ((supperfulus? ctrestr newrestr))
;	  ((simple-addition? ctrestr newrestr) (insert-rs newrestr ct)) 
;	  (t (re-structure-rs ct newrestr)))))





;
; =============================================================================
;
;
; supperfulus?  
; ------------
;
;
;       arguments     : oldrestr - <context set>
;                       restr    - <context>
;
;       returns       : <boolean>
;
;       description   : 
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun supperfulus? (oldrestr restr)
  (cond ((isnew.cts oldrestr) nil)
	((issubset.ct (choose.cts oldrestr) restr) t)
	(t (supperfulus? (others.cts oldrestr) restr))))



;
; =============================================================================
;
;
; simple-addition?  
; ----------------
;
;
;       arguments     : oldrestr - <context set>
;                       restr    - <context>
;
;       returns       : <boolean>
;
;       description   : 
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun simple-addition? (oldrestr restr)
  (cond ((isnew.cts oldrestr) t)
	((issubset.ct restr (choose.cts oldrestr)) nil)
	(t (simple-addition? (others.cts oldrestr) restr))))



;
; =============================================================================
;
;
; re-structure-rs 
; ---------------
;
;
;       arguments     : ct    - <context>
;                       newrs - <context>
;
;       returns       : 
;
;       description   : 
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun re-structure-rs (ct newrs)
  (re-structure-rs-1 ct (context-restriction ct) newrs))
      

(defun re-structure-rs-1 (ct ctrestr newrs)
  (cond ((issubset.ct newrs (choose.cts ctrestr))
	 (remove-rs (choose.cts ctrestr) ct)
	 (insert-rs newrs ct))
	((issubset.ct (choose.cts ctrestr) newrs) nil)
	(t (re-structure-rs-1 ct (others.cts ctrestr) newrs)))) 




;
; =============================================================================
;
;
; known-incompat  
; --------------
;
;
;       arguments     : ct1   - <context>
;                       ct2rs - <context set>
;
;       returns       : <boolean>
;
;       description   : 
;
;       
;
;
;                                  written :  jpm 12/02/82 
;                                  modified:  njm 10/06/88
;
;
;
(defun known-incompat (ct1 ct2rs)
  (ismemb.cts ct1 ct2rs))



;(trace known-incompat re-structure-rs-1 re-structure-rs insert-rs remove-rs update-this-hyp-1 update-hypotheses ck-contradiction implement-sneps-option) 



    
    




