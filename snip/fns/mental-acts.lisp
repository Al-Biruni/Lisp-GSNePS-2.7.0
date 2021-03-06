;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1993--2007
;; Research Foundation of State University of New York

;; Version: $Id: mental-acts.lisp,v 1.3 2007/08/21 01:54:40 mwk3 Exp $

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


(in-package :snip)


;;;
;;; Representation
;;;
;;;   believe-act = action believe
;;;         	    object1 prop
;;;   believe-act = action believe
;;;         	    object1 prop


;;; HC: Believing of the effects of an act should really be done simultaneously
;;; (or in parallel) instead of the current method that uses a DO-ALL in
;;; the function `schedule-believe-effects'.  The reason is that an act can
;;; have multiple effects, some of which might also be represented by
;;; rules.  For example, the rule Held(BLOCK) => ~Clear(BLOCK) in conjunction
;;; with the effects Held(BLOCK) and ~Clear(BLOCK) of the pickup action will
;;; lead to a contradiction if after picking up a block we first believe
;;; Held(BLOCK) and forward-chain on it, since that will derive ~Clear(BLOCK)
;;; which will contradict the still not disbelieved Clear(BLOCK).

;;; In preparation for changing this the `believe' function now uses
;;; `believe-propositions' which believes a set of propositions simultaneously.

;;; Changed FLJ/SCS 8/1/04 to do the following...
;;; Provides an error message, instead of causing a Lisp error, if no proposition.
(defun believe (believe-act)
  "Causes the propositions in object1(BELIEVE-ACT) to be believed."
  (if (isnew.ns (nodeset.n believe-act 'object1))
      (sneps:sneps-error "Attempt to perform believe with no proposition."
			 'snip 'believe)
    (believe-propositions (nodeset.n believe-act 'object1))))



;;; Want `believe P' always to do: retract ~P; assert P.
;;; Modified by flj on 2/12/04 for scs

;;; This version calls assert.n directly
;;;    rather than going via #! to avoid package problems.
(defun believe-propositions (propositions)
  "Causes PROPOSITIONS to be believed quasi-simultaneously.
   By `simultaneously' is meant that before believing any PROPOSITIONS
   all currently believed negations to any of them will be disbelieved,
   as well as all currently believed linked propositions."
  (let* ((sneps:*with-snepsul-eval-function*
	  ;; Make sure no tracing occurs in #! contexts:
          #'sneps:with-snepsul-standard-eval))
    ;; Disbelieve negations:
    (do.ns (proposition propositions)
	   (disbelieve-negation proposition)
	   (disbelieve-linked proposition))
    ;; Assert propositions:
    (do.ns (proposition propositions)
	   (cond ((isassert.n proposition)
		  (plantrace
		   "I already believe " (makeone.ns proposition) (new.restr)))
		 (t (assert.n proposition crntct)
		    (sneps:set.sv 'sneps:assertions
			    (insert.ns proposition
				       (sneps:value.sv 'sneps:assertions)))
		    (plantrace "Believe " (makeone.ns proposition) (new.restr)))))
    ;; Forward-chain on newly believed propositions:
    (dynamic-add* propositions)))

;;;  Added by flj on 2/12/04 for scs
(defun disbelieve-linked (proposition)
  "Retracts any believed propositions linked to PROPOSITION."
  (let ((linked-propositions
	 #!((- (find arg- 
		     (findassert max 1 arg ~proposition :context ~crntct))
	       ~proposition))))
    ;; Retract linked propositions
    (do.ns (linked-proposition linked-propositions)
	   (when (isassert.n linked-proposition)
	     (disbelieve-propositions (makeone.ns linked-proposition))))))


;;; Changed scs/flj 6/29/04 to use sneps:negate.n
;;; Changed scs/flj 8/01/04 for following reason...
;;; Now that sneps:contradictory-nodes.n uses find-negate instead of negate.n,
;;;    it might return an empty nodeset.
(defun disbelieve-negation (proposition
			    &aux
			    (not-prop (sneps:contradictory-nodes.n proposition)))
  "Retracts believed propositions that are contradictions of PROPOSITION."
  (unless (isnew.ns not-prop)
    (disbelieve-propositions 
     (sneps:remove-if-not.ns #'isassert.n not-prop))))

(defun disbelieve (disbelieve-act)
  "Causes the propositions in object1(DISBELIEVE-ACT) to be disbelieved."
  (disbelieve-propositions (nodeset.n disbelieve-act 'object1)))

;;;  Want `disbelieve P' always to do just retract P.
;;;  Modified by flj on 2/12/04 for scs

 ;;; This version does a clear-infer before disbelieving anything,
;;;    because processes aren't informed
;;;    when an antecedent is no longer believed.
(defun disbelieve-propositions (propositions)
  "Causes PROPOSITIONS to be disbelieved."
  (unless (isnew.ns propositions)
    (sneps:clear-infer)
    (let ((sneps:*with-snepsul-eval-function*
	   #'sneps:with-snepsul-standard-eval))
      (do.ns (proposition propositions)
	     (cond ((isassert.n proposition)
		    (cond ((ismemb.ns proposition (sneps:context-hyps crntct))
			   (plantrace "Disbelieve "
				      (makeone.ns proposition) (new.restr))
			   #3!((remove-from-context ~proposition ~crntct)))
			  (t (plantrace
			      "Can't disbelieve derived proposition "
			      (makeone.ns proposition) (new.restr)))))
		   (t (plantrace
		       "already didn't believe "
		       (makeone.ns proposition)
		       (new.restr))))))))

;;; For backward compatibility:

(defun forget (forget-act)
  "Alias for `disbelieve'."
  (disbelieve forget-act))



    
     ;;;
;;; Representation
;;;
;;;   adopt-act = action adopt
;;;         	    object1 prop
;;;   unadopt-act = action unadopt
;;;         	       object1 prop

(defun adopt (adopt-act)
  "Causes the policies in object1(ADOPT-ACT) to be adopted."
  ;;; Adoption is just like belief, but there is no negated policy.
  (if (isnew.ns (nodeset.n adopt-act 'object1))
      (sneps:sneps-error "Attempt to perform adopt with no policy."
			 'snip 'adopt)
    
    (let ((sneps:*with-snepsul-eval-function*
	    ;; Make sure no tracing occurs in #! contexts:
	   #'sneps:with-snepsul-standard-eval)
	  (policies (nodeset.n adopt-act 'object1)))
      ;; Adopt policies:
      (do.ns (policy policies)
	     (cond ((isassert.n policy)
		    (plantrace
		     "I already intend " (makeone.ns policy) (new.restr)))
		   (t #!((! ~policy :context ~crntct))
		      (plantrace "Adopt " (makeone.ns policy)
				 (new.restr)))))
      ;; Forward-chain on newly believed policies:
      (dynamic-add* policies))))


(defun unadopt (unadopt-act)
  "Causes the policies in object1(UNADOPT-ACT) to be unadopted."
  ;;; Unadoption is just like disbelief, but there is no negated policy.
  (let ((sneps:*with-snepsul-eval-function*
         #'sneps:with-snepsul-standard-eval))
    (do.ns (policy (nodeset.n unadopt-act 'object1))
	   (cond ((isassert.n policy)
		  (cond ((ismemb.ns policy (sneps:context-hyps crntct))
			 (plantrace "Unadopt "
				    (makeone.ns policy) (new.restr))
			 #3!((remove-from-context ~policy ~crntct)))
			(t (plantrace
			    "Can't unadopt derived policy "
			    (makeone.ns policy) (new.restr)))))
		 (t (plantrace
		     "already didn't intend "
		     (makeone.ns policy)
		     (new.restr)))))))


