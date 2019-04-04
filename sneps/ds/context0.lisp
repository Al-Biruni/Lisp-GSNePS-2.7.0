;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2007 Research Foundation of 
;;                          State University of New York

;; Version: context0.lisp,v 1.6 1993/06/04 06:23:31 snwiz Exp

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


;; Definition of the structure CONTEXT
;; ===================================
;;
;;                                        written :  njm  09/14/88 
;;                                        modified:  hc 7/13/92
;;                                                   nea 7/15
;;
;; Use the %context-... access functions as the actual access functions for
;; the context structure, and define the standard context-... access functions
;; in a way such that they can take structures as well as SNePSUL context
;; variables as their arguments (needed for easier implementation of
;; intensional contexts).
;;
(defstruct (context
	     (:print-function context-printer)
	     (:conc-name %context-)
	     (:predicate %context-p)
	     )
  (order nil)          ;; ordinal for ordering contexts
                       ;; added by scs 4/22/96
  (ca nil)             ;; Context access.
  (names nil)	       ;; Sneps variables set. Names used by the user
		       ;; to access this context.
  (hyps nil)           ;; Node set. Hypotheses defining this context.
  (gprops nil)         ;; Node set. Grading Propositions belonging to this context. 7/15
  (telprops nil)         ;; Node set. Telescoped supports in this context. added nea 7/15
  (grade nil)           ;; context-grade
  (restriction nil)    ;; Context set. Set of contexts that are
                       ;;  inconsistent with this context.
  (kinconsistent nil)) ;; Flag. T if this context is contradictory.

(defun context-printer (obj stream depth)
  (declare (ignore depth))
  (write-string (symbol-name (%context-ca obj)) stream))


;; Now define "tolerant" versions of the standard access functions
;; (the only difference is that now the access functions are defined as
;; macros which makes them non-funcallable, however, this can be fixed
;; if need arises):
;;
(defmacro get-context (context-or-name)
  `(cond ((%context-p ,context-or-name) ,context-or-name)
         ((is.sv ,context-or-name)
	  (value.sv ,context-or-name))))

;; Leave this one untouched for now, so it will only
;; return T for actual context structures:
(defun context-p (thing)
  (%context-p thing))

(defmacro context-ca (context-or-name)
  `(%context-ca (get-context ,context-or-name)))

(defmacro context-order (context-or-name)
  ;; scs 4/22/96
  `(%context-order (get-context ,context-or-name)))

(defmacro context-names (context-or-name)
  `(%context-names (get-context ,context-or-name)))

(defmacro context-hyps (context-or-name)
  `(%context-hyps (get-context ,context-or-name)))

;; nea 7/15
(defmacro context-gprops (context-or-name)
  `(%context-gprops (get-context ,context-or-name)))

;; nea 7/15
(defmacro context-telprops (context-or-name)
  `(%context-telprops (get-context ,context-or-name)))

;; nea 7/15
(defmacro context-grade (context-or-name)
  `(%context-grade (get-context ,context-or-name)))

(defmacro context-restriction (context-or-name)
  `(%context-restriction (get-context ,context-or-name)))

(defmacro context-kinconsistent (context-or-name)
  `(%context-kinconsistent (get-context ,context-or-name)))



    
    




