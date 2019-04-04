;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNIP; Base: 10 -*-

;; Copyright (C) 1984--2007 Research Foundation of 
;;                          State University of New York

;; Version: $Id: fnode.lisp,v 1.3 2007/08/21 01:54:35 mwk3 Exp $

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


; =============================================================================
;
; <flagged node> ::= ( <node> <support> < truth-flag > )
;
;     where    <truth-flag> ::=  'TRUE | 'FALSE | 'UNKNOWN | 'REQUESTED
;              <support>    ::=  (<ot> <os> ... <ot> <os>)
;
; -----------------------------------------------------------------------------
;
; RECOGNIZERS    is.fn      : <universal> --> <boolean>
;
; CONSTRUCTORS   make.fn    : <node> x <support> x <truth-flag> --> <flagged node>
;                merge.fn   : <flagged node> x <flagged node> --> <flagged node>
;
; SELECTORS      node.fn    : <flagged node> --> <node>
;                support.fn : <flagged node> --> <support>
;                flag.fn    : <flagged node> --> <truth-flag>
;
; TESTS          iseq.fn    : <flagged node> x <flagged node> --> <boolean>
;
; =============================================================================
;
; is.fn
; -----
;
;       arguments     : u - <universal>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "u" is a <flagged node>, "false"
;                       otherwise
;
;                                        written :  rgh  2/08/86
;                                        modified:  rgh  4/03/86
;                                        modified:  cpf 10/06/88
;
(defmacro is.fn (u)
  `(and (listp ,u)
        (is.n (first ,u))
	(is.sup (second ,u))
        (member (third ,u) '(TRUE FALSE UNKNOWN REQUESTED) :test #'eq)))
;
;
; =============================================================================
;
; make.fn
; -------
;
;       arguments     : n - <node>
;                       sup - <support>
;                       flag - <truth-flag>
;
;       returns       : <flagged node>
;
;       description   : returns a <flagged node> constructed from a given
;                       node, support and truth-flag
;
;                                        written :  rgh  2/08/86
;                                        modified:  cpf 10/06/88
;
;
(defmacro make.fn (n sup flag)
  `(list ,n ,sup ,flag))
;
;
; =============================================================================
;
; node.fn
; -------
;
;       arguments     : fn - <flagged node>
;
;       returns       : <node>
;
;       description   : returns the <node> of "fn"
;
;                                        written :  rgh  2/08/86
;                                        modified:  cpf 10/06/88
;
;
(defmacro node.fn (fn)
  `(first ,fn))
;
;
; =============================================================================
;
; support.fn
; ----------
;
;       arguments     : fn - <flagged node>
;
;       returns       : <support>
;
;       description   : returns the <support> of "fn"
;
;                                        written :  cpf 10/06/88
;                                        modified:
;
;
(defmacro support.fn (fn)
  `(second ,fn))
;
;
; =============================================================================
;
; flag.fn
; -------
;
;       arguments     : fn - <flagged node>
;
;       returns       : <truth-flag>
;
;       description   : returns the <truth-flag> of "fn"
;
;                                        written :  rgh  2/08/86
;                                        modified:  cpf 10/06/88
;
;
(defmacro flag.fn (fn)
  `(third ,fn))
;
;
;
; =============================================================================
;
; iseq.fn
; -------
;
;       arguments     : fn1, fn2 - <flagged node>
;
;       returns       : <boolean>
;
;       description   : returns "true" if "fn1" and "fn2" are equal,
;                       "false" otherwise
;
;                                        written :  rgh  2/08/86
;                                        modified:  scs  5/10/88
;
;

(defmacro iseq.fn (fn1 fn2)
  `(and (iseq.n (node.fn ,fn1) (node.fn ,fn2))
	(iseq.sup (support.fn ,fn1) (support.fn ,fn2))
        (eq (flag.fn ,fn1) (flag.fn ,fn2))))
;
;
; =============================================================================
;
; merge.fn
; --------
;
;       arguments     : fn1, fn2 - <flagged node>
;
;       returns       : <flagged node>
;
;       description   : returns a flagged node which has a support corresponding
;                       to the merge of the supports of "fn1" and "fn2".
;
;                                        written :  cpf  10/18/88
;                                        modified: 
;
;
(defmacro merge.fn (fn1 fn2)
  `(make.fn (node.fn ,fn1)
	    (merge.sup (support.fn ,fn1) (support.fn ,fn2))
	    (flag.fn ,fn1)))



; =============================================================================



    
    




