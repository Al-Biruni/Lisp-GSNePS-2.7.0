;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SNEPS; Base: 10 -*-

;; Copyright (C) 1984--2007
;; Research Foundation of State University of New York

;; Version: context2.lisp,v 1.7 1995/04/20 00:44:44 snwiz Exp

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
; Data Type:  <context>
;
; -----------------------------------------------------------------------------
;
; RECOGNIZERS    is.ct :           <universal> --> <boolean>
;
; CONSTRUCTORS   addtocontext     : <context> <node set> --> <context>
;                buildcontext     : <node set> --> <context>
;                fullbuildcontext : <node set> <context set> --> <context>                
;
; SELECTORS      --------- standard structure functions ---------
;
; TESTS          getcontext    : <node set> --> <boolean>
;                isinconsis.ct : <context>  --> <boolean>
;                isconsis.ct   : <context>  --> <boolean>
;
; UTILITY        name.ct       : <context> x <svar> --> <context>
;                removename.ct : <context> x <svar> --> <context>
;                addrestr.ct   : <context> x <context> --> <context>
;                iseq.ct       : <context> x <context> --> <boolean>
;                isless.ct     : <context> x <context> --> <integer> | NIL               
;                issubset.ct   : <context> x <context> --> <boolean>
;                allct         : <svar> --> <context set>
;                make-ct-set   : <node set> <context set> --> <context set>
;                make-hyp-set  : <node set> <context set> --> <node set>
;
;                contextaccess: <context> --> <atom>
;                print.ct     : 
;                read.ct      :
;                describe.ct  :
;                descrcontext : <context> --> <sequence>
;
; =============================================================================






;
; ==============================================================================
;
;  FUNCTIONS TO BE USED BY THE CONSTRUCTORS
;
; ==============================================================================
;


;
;
; ==============================================================================
;
; incridctcounter
; ---------------
;
;       arguments     : none
;
;       returns       : <integer>
;
;       description   : Increments the integer suffix associated with the context identifier 
;                       prefix `c'. Returns the incremented suffix.
;
;       side-effects  : Side effects the property list of the gennewcontext symbol.
;
;
;                                        written :  njm  09/14/88 
;                                        modified:      
;
;
;
(defmacro incridctcounter ()
  "Increments the integer suffix associated with the context identifier 
   prefix `c'. Returns the incremented suffix"
   `(setf (get 'gennewcontext 'c)
          (lisp::+ (get 'gennewcontext 'c) 1)))

;
;
; ==============================================================================
;
; idctcounter
; -----------
;
;       arguments     : none
;
;       returns       : <integer>
;
;       description   : Returns the integer last used for a context identifier 
;                       with prefix `C'.
;
;
;
;                                        written :  njm  09/14/88 
;                                        modified:      
;
;
;
(defmacro idctcounter ()
  "Returns the integer last used for a context identifier with prefix `C'."
  `(get 'gennewcontext 'c))




;
;
; ==============================================================================
;
; context
; -------
;
;
;
;        arguments     : ident - <identifier> 
;
;        returns       : <context> or NIL
;
;        description   : Given an <identifier> "ident", it returns the
;                        <context> whose <identifier> is "ident".
;
;                                        written :  njm  09/14/88  
;                                        modified: 
;
(defun context (ident)
  "Given an <identifier> ident, it returns the <context> whose <identifier> 
   is ident"
  (or (get ident '=scontext)
      ;; If context is all lowercase letters created by a grammar.
      (get (intern (nstring-downcase (princ-to-string ident))) '=scontext)
      ;; If context is capitalized as created by a grammar.
      (get (intern (string-capitalize (princ-to-string ident))) '=scontext)))

(defun c^ (context-name)
  ;; Yet another context referencer (for forward references during printing).
  ;; If a context with CONTEXT-NAME does not yet exist then a new empty and
  ;; unhashed context with that name gets built and returned.
  (or (context context-name)
      (setf (get context-name '=scontext)
	(make-context :ca context-name))))
;
; ==============================================================================
;
; newcontext
; ----------
;
;       arguments     : ident  - <context identifier>
;                       hypset - <node set>
;
;       returns       : <context> | NIL
;
;       description   : Creates and returns a new context with identifier `ident'.
;                       Returns NIL if a context with that identifier already exists.
;
;       side-effects  : It side-effects the property list of the gennewcontext
;                       atom and the context hash table. The `contexts' slot of the 
;                       nodes present in `hypset' is updated.
;
;
;                                        written :  njm  09/14/88 
;                                        modified:  mrc  10/18/88    
;                                                   nea 7/15
;
;
(defmacro newcontext (ident order hypset &optional gprops telprops)
  "Creates and returns a new context with identifier `ident' and hyps `hypset'.
   Returns NIL if a context with that identifier already exists."
  `(unless (context ,ident)
     (let ((ct (make-context :order order :ca ,ident :hyps ,hypset :gprops ,gprops :telprops ,telprops)))
       (setf (get ,ident '=scontext) ct)
       (setf (gethash ,hypset (value.sv 'contexts)) ct)
       (mapcar #'(lambda (nn) (setf (node-contexts nn)
				    (insert.cts ct (node-contexts nn))))
	       ,hypset)
       ct)))

  
;
;
; ==============================================================================
;
; gennewcontext
; -------------
;
;       arguments     : hypset - <node set>
;
;       returns       : <context>
;
;       description   : Generates and returns a new context 
;                       which hyps has the value "hypset".
;
;       side-effects  : It side-effects the property list of the gennewcontext
;                       atom and the context hash table. The `contexts' slot of the 
;                       nodes present in `hypset' is updated.
;
;
;                                        written :  njm  09/14/88 
;                                        modified:  njm  10/13/88    
;                                        modified:  mrc  10/18/88
;                                                   hc   05/17/89
;                                        modified:  scs  04/22/96
;                                                   nea 7/15
;
;
(defmacro gennewcontext (hypset &optional gprops telprops)
  "Generates and returns a context node which slot hyps slot has the value 
   hypset"
  `(do* ((order (incridctcounter) (incridctcounter))
	 (id (intern (format nil "C~d" order))
	     (intern (format nil "C~d" order)))
	 (result (newcontext id order ,hypset ,gprops ,telprops)
		 (newcontext id order ,hypset ,gprops ,telprops)))
	(result result)))


;; keeps track of the last integer used to form a context identifier 
;; with the prefix `c'.
(setf (get 'gennewcontext 'c) 0)

; ==============================================================================
;
; make-hyp-set
; ------------
;
;       arguments     : hypset - <node set>
;                       ctset  - <context set>                      
;
;       returns       : <node set>
;
;       description   : Computes a <node set> whose nodes are
;                       all the hyps in 'hypset' as well as all 
;                       the hyps in the contexts in 'ctset'.
;
; 
;                                            written:  mrc 10/18/88

(defun make-hyp-set (hypset ctset)
  (cond ((isnew.cts ctset) hypset)
	(t (union.ns (context-hyps (choose.cts ctset))
		     (make-hyp-set hypset (others.cts ctset))))))
;
;
; ==============================================================================
;
; getcontext 
; -----------
;
;        arguments      : hypset - <node set>
;
;        returns        : <context>
;
;        description    : If already exists a <context> which hyps are "hypset"
;                         returns that context, otherwise returns nil. 
;
;
;                                        written :  njm  09/16/88 
;                                        modified:  
;
;
(defmacro getcontext (hypset)
  "If already exists a <context> which hyps are `hypset' returns that context, 
   otherwise returns nil"
  `(gethash ,hypset (value.sv 'contexts))
)

; ==============================================================================
;
; make-ct-set
; ------------
;
;       arguments     : hypset - <node set>
;                       ctset  - <context set>                      
;
;       returns       : <context set>
;
;       description   : Computes a <context set> whose contexts are all 
;                       the contexts in 'ctset', as well as all the contexts
;                       ??????
;
; 
;                                            written:  mrc 10/18/88

(defun make-ct-set (hypset ctset)
  (cond ((isnew.ns hypset) ctset)
	(t (insert.cts (getcontext (makeone.ns (choose.ns hypset)))
		       (make-ct-set (others.ns hypset) ctset)))))

;
;
; ==============================================================================
;
; isinconsis.ct
; -------------
;
;       arguments     : c - <context>
;
;       returns       : <boolean>
;
;       description   : It returns "true" if "c" is a inconsistent <context>,
;                       or "false" otherwise.
;
;                                        written :  njm  09/14/88  
;                                        modified:  scs  05/28/07
;
;
(defun isinconsis.ct (c)
  "It returns true if `c' is a inconsistent <context>, or false otherwise"
  ;; Memo function using the kinconsistent slot of c
  (cond ((%context-kinconsistent c))
	((member (getcontext (new.ns)) (context-restriction c)
		 :test #'eq)
	 (setf (%context-kinconsistent c) t))
	(t nil)))



; ==============================================================================
;
; fullbuildcontext
; ----------------
;
;       arguments     : hypset - <node set>
;                       ctset  - <context set>                      
;
;       returns       : <context>
;
;       description   : Generates and returns a new context which hyps are
;                       the hyps in 'hypset', as well as the hyps in the
;                       contexts in 'ctset'.
;                       If such a context already exists the old context 
;                       is returned.
;                       The restriction set of the new context is computed.
;                       This computation is based on the restriction sets
;                       of the hyps in 'hypset' and of the contexts in 'ctset'.
;
;       side-effects  : It side-effects the property list of the gennewcontext
;                       atom and the context hash table. The `contexts' slot of 
;                       the nodes present in `hypset' is updated.
;
; 
;                                        written :  mrc  10/18/88    
;                                        modified:  nea 7/15
;
(defun fullbuildcontext (hypset ctset &optional gprops telprops)
  "Generates and returns a new context which hyps are the hyps in 'hypset', 
   as well as the hyps in the contexts in 'ctset'.If such a context already 
   exists the old context is returned."                      
  (let* ((hyps (make-hyp-set hypset ctset))
	  (exists (getcontext hyps)))
     (unless exists
       (setq exists (buildcontext hyps gprops telprops))
       (set-rs-union exists (make-ct-set hypset ctset))
       (if (isinconsis.ct exists) (updateall exists)))
     exists))
;
;
; ==============================================================================
;
; buildcontext
; ------------
;
;       arguments     : hypset - <node set>                      
;
;       returns       : <context>
;
;       description   : Generates and returns a new context which hyps are
;                       HYPSET. If such context already exists the old context 
;                       is returned.  
;
;       side-effects  : It side-effects the property list of the gennewcontext
;                       atom and the context hash table. The `contexts' slot of 
;                       the nodes present in `hypset' is updated.
;
;
;
;                                        written :  njm  09/16/88 
;                                        modified:  mrc  10/18/88    
;                                                   nea 7/15
;
(defun buildcontext (hypset &optional gprops telprops)
  "Generates and returns a new context which hyps are HYPSET. If such context
   already exists the old context is returned"
  ;(or (getcontext hypset)
      (gennewcontext hypset gprops telprops));)

;
;
; ==============================================================================
;
;                         DEITAR FORA ?
; addtocontext.ct
; ---------------
;
;       arguments     : ct - <context>
;                       hypset - <node set>
;
;       returns       : <context>
;
;       description   : If "hyp" is present in the hyps slot of "ct", "ct" is 
;                       returned. Otherwise a new context is created and 
;                       returned. In the last case the hyps context slot is set
;                       with the union of "hyp" with the contents of hyps "ct"
;                       slot. The restriction of the created context is computed
;          
;       side-effects  : It side-effects the property list of the gennewcontext
;                       atom and the context hash table. The `contexts' slot of 
;                       the nodes present in `hypset' is updated.
;
;
;                                        written :  njm  09/16/88 
;                                        modified:  njm  10/13/88    
;
;
;(defmacro addtocontext.ct (ct hypset)
;  "If hyp is present in the hyps slot of ct, ct is returned. Otherwise a
;   new context is created and returned. In the last case the hyps context slot 
;   is set with the union of hyp with the contents of hyps ct slot"
;  `(if (issubset.ns ,hypset (context-hyps ,ct))
;       ,ct
;       (buildcontext (union.ns (context-hyps ,ct) ,hypset) t)))

;
; ==============================================================================
;
;  RECOGNIZERS
;
; ==============================================================================


;
;
; ==============================================================================
;
; is.ct 
; -----
;
;        arguments      : u - <universal>
;
;        returns        : <boolean>
;
;        description    : It returns "true" if "u" is a <context>, or "false"
;                         otherwise.
;
;                                        written :  njm  09/14/88 
;                                        modified:
;
;
(defun is.ct (u)
  (context-p u))


;
;
; ==============================================================================
;
; isconsis.ct
; -----------
;
;       arguments     : c - <context>
;
;       returns       : <boolean>.
;
;       description   : It returns "true" if "c" is a consistent <context>, or
;                       "false" otherwise.
;
;                                        written :  njm  09/14/88 
;                                        modified:
;
(defmacro isconsis.ct (c)
  "It returns true if `c' is a consistent <context>, or false otherwise"
  `(not (eq (choose.cts (context-restriction ,c)) (getcontext (new.ns)))))

;
; ==============================================================================
;
;  UTILITIES
;
; ==============================================================================
;
;
;
; name.ct
; -------
;
;       arguments     : ctx - <context>
;                       svr - <svar>
;
;       returns       : <context>
;
;       description   : It adds "svr" to the slot "name" of context "ctx".
;                       The <context> "ctx" becomes the value of the <svar> "svr".  
;                       If "svr" was in the name slot of the context "ctx" then
;                       nothing change.
;
;       side-effects  : If the value of the variable "svr" was a context different
;                       from "ctx", then the value of names slot of that context is 
;                       updated.
;
;
;                                        written :  njm  09/20/88 
;                                        modified:
;
;
(defun name.ct (ctx svr)
  "It adds `svr' to the slot `name' of context `ctx'.
   The <context> `ctx' becomes the value of the <svar> `svr'.  
   If `sv' was in the name slot of the context `ctx' then nothing changes"
  (progn (set.sv svr ctx)
	  (setf (context-names ctx) (insert.svs svr (context-names ctx)))	  
	  ctx))
;
;
; ==============================================================================
;
; removename.ct
; -------------
;
;       arguments     : c - <context>
;                       sv - <svar>
;
;       returns       : <context>
;
;       description   : It removes "sv" from the slot "name" of context "c". 
;                       If "sv" was not in the name slot the context "c" is  
;                       unchanged.
;
;
;
;                                        written :  njm  09/20/88 
;                                        modified:
;
;
(defun removename.ct (c sv)
  "It removes `sv' from the slot `name' of context `c'. 
   If `sv' was not in the name slot the context `c' is unchanged"
  (prog2
     (setf (context-names c) (remove.svs sv (context-names c))) 
     c))


;
;
;
; ==============================================================================
; issubset.ct
; -----------
;
;        arguments     : c1 - <context> 
;                        c2 - <context>
;
;        returns       : <boolean>
;
;        description   : Returns "true" if the hypothesis set of the context "c1" 
;                        is a subset of the hypothesis set of the context "c2",
;                        False otherwise.   
;
;                                        written :  njm  09/17/88 
;                                        modified:  mrc 11/14/88
;                                                   nea 7/15
;
;
(defmacro issubset.ct (c1 c2)
  "Returns true if the hypothesis set of the context c1 is a subset of the 
   hypothesis set of the context c2, False otherwise"
  `(and ;(all-asserted.ns (context-hyps ,c1))
      ;(all-asserted.ns (context-gprops ,c2)) 
      (issubset.ns (context-hyps ,c1) (context-hyps ,c2))
      (issubset.ns (context-telprops ,c1) (context-telprops ,c2))
    ))
;
;
; ==============================================================================
;
; iseq.ct
; -------
;
;       arguments     : c1 - <context>
;                       c2 - <context>
;
;       returns       : <boolean>
;
;       description   : It returns "true" if c1 and c2 are the same context,
;                       "false" otherwise.
;
;                                        written :  njm  09/14/88  
;                                        modified:
;
(defun iseq.ct (c1 c2)
  "Returns true if `c1' and `c2' are the same context, false otherwise"
  (let ( (hyps1 (context-hyps c1))
         (hyps2 (context-hyps c2))
       )
  (or (eq c1 c2)
      (eq hyps1 hyps2)) 
  ))

;
; 
; ==============================================================================
;
; isless.ct
; ---------
;
;       arguments     : c1 - <context>
;                       c2 - <context>
;
;       returns       : <boolean>
;
;       description   : It returns "true" if c1 compares as less than c2;
;                       "false" otherwise.
;
;      
;
;                                        written :  njm  09/14/88
;                                        modified:  scs 04/22/96
;                                                    hi 05/16/99
;
(defun isless.ct (c1 c2)
  "Returns True if `c1' compares as less than `c2', False otherwise"                   
  (lisp:> (context-order c1) (context-order c2)))

;
;
; ==============================================================================
;
; allct
; -----
;
;       arguments     : none
;
;       returns       : <context set>
;
;       description   : Returns a set which elements are all the contexts 
;                       stored in the net.  
;
;
;                                        written :  njm  09/20/88  
;                                        modified:   hc  07/07/93
;                                                    hc  09/07/93
;
(defmacro allct ()
  "Returns a set which elements are all the contexts stored in the net"
  `(let ((ctset (new.cts)))
     (if (value.sv 'contexts)
	 (maphash #'(lambda (key val) (declare (ignore key))
			    (setq ctset (insert.cts val ctset)))
		  (value.sv 'contexts)))
     ctset))


;
;
; ==============================================================================
;
;   The following functions are particular to this implementation:
;
; ==============================================================================
;

;
; ==============================================================================
;
; contextaccess
; -------------
;
;        arguments     : ct - <context> 
;
;        returns       : <atom>
;
;        description   : It returns the interned <atom> used to access the 
;                        uninterned atom representing the <node> "n".
;
;                                        written:  njm 09/20/88
;                                        modified: 
;
;
(defmacro contextaccess (ct)
  "It returns the interned <atom> used to access the uninterned atom
   representing the <node> `n'"
  `(context-ca ,ct))


;
; ==============================================================================
;
; describe.ct
; ----------
;
;       arguments     : c - <context>
;
;       returns       : <atom>
;
;       description   : It returns an <atom> which is a description of
;                       the <context> "c" to be printed.
;
;                                        written :  njm  09/14/88  
;                                        modified:  
;
(defun describe.ct (c)
  "It returns an  <atom> which is a description of the <context> `c' to be
   printed"
  c)



; ==========================================================================
;
; descrcontext 
; ------------
;
;      arguments     : ct - <context>
;
;      returns       : <sequence>
;
;      description   : it lists the description of the <context> "cts".
;
;
;                                         written :  njm 09/28/88
;                                         modified:  hc  10/2/90
;                                                    nea 7/15
;                                                   
;
(defmacro descrcontext (ct)
  "Lists the description of the <context> `ct'"
  `(list
     (list 'assertions
	   (mapcar #'(lambda (hyp)
		       (snip:slight-describe-or-surface hyp nil))
		   (context-hyps ,ct)))
     (list 'gprops
     (mapcar #'(lambda (hyp)
           (snip:slight-describe-or-surface hyp nil))
       (context-gprops ,ct)))
     (list 'telprops
     (mapcar #'(lambda (hyp)
           (snip:slight-describe-or-surface hyp nil))
       (context-telprops ,ct)))
     (list 'restriction 
	   (mapcar #'(lambda (c)
		       (mapcar #'(lambda (hyp)
				   (snip:slight-describe-or-surface hyp nil))
			       (context-hyps c)))
		   (context-restriction ,ct)))
     (list 'named (context-names ,ct))))

  

;
; =============================================================================
;
;
; describe-all-contexts 
; ---------------------
;
;
;       arguments     : file - <filename>
;
;       returns       : NIL
;
;       description   : Outputs the information concerning all contexts into 'file'.
;                       If this argument does not exist the output is made to the
;                       standard output port.
;
;       
;
;
;                                  written :  mrc 11/30/88 
;                                  modified:  njm/hc 05/12/89
;
;
(defun describe-all-contexts (&optional file)
  (declare (special outunit))
  (with-open-stream (outstream (if file
				   (open (user:sneps-translate file)
                                         :direction :output
					 :if-does-not-exist :create
					 :if-exists :new-version)
				   outunit))
    (do.cts (ct (allct))
      (describe-one-context ct outstream)))
  (values))

(defun describe-one-context (ct file)
  (format file "~%~%~T CONTEXT-CA: ~A~
                         ~%~T CONTEXT-NAMES: ~A~
                         ~%~T CONTEXT-HYPS: ~A~
                         ~%~T CONTEXT-GPROPS: ~A~
                         ~%~T CONTEXT-TELPROPS: ~A~
                         ~%~T CONTEXT-RESTRICTION: ~A"
	  (context-ca ct)
	  (context-names ct)
	  (context-hyps ct)
    (context-gprops ct)
    (context-telprops ct)
	  (context-restriction ct)))


  



    
    




