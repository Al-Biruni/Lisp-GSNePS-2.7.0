(in-package :sneps)
(defvar *initial-relations*           ;; added WHEN, DO, IF for SNeRE: dk
    ;; added act, action, precondition, effect for SNeRE: scs
    ;; added whenever for SNeRE: hi 3/31/99
    '(attachedfunction &ant ant arg forall cq dcq default emax emin etot exists
      max min pevb thresh threshmax
      when whenever do if vars suchthat 
      condition then else
      act plan goal action precondition effect
      object1 object2 prop grade))




(mapcar #'(lambda (ident)
	    (let ((ident&conv (list ident (intern (concatenate 'string (symbol-name ident) "-")))))
	      (export ident&conv)
	      (shadowing-import ident&conv (find-package 'snepsul)) 
	      (shadowing-import ident&conv (find-package 'snepslog)) 
	      (shadowing-import ident&conv (find-package 'snip)))) 
	*initial-relations*)

(export *initial-relations*)