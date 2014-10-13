(load "util.l")

(defun evalexp (bindings exp)
  (simplify (bind-values bindings exp)))

(defun bind-values (bindings exp)
  (if (null bindings)
    exp
    (let* ((current-bindings (car bindings))
           (old              (first  current-bindings))
           (new              (second current-bindings)))
      (bind-values (cdr bindings)
                   (subst new old exp)))))

(defun simplify (exp)
  (if (and (listp exp) exp)
    (let* ((operator            (car exp))
           (operands            (cdr exp))
           (simplified-operands (mapcar #'simplify operands))
           (simplified-exp      (cons operator simplified-operands))
           (simplifier          (case operator
                                  ('+ #'simplify-add)
                                  ('-  #'simplify-sub)
                                  ('* #'simplify-mul)
                                  ('/ #'simplify-div))))
      (funcall simplifier simplified-exp))
    exp))


;; Expression builder functions
(defun addexp (a b)
  (list '+ a b))
(defun subexp (a b)
  (list '- a b))
(defun mulexp (a b)
  (list '* a b))
(defun divexp (a b)
  (list '/ a b))

;; Predicates
(defun zero? (x)
  (equal x 0))

(defun one? (x)
  (equal x 1))

(defun addexp? (exp)
  (when (listp exp)
    (equal '+ (car exp))))

(defun subexp? (exp)
  (when (listp exp)
    (equal '- (car exp))))

(defun mulexp? (exp)
  (when (listp exp)
    (equal '* (car exp))))

(defun divexp? (exp)
  (when (listp exp)
    (equal '/ (car exp))))

;; Simplifiers
(defun simplify-add (exp)
  (let ((operands (remove-if #'zero? (cdr exp))))
    (cond
     ((null       operands) 0)
     ((numbers?   operands) (eval exp))
     ((singleton? operands) (car operands))
     (T                     exp))))

(defun simplify-sub (exp)
  (let ((operands (remove-if #'zero? (cdr exp))))
    (cond
     ((null       operands) 0)
     ((numbers?   operands) (eval exp))
     ((singleton? operands) (cons '- operands))
     ((doubleton? operands) (if (apply #'eql operands)
                              0
                              exp))
     (T                     exp))))

(defun simplify-mul (exp)
  (let ((operands (remove-if #'one? (cdr exp))))
    (cond
     ((null       operands) 1)
     ((member 0   operands) 0)
     ((numbers?   operands) (eval exp))
     ((singleton? operands) (car operands))
     (T                     exp))))

(defun simplify-div (exp)
  (let ((operands (cdr exp)))
    (cond
     ((null          operands)  1)
     ((zero?    (car operands)) 0)
     ((member 0 (cdr operands)) (error "division by zero"))
     ((numbers?      operands)  (eval exp))
     (T                         exp))))
