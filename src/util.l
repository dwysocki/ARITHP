(defun compose (&rest functions)
  "Compose FUNCTIONS right-associatively, returning a function"
  #'(lambda (x)
      (reduce #'funcall functions
              :initial-value x
              :from-end t)))

(defun singleton? (x)
  (= 1 (length x)))

(defun doubleton? (x)
  (= 2 (length x)))

(defun numbers? (x)
  (every #'numberp x))
