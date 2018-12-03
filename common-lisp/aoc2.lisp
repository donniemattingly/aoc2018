(defun get-input ()
  (let 
      ((in (open "../inputs/input-2.1.txt" :if-does-not-exist nil))
       input)
    (when in
      (loop for line = (read-line in nil)
         while line do (push line input))
      (close in))
    input))


(defun update-counts (alist char)
  (let ((cur (assoc char alist :test #'string-equal)))
    (if cur
        (setf (cdr cur) (+ 1 (cdr cur)))
        (setf alist (acons char 1 alist))
        )
    alist)
  )

(defun repeat-counts (val)
  (let ((counts nil))
    (map nil 
         (lambda (c) 
           (setf counts (update-counts counts c))
           )
         val )
    counts 
    ))

(defun partone (input)
  (let ((twocount 0) (threecount 0))
    (mapc (lambda (str) (let ((counts (repeat-counts str)))
                          (if (rassoc 2 counts)
                              (setf twocount (+ 1 twocount)))
                          (if (rassoc 3 counts)
                              (setf threecount (+ 1 threecount)))
                          ))
          input)
    (princ twocount)
    (terpri)
    (princ threecount)
    (terpri)
    (* twocount threecount)
    ))

(defun string-to-charlist (str)
  (coerce str 'list))

(defun test-string (a b)
  (let ((achars (string-to-charlist a)) (bchars (string-to-charlist b)))
    (let ((diffcount (list-length (remove-if (lambda (x) (string-equal (car x) (cdr x))) (pairlis achars bchars)))))
      (= 1 diffcount))))

(defun get-common-chars (a b)
  (let ((achars (string-to-charlist a)) (bchars (string-to-charlist b)))
    (format nil "~{~A~^~}" 
            (reverse 
             (mapcar (lambda (x) (car x)) 
                     (remove-if-not (lambda (x) (string-equal (car x) (cdr x))) (pairlis achars bchars)))))))

(defun parttwo (input)
  (loop for x in input
     do (loop for y in input
           do (if (test-string x y) (print (get-common-chars x y))))))
