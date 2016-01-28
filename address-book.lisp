#|Author: anonmu|#

(defvar *db* nil)

(defun address-book (firstname lastname age email)
  `(:firstname ,firstname :lastname ,lastname :age ,age :email ,email))

(defun add-new-contact (nc) (push nc *db*))

(defun dump-db ()
  (dolist (nc *db*)
    (format t "~{~a:~10t~a~%~}~%" nc)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-info ()
  (address-book
   (prompt-read "First name")
   (prompt-read "Last name")
   (or (parse-integer (prompt-read "Age") :junk-allowed t) 0)
   (prompt-read "e-mail")))

(defun add-contacts ()
  (loop (add-new-contact (prompt-for-info))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun where (&key firstname lastname age email)
  #'(lambda (nc)
      (and
       (if firstname  (equal  (getf nc :firstname) firstname)  t)
       (if lastname   (equal  (getf nc :lastname)  lastname)   t)
       (if age        (equal  (getf nc :age)       age)        t)
       (if email      (equal  (getf nc :email)     email)      t))))

(defun update (selector-fn &key firstname lastname age email)
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if firstname   (setf (getf row :firstname) firstname))
               (if lastname    (setf (getf row :lastname)  lastname))
               (if age         (setf (getf row :age)       age))
               (if email       (setf (getf row :email)     email)))
             row) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
