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
