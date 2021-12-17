(let ((in (open "/some/file/name.txt")))
  (format t "~a~%" (read-line in))
  (close in))
