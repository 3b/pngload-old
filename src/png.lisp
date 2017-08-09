(in-package :mediabox-png)

(defclass parse-data ()
  ((buffer :reader buffer
           :initarg :buffer)
   (data :reader data
         :initform (make-png))
   (parse-tree :reader parse-tree)))

(defstruct png
  colour-type
  pallete-count)

(defun read-png-stream (stream)
  (with-fast-input (buffer nil stream)
    (let ((parse-data (make-instance 'parse-data :buffer buffer)))
      (with-slots (parse-tree) parse-data
        (setf parse-tree (parse parse-data :datastream)))
      parse-data)))

(defun read-png-file (path)
  (with-open-file (in path :element-type 'octet)
    (read-png-stream in)))
