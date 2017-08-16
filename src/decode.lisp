(in-package :pngload)

(defvar *decode-data* nil)

(defun get-image-bytes ()
  (with-slots (width height interlace-method) *png-object*
    (ecase interlace-method
      (:null
       (+ height (* height (get-scanline-bytes width))))
      (:adam7
       (loop :for (width height) :in (calculate-sub-image-dimensions)
             :sum (* height (1+ (get-scanline-bytes width))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +filter-type-none+ 0)
  (define-constant +filter-type-sub+ 1)
  (define-constant +filter-type-up+ 2)
  (define-constant +filter-type-average+ 3)
  (define-constant +filter-type-paeth+ 4))

(defun allocate-image-data ()
  (with-slots (width height color-type bit-depth transparency) *png-object*
    (let ((channels (ecase color-type
                      ((:truecolour :indexed-colour) 3)
                      (:truecolour-alpha 4)
                      (:greyscale-alpha 2)
                      (:greyscale 1))))
      (when transparency
        (assert (member color-type '(:truecolour :indexed-colour :greyscale)))
        (incf channels))
      (make-array `(,height ,width ,@(when (> channels 1) (list channels)))
                  :element-type (ecase bit-depth
                                  ((1 2 4 8) 'ub8)
                                  (16 'ub16))))))

(declaim (inline unfilter-sub))
(defun unfilter-sub (x data start pixel-bytes)
  (declare (ub8a1d data)
           (ub8 pixel-bytes)
           (ub32 x)
           (fixnum start)
           (optimize speed))
  #++(aref data (+ start (- x pixel-bytes)))
  (if (>= x pixel-bytes)
      (aref data (+ start (- x pixel-bytes)))
      0))

(declaim (inline unfilter-up))
(defun unfilter-up (x data start-up)
  (declare (ub8a1d data)
           (ub32 x)
           (fixnum start-up)
           (optimize speed))
  (aref data (+ x start-up)))

(declaim (inline unfilter-average))
(defun unfilter-average (x data start start-up pixel-bytes)
  (declare (ub8a1d data)
           (ub32 x)
           (fixnum start start-up)
           (ub8 pixel-bytes)
           (optimize speed))
  (let ((a (unfilter-sub x data start pixel-bytes))
        (b (unfilter-up x data start-up)))
    (declare (ub8 a b))
    (floor (+ a b) 2)))

(defun unfilter-average/1 (x data start pixel-bytes)
  ;; special case for first line
  (declare (ub8a1d data)
           (ub32 x)
           (fixnum start)
           (ub8 pixel-bytes)
           (optimize speed))
  (let ((a (unfilter-sub x data start pixel-bytes)))
    (declare (ub8 a))
    (floor a 2)))

(declaim (inline unfilter-paeth))
(defun unfilter-paeth (x data start-left start-up pixel-bytes)
  (declare (ub8a1d data)
           (ub32 x)
           (fixnum start-left start-up)
           (ub8 pixel-bytes)
           (optimize speed))
  (let* ((a (unfilter-sub x data start-left pixel-bytes))
         (b (unfilter-up x data start-up))
         (c (unfilter-sub x data start-up pixel-bytes))
         (p (- (+ a b) c))
         (pa (abs (- p a)))
         (pb (abs (- p b)))
         (pc (abs (- p c))))
    (cond ((and (<= pa pb) (<= pa pc)) a)
          ((<= pb pc) b)
          (t c))))
(declaim (inline paeth paeth2))
(defun paeth (a b c)
  (declare (ub8 a b c)
           (optimize speed))
  (let* ((p (- (+ a b) c))
         (pa (abs (- p a)))
         (pb (abs (- p b)))
         (pc (abs (- p c))))
    (cond ((and (<= pa pb) (<= pa pc)) a)
          ((<= pb pc) b)
          (t c))))

(defun paeth2 (a b x data start-up pixel-bytes)
  (declare (ub8a1d data)
           (ub32 x)
           (fixnum start-up)
           (ub8 pixel-bytes a b)
           (optimize speed))
  (let* ((c (unfilter-sub x data start-up pixel-bytes))
         (p (- (+ a b) c))
         (pa (abs (- p a)))
         (pb (abs (- p b)))
         (pc (abs (- p c))))
    (cond ((and (<= pa pb) (<= pa pc)) a)
          ((<= pb pc) b)
          (t c))))

(declaim (inline unfilter-byte))
(defun unfilter-byte (filter x data start start-up pixel-bytes)
  (ecase filter
    (#.+filter-type-none+ 0)
    (#.+filter-type-sub+ (unfilter-sub x data start pixel-bytes))
    (#.+filter-type-up+ (unfilter-up x data start-up))
    (#.+filter-type-average+
     (unfilter-average x data start start-up pixel-bytes))
    (#.+filter-type-paeth+
     (unfilter-paeth x data start start-up pixel-bytes))
    (-1
     (unfilter-average/1 x data start pixel-bytes))))

(defun unfilter-row (data filter in-start pixel-bytes scanline-bytes
                     left-start up-start)
  (declare (ub8a1d data)
           (optimize speed)
           (fixnum in-start pixel-bytes scanline-bytes left-start up-start))
  (let ((length (length data)))
    (check-type left-start unsigned-byte)
    (check-type in-start unsigned-byte)
    (check-type up-start unsigned-byte)
    (macrolet ((lds0 (&body body)
                 `(locally (declare (optimize (safety 0)))
                    ,@body))
               (row (a b c call)
                    (print
                     `(loop :for xs fixnum :from (1+ in-start) :below length
                            :for xo fixnum :from left-start :below length
                            :for x fixnum :from 0 :below scanline-bytes
                            ,@(when a
                                `(:for left fixnum :from (- left-start pixel-bytes)
                                       :below length
                                       :for a = (if (< x pixel-bytes)
                                                    0
                                                    (lds0
                                                     (aref data left)))))
                            ,@(when b
                                `(:for up fixnum :from up-start :below length
                                       :for b = (lds0
                                                 (aref data up))))
                            ,@(when c
                                `(:for upleft fixnum :from (- up-start pixel-bytes)
                                       :below length
                                       :for c = (if (< x pixel-bytes)
                                                    0
                                                    (lds0
                                                     (aref data upleft)))))
                            :for sample = (lds0
                                            (aref data xs))
                            :for out of-type (unsigned-byte 8)
                              = (locally (declare (optimize (safety 0))
                                          )
                                  ,call)
                            :do (lds0
                                  (setf (aref data xo)
                                        ,(if (eql call 0)
                                             'sample
                                             '(ldb (byte 8 0) (+ sample out)))))))))
      (ecase filter
        (#.+filter-type-none+ (row nil nil nil 0))
        (#.+filter-type-sub+
         (row t nil nil a)
         ;(row nil nil nil (unfilter-sub x data left-start pixel-bytes))
         )
        (#.+filter-type-up+
         (row nil t nil b)
         ;(row nil nil nil (unfilter-up x data up-start))
         )
        (#.+filter-type-average+
         (row t t nil (floor (+ a b) 2))
         ;(row nil nil nil (unfilter-average x data left-start up-start pixel-bytes))
         )
        (#.+filter-type-paeth+
         (row t t t (paeth a b c))
         ;(row t t nil (paeth2 a b x data up-start pixel-bytes))
         ;(row nil nil nil (unfilter-paeth x data left-start up-start pixel-bytes))
         )
        (-1
         (row t nil nil (floor a 2)
              #++(unfilter-average/1 x data left-start pixel-bytes)))))))

(defun unfilter (data width height start)
  (declare (ub32 width height)
           (fixnum start)
           (ub8a1d data))
  (let* ((pixel-bytes (get-pixel-bytes))
        (scanline-bytes (get-scanline-bytes width))
        (row-bytes (1+ scanline-bytes)))
    (declare (fixnum pixel-bytes scanline-bytes row-bytes))
    (when (plusp height)
      (let ((filter (aref data start)))
        (setf filter (case filter
                       (#.+filter-type-average+ -1)
                       (#.+filter-type-paeth+ +filter-type-sub+)
                       (#.+filter-type-up+ +filter-type-none+)
                       (t filter)))
        (unfilter-row data filter start pixel-bytes scanline-bytes
                      start start)))

    (loop :for y :from 1 :below height
          :for in-start :from (+ start row-bytes) :by row-bytes
          :for left-start :from (+ start scanline-bytes) :by scanline-bytes
          :for up-start :from start :by scanline-bytes
          :for filter = (aref data in-start)
          :do (unfilter-row data filter in-start pixel-bytes scanline-bytes
                            left-start up-start))))

(defun decode ()
  (let ((data (data *png-object*)))
    (declare (ub8a1d data))
    (macrolet ((copy/16 ()
                 `(progn
                    (assert (zerop (mod (array-total-size data) 2)))
                    (loop :for d :below (array-total-size image-data)
                          :for s :below (array-total-size data) :by 2
                          :do (locally (declare (optimize speed (safety 0)))
                                (setf (row-major-aref image-data d)
                                      (dpb (aref data s) (byte 8 8)
                                           (aref data (1+ s))))))))
               (copy/8 ()
                 `(loop :for d :below (array-total-size image-data)
                        :for s :below (array-total-size data)
                        :do (locally (declare (optimize speed (safety 0)))
                              (setf (row-major-aref image-data d)
                                    (aref data s)))))
               ;; color-key transparency is a bit of a hack
               ;; currently it does a copy as if it didn't have
               ;; transparency, then shifts values into correct
               ;; positions while adding alpha
               (trns (opaque)
                 `(loop
                    :with c = (array-dimension image-data 2)
                    :with key = (etypecase transparency
                                  (ub16
                                   (make-array 1 :element-type 'ub16
                                                 :initial-element transparency))
                                  (ub16a1d
                                   transparency))
                    :for s :from (- (* width height (1- c)) (1- c))
                    :downto 0 :by (1- c)
                    :for d :from (- (array-total-size image-data) c)
                    :downto 0 :by c
                    :do (loop
                          :for i :below (1- c)
                          :for k :across key
                          :for v = (row-major-aref image-data (+ s i))
                          :do (setf (row-major-aref image-data (+ d i)) v)
                          :count (= v k) :into matches
                          :collect (list v k matches) into foo
                          :finally (setf (row-major-aref image-data (+ d (1- c)))
                                         (if (= matches (1- c))
                                             0
                                             ,opaque))))))
      (with-slots (width height bit-depth interlace-method color-type
                   palette transparency)
          *png-object*
        (setf (data *png-object*) (allocate-image-data))
        (if (eq interlace-method :null)
            (unfilter data width height 0)
            (setf data (deinterlace-adam7 data)))
        (assert (and (typep bit-depth 'ub8)
                     (member bit-depth '(1 2 4 8 16))))
        (let ((image-data (data *png-object*)))
          (flet ((copy/3d/16 ()
                   (declare (ub16a3d image-data))
                   (copy/16))
                 (copy/2d/16 ()
                   (declare (ub16a2d image-data))
                   (copy/16))
                 (copy/3d/8 ()
                   (declare (ub8a3d image-data))
                   (copy/8))
                 (copy/2d/8 ()
                   (declare (ub8a2d image-data))
                   (copy/8))
                 (copy/2d/sub ()
                   ;; used for both 2d and 3d arrays, in case we have
                   ;; a tRNs chunk, so can't declare array rank
                   (declare (ub8a image-data))
                   (loop :with s = 0  ;; start of row in source
                         :with x = 0  ;; x coord of current pixel
                         :with bx = 0 ;; offset of byte containing x
                         :with p = 0  ;; pixel in byte
                         :with b = 0  ;; current byte
                         :with scanline-bytes = (get-scanline-bytes width)
                         :with ssize = (array-total-size data)
                         :for d :below (array-total-size image-data)
                         :while (< (+ s bx) ssize)
                         ;; read next byte of pixels from source
                         :when (zerop p)
                           :do (setf b (aref data (+ s bx)))
                         :do (setf (row-major-aref image-data d)
                                   (ldb (byte bit-depth (- 8 p bit-depth))
                                        b))
                             (incf p bit-depth)
                             (incf x)
                             (cond
                               ((>= x width) ;; no more pixels in row
                                (setf x 0
                                      bx 0
                                      p 0)
                                (incf s scanline-bytes))
                               ((>= p 8) ;; no more pixels in byte
                                (setf p 0)
                                (incf bx 1)))))
                 (copy/pal/8 ()
                   (declare (ub8a3d image-data))
                   (loop
                     :with c = (array-dimension image-data 2)
                     :for d :below (array-total-size image-data) :by c
                     :for s :across data
                     :do  (setf (row-major-aref image-data (+ d 0))
                                (aref palette s 0)
                                (row-major-aref image-data (+ d 1))
                                (aref palette s 1)
                                (row-major-aref image-data (+ d 2))
                                (aref palette s 2))
                          (when transparency
                            (setf (row-major-aref image-data (+ d 3))
                                  (if (array-in-bounds-p transparency s)
                                      (aref transparency s)
                                      255)))))
                 (copy/pal/sub ()
                   (loop
                     :with scanline-bytes = (get-scanline-bytes width)
                     :with pixels-per-byte = (/ 8 bit-depth)
                     :for y :below height
                     :for yb = (* y scanline-bytes)
                     :do (loop
                           :for x :below width
                           :do (multiple-value-bind (b p)
                                   (floor x pixels-per-byte)
                                 (let ((i (ldb (byte bit-depth
                                                     (- 8 (* p bit-depth) bit-depth))
                                               (aref data (+ yb b)))))
                                   (setf (aref image-data y x 0)
                                         (aref palette i 0)
                                         (aref image-data y x 1)
                                         (aref palette i 1)
                                         (aref image-data y x 2)
                                         (aref palette i 2))
                                   (when transparency
                                     (setf (aref image-data y x 3)
                                           (if (array-in-bounds-p transparency i)
                                               (aref transparency i)
                                               255))))))))
                 (trns/16 ()
                   (trns #xffff))
                 (trns/8 ()
                   (trns #xff)))
            (ecase color-type
              ((:truecolour :truecolour-alpha :greyscale-alpha)
               (ecase bit-depth
                 (8 (copy/3d/8))
                 (16 (copy/3d/16)))
               (when transparency
                 (ecase bit-depth
                   (8 (trns/8))
                   (16 (trns/16)))))
              (:greyscale
               (if transparency
                   (ecase bit-depth
                     (8 (copy/3d/8) (trns/8))
                     (16 (copy/3d/16) (trns/16))
                     ((1 2 4) (copy/2d/sub) (trns/8)))
                   (ecase bit-depth
                     (8 (copy/2d/8))
                     (16 (copy/2d/16))
                     ((1 2 4) (copy/2d/sub)))))
              (:indexed-colour
               (ecase bit-depth
                 (8 (copy/pal/8))
                 ((1 2 4) (copy/pal/sub)))))))))
    *png-object*))
