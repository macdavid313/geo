;;;; types.cl
(in-package #:geo.types)

;;; Types
(deftype coord-num ()
  '(and real (not (satisfies nanp))))

(deftype geometry ()
  '(or
    point multi-point
    line-string multi-line-string
    linear-ring
    polygon multi-polygon
    rect
    geometry-collection))

(defun geometry-p (x) (typep x 'geometry))

(defun coord-printer (c stream depth)
  (declare (ignore depth))
  (with-slots (x y z) c
    (if* z
       then (format stream "Coordinate<~,5f, ~,5f, ~,5f>" x y z)
       else (format stream "Coordinate<~,5f, ~,5f>" x y))))

(defstruct (coord (:constructor mk-coord)
                  (:print-function coord-printer))
  (x nil :type double-float :read-only t)
  (y nil :type double-float :read-only t)
  (z nil :type (or null double-float) :read-only t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct abstract-geometry
    (srid nil :type (or null fixnum))   ; spatial reference identifier
    (dim nil :type (or null (integer 2 3))) ; coordinate dimension (only 2D and 3D are supported)
    (bbox nil :type (or null rect)) ; the minimal rectangular polygon that contains the geometry
    (storage nil                    ; the underlying storage
     :type (or null (simple-array (or double-float geometry) (*)))))

  (defmacro defgeometry (name)
    `(defstruct (,name (:include abstract-geometry)
                       (:constructor ,(intern (string+ "mk-" name))))))

  (defgeometry point)
  (defgeometry line-string)
  (defgeometry linear-ring)
  (defgeometry polygon)
  (defgeometry rect)
  (defgeometry multi-point)
  (defgeometry multi-line-string)
  (defgeometry multi-polygon)
  (defgeometry geometry-collection))

;;; Printer
(defun print-geometry (g stream)
  (let ((name (typecase g
                (point "POINT")
                (line-string "LINESTRING")
                (linear-ring "LINEARRING")
                (polygon "POLYGON")
                (rect "RECT")
                (multi-point "MULTIPOINT")
                (multi-line-string "MULTILINESTRING")
                (multi-polygon "MULTIPOLYGON")
                (geometry-collection "GEOMETRYCOLLECTION"))))
    (if* (geometry-empty-p g)
       then (format stream "~A EMPTY" name)
       else (print-unreadable-object (g stream :identity t)
              (write-string name stream)))))

(defmethod print-object ((g abstract-geometry) stream)
  (print-geometry g stream))

(defmethod print-object ((g rect) stream)
  (if* (geometry-empty-p g)
     then (print-geometry g stream)
     else (multiple-value-bind (xmin ymin xmax ymax) (rect-xy g)
            (if* (= (dim g) 2)
               then (format stream "RECT (~,5f, ~,5f, ~,5f, ~,5f)"
                                   xmin ymin xmax ymax)
               else (print-geometry g stream)))))

;;; Constants
(defconstant +empty-point+ (mk-point))
(defconstant +empty-line-string+ (mk-line-string))
(defconstant +empty-linear-ring+ (mk-linear-ring))
(defconstant +empty-polygon+ (mk-polygon))
(defconstant +empty-rect+ (mk-rect))
(defconstant +empty-multi-point+ (mk-multi-point))
(defconstant +empty-multi-line-string+ (mk-multi-line-string))
(defconstant +empty-multi-polygon+ (mk-multi-polygon))
(defconstant +empty-geometry-collection+ (mk-geometry-collection))

;;; Constructors
(defun make-coord (x y &optional z)
  (check-type x coord-num)
  (check-type y coord-num)
  (when z (check-type z coord-num))
  (mk-coord :x (if (double-float-p x) x (float x 0d0))
            :y (if (double-float-p y) y (float y 0d0))
            :z (when z (if (double-float-p z) z (float z 0d0)))))

(defun make-point (x y &key z srid)
  (check-type x coord-num)
  (check-type y coord-num)
  (when z (check-type z coord-num))
  (check-type srid (or null fixnum))
  (setq x (if (double-float-p x) x (float x 0d0))
        y (if (double-float-p y) y (float y 0d0))
        z (when z (if (double-float-p z) z (float z 0d0))))
  (let* ((dim (if z 3 2))
         (storage (make-array dim :element-type 'double-float)))
    (setf (aref storage 0) x
          (aref storage 1) y)
    (when z (setf (aref storage 2) z))
    (mk-point :srid srid
              :dim dim
              :storage storage)))

(defun make-point-from-coord (coord &key srid)
  (check-type coord coord)
  (let ((storage (make-array (dim coord) :element-type 'double-float)))
    (multiple-value-bind (x y z) (coord-xyz coord)
      (setf (aref storage 0) x
            (aref storage 1) y)
      (when z (setf (aref storage 2) z))
      (mk-point :srid srid
                :dim (dim coord)
                :storage storage))))

(defun make-point-from-coords (coords &key srid)
  (check-type coords (array coord (*)))
  (check-type srid (or null fixnum))
  (if* (zerop (length coords))
     then +empty-point+
     else (make-point-from-coord (aref coords 0) :srid srid)))

(defun resolve-dim-from-items (items)
  (reduce (lambda (dima dimb)
            (check-type dima (or null (integer 2 3)))
            (check-type dimb (or null (integer 2 3)))
            (cond ((null dima) dimb)
                  ((null dimb) dima)
                  (t (when (/= dima dimb)
                       (error "Inconsistent dimensions found!"))
                     dima)))
          items
          :key #'dim
          :initial-value nil))

(defun coordinates->storage (coordinates dim)
  (declare (type (or list (array coord (*))) coordinates))
  (let* ((ncoords (length coordinates))
         (storage (make-array (* ncoords dim) :element-type 'double-float)))
    (declare (type fixnum ncoords)
             (type (integer 2 3) dim))
    (do ((i 0 (+ i 1))
         c)
        ((= i ncoords) storage)
      (if* (arrayp coordinates)
         then (setq c (aref coordinates i))
         else (setq c (car coordinates))
              (setq coordinates (cdr coordinates)))
      (multiple-value-bind (x y z) (coord-xyz c)
        (declare (type double-float x y z))
        (setf (aref storage (* dim i)) x
              (aref storage (+ (* dim i) 1)) y)
        (when (= dim 3)
          (setf (aref storage (+ (* dim i) 2)) z))))))

(defun make-line-string-from-coords (coords &key srid)
  (check-type coords (or list (array coord (*))))
  (check-type srid (or null fixnum))
  (if* (zerop (length coords))
     then +empty-line-string+
     else (let ((dim (resolve-dim-from-items coords)))
            (mk-line-string :srid srid
                            :dim dim
                            :storage (coordinates->storage coords dim)))))

(defun make-linear-ring-from-coords (coords &key srid)
  (check-type coords (or list (array coord (*))))
  (check-type srid (or null fixnum))
  (if* (zerop (length coords))
     then +empty-linear-ring+
     else (let ((dim (resolve-dim-from-items coords)))
            (mk-linear-ring :srid srid
                            :dim dim
                            :storage (coordinates->storage coords dim)))))

(defun make-polygon (exterior interiors &key srid)
  (check-type exterior linear-ring)
  (check-type interiors (array linear-ring (*)))
  (check-type srid (or null fixnum))
  (if* (geometry-empty-p exterior)
     then +empty-polygon+
     else (let ((rings (concatenate '(simple-array linear-ring (*))
                                    (make-array 1 :element-type 'linear-ring :initial-element exterior)
                                    interiors)))
            (mk-polygon :srid srid
                        :dim (resolve-dim-from-items rings)
                        :storage rings))))

(defun make-polygon-from-linear-rings (rings &key srid)
  (check-type rings (or list (array linear-ring (*))))
  (check-type srid (or null fixnum))
  (if* (zerop (length rings))
     then +empty-polygon+
     else (let ((rings (make-array (length rings) :element-type 'linear-ring :initial-contents rings)))
            (mk-polygon :srid srid
                        :dim (resolve-dim-from-items rings)
                        :storage rings))))

(defun make-rect-from-xy (xmin ymin xmax ymax &key srid)
  (check-type xmin coord-num)
  (check-type ymin coord-num)
  (check-type xmax coord-num)
  (check-type ymax coord-num)
  (check-type srid (or null fixnum))
  (let ((storage (make-array 4 :element-type 'double-float)))
    (setf (aref storage 0) (float xmin 0d0)
          (aref storage 1) (float ymin 0d0)
          (aref storage 2) (float xmax 0d0)
          (aref storage 3) (float ymax 0d0))
    (mk-rect :srid srid
             :dim 2
             :storage storage)))

(defun make-multi-geometry-collection (items empty constructor-fn)
  (if* (zerop (length items))
     then empty
     else (funcall constructor-fn :srid (get-srid (elt items 0))
                                  :dim (resolve-dim-from-items items)
                                  :storage (if* (listp items)
                                              then (make-array (length items) :element-type 't
                                                                              :initial-contents items)
                                              else items))))

(defun make-multi-point (points)
  (check-type points (or list (array point (*))))
  (make-multi-geometry-collection points +empty-multi-point+ #'mk-multi-point))

(defun make-multi-line-string (line-strings)
  (check-type line-strings (or list (array line-string (*))))
  (make-multi-geometry-collection line-strings +empty-multi-line-string+ #'mk-multi-line-string))

(defun make-multi-polygon (polygons)
  (check-type polygons (or list (array polygon (*))))
  (make-multi-geometry-collection polygons +empty-multi-polygon+ #'mk-multi-polygon) )

(defun make-geometry-collection (gs)
  (check-type gs (or list (array geometry (*))))
  (make-multi-geometry-collection gs +empty-geometry-collection+ #'mk-geometry-collection))

;;; APIs
(defun coord-xy (c)
  (check-type c coord)
  (values (coord-x c) (coord-y c)))

(defun coord-xyz (c)
  (check-type c coord)
  (values (coord-x c) (coord-y c) (coord-z c)))

(defun coord-equal-p (c1 c2 &optional (tolerance 1.0d-6))
  (check-type c1 coord)
  (check-type c2 coord)
  (multiple-value-bind (x1 y1 z1) (coord-xyz c1)
    (multiple-value-bind (x2 y2 z2) (coord-xyz c2)
      (and (= (dim c1) (dim c2))
           (< (abs (- x1 x2)) tolerance)
           (< (abs (- y1 y2)) tolerance)
           (if (and z1 z2) (< (abs (- z1 z2)) tolerance) t)))))

(defun dim (x)
  (check-type x (or coord geometry))
  (if* (coord-p x)
     then (if (coord-z x) 3 2)
     else (slot-value x 'dim)))

(defun get-srid (g)
  (check-type g geometry)
  (slot-value g 'srid))

(defun set-srid (g srid)
  (check-type g geometry)
  (check-type srid fixnum)
  (setf (slot-value g 'srid) srid))

(defun geometry-empty-p (g)
  (check-type g geometry)
  (eq g (typecase g
          (point +empty-point+)
          (line-string +empty-line-string+)
          (linear-ring +empty-linear-ring+)
          (polygon +empty-polygon+)
          (rect +empty-rect+)
          (multi-point +empty-multi-point+)
          (multi-line-string +empty-multi-line-string+)
          (multi-polygon +empty-multi-polygon+)
          (geometry-collection +empty-geometry-collection+))))

(defun coordinates (g)
  (check-type g geometry)
  (typecase  g
    (point
     (if* (geometry-empty-p g)
        then (make-array 0 :element-type 'coord)
        else (with-slots (storage) g
               (make-array 1 :element-type 'coord
                             :initial-element (if* (= 2 (dim g))
                                                 then (mk-coord :x (aref storage 0)
                                                                :y (aref storage 1))
                                                 else (mk-coord :x (aref storage 0)
                                                                :y (aref storage 1)
                                                                :z (aref storage 2)))))))
    ((or line-string linear-ring)
     (if* (geometry-empty-p g)
        then (make-array 0 :element-type 'coord)
        else (with-slots (storage dim) g
               (do* ((ncoords (/ (length storage) dim))
                     (coords (make-array ncoords :element-type 'coord))
                     (i 0 (+ i 1)))
                    ((= i ncoords) coords)
                 (setf (aref coords i)
                       (if* (= dim 2)
                          then (mk-coord :x (aref storage (* dim i))
                                         :y (aref storage (+ (* dim i) 1)))
                          else (mk-coord :x (aref storage (* dim i))
                                         :y (aref storage (+ (* dim i) 1))
                                         :z (aref storage (+ (* dim i) 2)))))))))

    ((or polygon rect multi-point multi-line-string multi-polygon geometry-collection)
     (slot-value g 'storage))
    (t (error "Unkown geometry type: ~a" g))))

(defmacro do-geometry-xy ((x y) g &body body)
  (let ((gvar (gensym))
        (svar (gensym))
        (ncoords (gensym))
        (i (gensym)))
    `(let ((,gvar ,g))
       (unless (or (point-p ,gvar) (line-string-p ,gvar) (linear-ring-p ,gvar))
         (error "g must be type of point, or line-string, or linear-ring: ~a" ,gvar))
       (unless (geometry-empty-p ,gvar)
         (let* ((,svar (slot-value ,gvar 'storage))
                (,ncoords (/ (length ,svar) 2)))
           (declare (type (simple-array double-float (*)) ,svar)
                    (type fixnum ,ncoords))
           (do ((,i 0 (1+ ,i))
                ,x
                ,y)
               ((= ,i ,ncoords))
             (setq ,x (aref ,svar (* 2 ,i))
                   ,y (aref ,svar (1+ (* 2 ,i))))
             ,@body))))))

(defun point-xy (g)
  (check-type g point)
  (with-slots (storage) g
    (values (aref storage 0)            ; x
            (aref storage 1))))         ; y

(defun point-xyz (g)
  (check-type g point)
  (with-slots (dim storage) g
    (values (aref storage 0)                     ; x
            (aref storage 1)                     ; y
            (when (= dim 3) (aref storage 2))))) ; z

(defun ensure-ring (g)
  (check-type g (or line-string linear-ring))
  (cond ((linear-ring-p g) g)
        ((geometry-empty-p g) +empty-linear-ring+)
        (t (let* ((coords (coordinates g))
                  (ncoords (length coords))
                  (ch (aref coords 0))             ; head
                  (ct (aref coords (1- ncoords)))) ; tail
             (make-linear-ring-from-coords
              (if* (coord-equal-p ch ct)
                 then coords
                 else (concatenate 'vector coords (vector ch))))))))

(defun rect-xy (g)
  (unless (and (rect-p g) (= (dim g) 2))
    (error "'rect-xy' only supports 2D rectangle."))
  (with-slots (storage) g
    (if* (= (dim g) 2)
       then (values (aref storage 0)     ; xmin
                    (aref storage 1)     ; ymin
                    (aref storage 2)     ; xmax
                    (aref storage 3))))) ; ymax

(defun rect->polygon (g)
  (declare (optimize speed (safety 0)))
  (unless (and (rect-p g) (= (dim g) 2))
    (error "'rect-xy' only supports 2D rectangle."))
  (if* (geometry-empty-p g)
     then +empty-polygon+
     else (multiple-value-bind (xmin ymin xmax ymax)
              (rect-xy g)
            (mk-polygon :srid (get-srid g)
                        :dim 2
                        :storage
                        (make-array 1 :element-type 'linear-ring
                                      :initial-element
                                      (mk-linear-ring :srid (get-srid g)
                                                      :dim 2
                                                      :storage
                                                      (with-stack-list (storage xmin ymin
                                                                                xmax ymin
                                                                                xmax ymax
                                                                                xmin ymax
                                                                                xmin ymin)
                                                        (make-array 10 :element-type 'double-float
                                                                       :initial-contents storage))))))))

(defun polygon-exterior (g)
  (check-type g polygon)
  (aref (slot-value g 'storage) 0))

(defun polygon-interiors (g)
  (check-type g polygon)
  (subseq (slot-value g 'storage) 1))

(declaim (ftype (function (geometry) (or null rect)) envelope-2d))
(defun envelope-2d (g)
  (declare (type geometry g)
           (optimize (speed 3) (safety 0) (space 0)))
  (labels ((expand-to-include/coord (env coord) ; merge from a coord
             (declare (type rect env)
                      (type coord coord))
             (multiple-value-bind (xmin ymin xmax ymax) (rect-xy env)
               (declare (type double-float xmin ymin xmax ymax))
               (multiple-value-bind (x y) (coord-xy coord)
                 (declare (type double-float x y))
                 (with-slots (storage) env
                   (setf (aref storage 0) (min xmin x)  ; xmin
                         (aref storage 1) (min ymin y)  ; ymin
                         (aref storage 2) (max xmax x)  ; xmax
                         (aref storage 3) (max ymax y)) ; ymax
                   env))))
           (expand-to-include/envelope (env other) ; merge from another envelope
             (declare (type rect env other))
             (cond ((geometry-empty-p env) other)
                   ((geometry-empty-p other) env)
                   (t (multiple-value-bind (xmin ymin xmax ymax) (rect-xy env)
                        (declare (type double-float xmin ymin xmax ymax))
                        (multiple-value-bind (other-xmin other-ymin other-xmax other-ymax) (rect-xy other)
                          (declare (type double-float other-xmin other-ymin other-xmax other-ymax))
                          (with-slots (storage) env
                            (setf (aref storage 0) (min xmin other-xmin)  ; xmin
                                  (aref storage 1) (min ymin other-ymin)  ; ymin
                                  (aref storage 2) (max xmax other-xmax)  ; xmax
                                  (aref storage 3) (max ymax other-ymax)) ; ymax
                            env)))))))
    (declare (dynamic-extent #'expand-to-include/coord #'expand-to-include/envelope))
    (if* (geometry-empty-p g)
       then +empty-rect+
       else (typecase g
              ((or point line-string linear-ring)
               (do* ((cs (coordinates g))
                     (ncs (length cs))
                     (i 1 (1+ i))       ; start from the 2nd item, if any
                     (env               ; initialised by the fist coord
                      (multiple-value-bind (x y)
                          (coord-xy (aref cs 0))
                        (make-rect-from-xy x y x y))))
                    ((= i ncs) env)
                 (setq env (expand-to-include/coord env (aref cs i)))))
              (polygon (bbox (polygon-exterior g)))
              (rect g)
              (t (if* (every 'geometry-empty-p (slot-value g 'storage))
                    then +empty-rect+
                    else (do* ((gs (coordinates g))
                               (ngs (length gs))
                               (i 1 (1+ i)) ; start from the 2nd item, if any
                               (env (envelope-2d (aref gs 0)))) ; initialised by the first envelope
                              ((= i ngs) env)
                           (setq env (expand-to-include/envelope env (envelope-2d (aref gs i)))))))))))

(declaim (ftype (function (geometry) rect) bbox))
(defun bbox (g)
  (check-type g geometry)
  (with-slots (dim bbox) g
    (if* (or (geometry-empty-p g)
             (and (typep g '(or multi-point multi-line-string multi-polygon geometry-collection))
                  (every 'geometry-empty-p (slot-value g 'storage))))
       then +empty-rect+
       else (when (and (= dim 2) (not bbox))
              (setf bbox (envelope-2d g)))
            bbox)))
