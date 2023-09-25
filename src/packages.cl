;;;; packages.cl
(in-package #:cl-user)

(defpackage #:geo.types
  (:use #:cl
        #:excl
        #:util.string)
  (:export #:geometry
           #:point
           #:multi-point
           #:line-string
           #:multi-line-string
           #:linear-ring
           #:polygon
           #:multi-polygon
           #:rect
           #:geometry-collection
           #:dim
           #:geometry-empty-p
           #:polygon-interiors
           #:polygon-exterior
           #:coord-xyz
           #:coordinates
           #:point-xyz
           #:+empty-point+
           #:+empty-line-string+
           #:+empty-linear-ring+
           #:+empty-polygon+
           #:+empty-rect+
           #:+empty-multi-point+
           #:+empty-multi-line-string+
           #:+empty-multi-polygon+
           #:+empty-geometry-collection+
           #:make-coord
           #:make-point
           #:make-point-from-coord
           #:make-point-from-coords
           #:make-line-string-from-coords
           #:make-linear-ring-from-coords
           #:make-polygon
           #:make-polygon-from-linear-rings
           #:make-rect-from-xy
           #:make-multi-point
           #:make-multi-line-string
           #:make-multi-polygon
           #:make-geometry-collection))

(defpackage #:geo.wkt
  (:use #:cl
        #:excl
        #:geo.types)
  (:nicknames #:wkt)
  (:import-from #:util.string #:string+)
  (:export #:read-wkt
           #:*wkt-writer-rounding-precision*
           #:*wkt-writer-trim-p*
           #:write-wkt))

(defpackage #:geo
  (:use #:cl
        #:excl
        #:geo.types
        #:geo.wkt))
