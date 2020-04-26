(defpackage #:random-jsf
  (:use #:cl)
  (:export #:jrandom #:jrandom-seed #:make-random-jsf))
(in-package #:random-jsf)

(defmacro ldb32 (n) `(ldb (byte 32 0) ,n))

; TODO this probably needs LDB32 applied to it for 32-bit LISPs
(defmacro rot32 (x k) `(logior (ash ,x ,k) (ash ,x (- (- 32 ,k)))))

; TODO b, c, d will need local storage if multi-threaded? but don't know
; how to make the array changes atomic in such a case...
(defun jrandom (ctx)
  (declare (optimize (speed 3) (safety 1) (debug 1))
           ((simple-array (unsigned-byte 32) (5)) ctx))
  (let ((e (ldb32 (- (aref ctx 1) (rot32 (aref ctx 2) 27)))))
    (declare ((unsigned-byte 32) e))
    (setf (aref ctx 1) (logxor (aref ctx 2) (ldb32 (rot32 (aref ctx 3) 17))))
    (setf (aref ctx 2) (ldb32 (+ (aref ctx 3) (aref ctx 4))))
    (setf (aref ctx 3) (ldb32 (+ (aref ctx 4) e)))
    (setf (aref ctx 4) (ldb32 (+ e (aref ctx 1))))))

(defun jrandom-seed (ctx)
  (declare ((simple-array (unsigned-byte 32) (5)) ctx))
  (aref ctx 0))

(defun make-random-jsf (seed)
  (declare ((unsigned-byte 32) seed))
  (let ((ctx (make-array 5 :element-type '(unsigned-byte 32))))
    (setf seed (ldb32 seed))
    (psetf (aref ctx 0) seed       ; original seed
           (aref ctx 1) #xf1ea5eed ; A
           (aref ctx 2) seed       ; B
           (aref ctx 3) seed       ; C
           (aref ctx 4) seed)      ; D
    (loop :repeat 20 :do (jrandom ctx))
    ctx))
