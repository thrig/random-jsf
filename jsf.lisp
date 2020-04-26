(defpackage #:random-jsf
  (:use #:cl)
  (:export #:jrandom #:jrandom-seed #:make-random-jsf))
(in-package #:random-jsf)

(defmacro ldb32 (n) `(ldb (byte 32 0) ,n))

; TODO this probably needs LDB32 applied to it for 32-bit LISPs
(defmacro rot32 (x k) `(logior (ash ,x ,k) (ash ,x (- (- 32 ,k)))))

(defun jrandom (ctx)
  (declare (optimize (speed 3) (safety 1) (debug 1))
           ((simple-array (unsigned-byte 32) (5)) ctx))
  (let* ((b (aref ctx 2))
         (c (aref ctx 3))
         (d (aref ctx 4))
         (e (ldb32 (- (aref ctx 1) (rot32 b 27)))))
    (declare ((unsigned-byte 32) e))
    (let ((a (setf (aref ctx 1) (logxor b (ldb32 (rot32 c 17))))))
      (setf (aref ctx 2) (ldb32 (+ c d)))
      (setf (aref ctx 3) (ldb32 (+ d e)))
      (setf (aref ctx 4) (ldb32 (+ e a))))))

(defun jrandom-seed (ctx)
  (declare ((simple-array (unsigned-byte 32) (5)) ctx))
  (aref ctx 0))

; slot 0 is original seed; A => 1, B => 2, C => 3, D => 4
(defun make-random-jsf (seed)
  (declare ((unsigned-byte 32) seed))
  (let ((ctx (make-array 5 :element-type '(unsigned-byte 32)
                           :initial-element (ldb32 seed))))
    (setf (aref ctx 1) #xf1ea5eed)
    (loop :repeat 20 :do (jrandom ctx))
    ctx))
