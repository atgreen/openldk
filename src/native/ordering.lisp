;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2026  Anthony Green <green@moxielogic.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
;;;
;;; This file is part of OpenLDK.

;;; OpenLDK is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your
;;; option) any later version.

;;; OpenLDK is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with OpenLDK; see the file COPYING.  If not, please see
;;; <http://www.gnu.org/licenses/>.

;;; Linking this library statically or dynamically with other modules is
;;; making a combined work based on this library.  Thus, the terms and
;;; conditions of the GNU General Public License cover the whole
;;; combination.

;;; As a special exception, the copyright holders of this library give
;;; you permission to link this library with independent modules to
;;; produce an executable, regardless of the license terms of these
;;; independent modules, and to copy and distribute the resulting
;;; executable under terms of your choice, provided that you also
;;; meet, for each linked independent module, the terms and conditions
;;; of the license of that module.  An independent module is a module
;;; which is not derived from or based on this library.  If you modify
;;; this library, you may extend this exception to your version of the
;;; library, but you are not obligated to do so.  If you do not wish
;;; to do so, delete this exception statement from your version.

(in-package :openldk)

;;; Stronger ordering is intentional: these entry points share the ordering
;;; protocol used by volatile bytecode fields. Keep plain accesses plain.

(defmethod |compareAndExchangeByte(Ljava/lang/Object;JBB)B| :around (unsafe obj offset expected new-val)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |compareAndExchangeInt(Ljava/lang/Object;JII)I| :around (unsafe object offset expected replacement)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |compareAndExchangeLong(Ljava/lang/Object;JJJ)J| :around (unsafe object offset expected replacement)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |compareAndExchangeReference(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;| :around (unsafe object offset expected replacement)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |compareAndSetBoolean(Ljava/lang/Object;JZZ)Z| :around (unsafe obj offset expected new-val)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |compareAndSetByte(Ljava/lang/Object;JBB)Z| :around (unsafe obj offset expected new-val)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |compareAndSetInt(Ljava/lang/Object;JII)Z| :around (unsafe obj field-id expected-value new-value)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |compareAndSetLong(Ljava/lang/Object;JJJ)Z| :around (unsafe obj field-id expected-value new-value)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |compareAndSetObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)| :around (unsafe obj field-id expected-value new-value)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |compareAndSetReference(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z| :around (unsafe obj field-id expected-value new-value)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |compareAndSwapInt(Ljava/lang/Object;JII)Z| :around (unsafe obj field-id expected-value new-value)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |compareAndSwapLong(Ljava/lang/Object;JJJ)Z| :around (unsafe obj field-id expected-value new-value)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |compareAndSwapObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z| :around (unsafe obj field-id expected-value new-value)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |getCharVolatile(Ljava/lang/Object;J)C| :around (unsafe param-object param-long)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |getIntVolatile(Ljava/lang/Object;J)I| :around (unsafe param-object param-long)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |getLongVolatile(J)| :around (unsafe ptr)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |getLongVolatile(Ljava/lang/Object;J)J| :around (unsafe obj l)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |getObjectVolatile(Ljava/lang/Object;J)Ljava/lang/Object;| :around (unsafe obj l)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |getReferenceAcquire(Ljava/lang/Object;J)Ljava/lang/Object;| :around (unsafe obj l)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |getReferenceVolatile(Ljava/lang/Object;J)Ljava/lang/Object;| :around (unsafe object offset)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |putIntVolatile(Ljava/lang/Object;JI)V| :around (unsafe obj offset value)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |putObjectVolatile(Ljava/lang/Object;JLjava/lang/Object;)V| :around (unsafe obj l value)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |putOrderedObject(Ljava/lang/Object;JLjava/lang/Object;)V| :around (unsafe obj l value)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |putReferenceRelease(Ljava/lang/Object;JLjava/lang/Object;)V| :around (unsafe obj l value)
  (with-java-memory-ordering
    (call-next-method)))

(defmethod |putReferenceVolatile(Ljava/lang/Object;JLjava/lang/Object;)V| :around (unsafe object offset value)
  (with-java-memory-ordering
    (call-next-method)))

;; Bytecoded AtomicReference methods bypass the Unsafe entry points. Order
;; their native overrides by the same protocol as direct volatile field reads.
(defun %ordered-native-override (function)
  "Wrap FUNCTION without capturing mutable iteration variables."
  (lambda (&rest args)
    (with-java-memory-ordering
      (apply function args))))

(maphash (lambda (key function)
           (when (starts-with? "java/util/concurrent/atomic/AtomicReference." key)
             (setf (gethash key *native-overrides*) (%ordered-native-override function))))
         *native-overrides*)
