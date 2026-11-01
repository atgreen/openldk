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

;;; jdk.test.whitebox.WhiteBox is a HotSpot test-only API whose natives live
;;; in libjvm (gated by -XX:+WhiteBoxAPI).  OpenLDK has no HotSpot, but the
;;; jtreg harness touches WhiteBox before any test/jdk test runs: its
;;; requires.VMProps provider holds a `static final WhiteBox WB =
;;; WhiteBox.getWhiteBox()`, whose <clinit> calls registerNatives(), and then
;;; queries a handful of WB methods to compute @requires "vm.*" properties.
;;;
;;; This is a MINIMAL shim covering exactly the natives VMProps reaches, with
;;; honest-but-conservative answers, so property gathering completes and the
;;; suite can run.  It deliberately does not model real VM internals: VM flags
;;; read back absent (null), and optional features report not-included.  That
;;; makes @requires filtering approximate -- some tests are included or
;;; excluded differently than on HotSpot -- which is acceptable for a first
;;; baseline.  The build-time specializer is the stub class declared in
;;; bootstrap.lisp (like jdk/internal/misc/ScopedMemoryAccess).
;;;
;;; Anything outside VMProps' reach remains unimplemented and, when called,
;;; raises UnsatisfiedLinkError the way a real missing native does.

(defun |jdk/test/whitebox/WhiteBox.registerNatives()V| ()
  "WhiteBox.<clinit> calls this static native to bind the API's natives; here
there is nothing to bind, so it is a no-op."
  nil)

;;; VM flag queries: report every flag as absent (null), which VMProps and
;;; its callers treat as "not set / use the default".
(defmethod |getBooleanVMFlag(Ljava/lang/String;)Ljava/lang/Boolean;|
    ((this |jdk/test/whitebox/WhiteBox|) name)
  (declare (ignore name))
  nil)

(defmethod |getIntVMFlag(Ljava/lang/String;)Ljava/lang/Long;|
    ((this |jdk/test/whitebox/WhiteBox|) name)
  (declare (ignore name))
  nil)

(defmethod |getStringVMFlag(Ljava/lang/String;)Ljava/lang/String;|
    ((this |jdk/test/whitebox/WhiteBox|) name)
  (declare (ignore name))
  nil)

;;; The real memory page size is honest and cheap to answer.
(defmethod |getVMPageSize()I| ((this |jdk/test/whitebox/WhiteBox|))
  (sb-posix:getpagesize))

;;; Optional VM features: report not-included / unsupported.  Booleans are
;;; JVMS ints, 1 true / 0 false.
(defmethod |isJVMCISupportedByGC()Z| ((this |jdk/test/whitebox/WhiteBox|)) 0)
(defmethod |hasLibgraal()Z| ((this |jdk/test/whitebox/WhiteBox|)) 0)
(defmethod |isJFRIncluded()Z| ((this |jdk/test/whitebox/WhiteBox|)) 0)
(defmethod |isJVMTIIncluded()Z| ((this |jdk/test/whitebox/WhiteBox|)) 0)
(defmethod |isDTraceIncluded()Z| ((this |jdk/test/whitebox/WhiteBox|)) 0)
(defmethod |isCDSIncluded()Z| ((this |jdk/test/whitebox/WhiteBox|)) 0)
(defmethod |canWriteJavaHeapArchive()Z| ((this |jdk/test/whitebox/WhiteBox|)) 0)
(defmethod |isStatic()Z| ((this |jdk/test/whitebox/WhiteBox|)) 0)

;;; VMProps uses getLibcName().contains("musl") to set vm.musl; glibc here.
(defmethod |getLibcName()Ljava/lang/String;| ((this |jdk/test/whitebox/WhiteBox|))
  (jstring "gnu"))

;;; GC queries, reached through jdk.test.whitebox.gc.GC.  The int argument is
;;; GC's enum "name" (CollectedHeap::Name): Serial 1, Parallel 2, G1 3,
;;; Epsilon 4, Z 5, Shenandoah 6.  Report G1 -- JDK 25's ergonomic default --
;;; as the one supported and selected collector, so GC.selected() resolves
;;; instead of throwing "No selected GC found".
(defconstant +whitebox-gc-g1+ 3)

(defmethod |isGCSupported(I)Z| ((this |jdk/test/whitebox/WhiteBox|) name)
  (if (eql name +whitebox-gc-g1+) 1 0))

(defmethod |isGCSelected(I)Z| ((this |jdk/test/whitebox/WhiteBox|) name)
  (if (eql name +whitebox-gc-g1+) 1 0))

(defmethod |isGCSupportedByJVMCICompiler(I)Z| ((this |jdk/test/whitebox/WhiteBox|) name)
  (declare (ignore name))
  0)

;;; No explicit GC flag was given, so the selection is ergonomic.
(defmethod |isGCSelectedErgonomically()Z| ((this |jdk/test/whitebox/WhiteBox|)) 1)

;;; Compiler queries, reached through jdk.test.whitebox.code.Compiler.  With
;;; no C2/JVMCI and every VM flag null, Compiler.is{C1,C2,Graal,JVMCI}Enabled
;;; all short-circuit to false; getIntxVMFlag joins the other flag readers.
(defmethod |isC2OrJVMCIIncluded()Z| ((this |jdk/test/whitebox/WhiteBox|)) 0)

(defmethod |getIntxVMFlag(Ljava/lang/String;)Ljava/lang/Long;|
    ((this |jdk/test/whitebox/WhiteBox|) name)
  (declare (ignore name))
  nil)

;;; CPUInfo parses this; an empty list of features is safe and yields no
;;; CPU-feature @requires matches.
(defmethod |getCPUFeatures()Ljava/lang/String;| ((this |jdk/test/whitebox/WhiteBox|))
  (jstring ""))
