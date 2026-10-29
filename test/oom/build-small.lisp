;;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
;;;; Build a small-heap executable without the optional broad JDK warm-up,
;;;; whose security-method compilation itself needs more than 512 MiB.
(require :asdf)
(asdf:load-system :openldk)
(openldk::initialize)
(openldk::%reset-runtime-for-save)
(clrhash openldk::*lisp-to-java-threads*)
(setf openldk::*current-thread* nil)
(sb-ext:save-lisp-and-die ".cache/openldk-oom-small"
                        :executable t :save-runtime-options t
                        :toplevel #'openldk:main-wrapper)
