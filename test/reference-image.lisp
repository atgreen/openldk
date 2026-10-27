;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
;;; Build the fixture used by make check-reference-image. Its static state
;;; deliberately survives, so the restarted service must deliver a saved reference.
(in-package :cl-user)

(let ((directory (uiop:ensure-directory-pathname (uiop:getenv "LDK_REFERENCE_IMAGE_DIR"))))
  (openldk::dump-warm-image (namestring (merge-pathnames "reference-image" directory))
                           "ReferenceImage" :classpath (namestring directory)
                           :warmups (list nil) :reset-classes nil))
