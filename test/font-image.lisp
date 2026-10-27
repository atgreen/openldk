;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
;;; Keep logical scalers and cached native glyph records across an image restart.
(in-package :cl-user)

(let ((directory (uiop:ensure-directory-pathname (uiop:getenv "LDK_FONT_IMAGE_DIR"))))
  (openldk::dump-warm-image (namestring (merge-pathnames "font-image" directory))
                           "FontImage" :classpath (namestring directory)
                           :warmups (list nil) :reset-classes nil))
