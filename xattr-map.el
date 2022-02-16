;;; xattr-map.el --- Map implementation for xattr    -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Valeriy Litkovskyy

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Map implementation for xattr.  Some operations are not possible for obvious
;; reasons (map-insert for example).

;;; Code:

(require 'map)
(require 'xattr)

(cl-defstruct (xattr-map (:constructor xattr-map (file)))
  "Type used to implement map for xattr."
  file)

(cl-defmethod map-elt ((map xattr-map) key &optional default _testfn)
  "Get extended attribute MAP's value by KEY or DEFAULT."
  (or (xattr-get (xattr-map-file map) key) default))

(cl-defmethod map-delete ((map xattr-map) key)
  "Remove extended attribute MAP's element by KEY."
  (xattr-remove (xattr-map-file map) key)
  map)

(cl-defmethod map-keys ((map xattr-map))
  "List extended attribute MAP's keys."
  (xattr-list (xattr-map-file map)))

(cl-defmethod map-apply (function (map xattr-map))
  "Apply FUNCTION to each element of extended attribute MAP.
Return the result as list.  FUNCTION is called with xattr key and
value."
  (let ((file (xattr-map-file map)))
    (mapcar (lambda (key) (funcall function key (xattr-get file key)))
            (xattr-list file))))

(cl-defmethod map-do (function (map xattr-map))
  "Apply FUNCTION to each element of extended attribute MAP.
Return nil.  FUNCTION is called with xattr key and value."
  (let ((file (xattr-map-file map)))
    (mapc (lambda (key) (funcall function key (xattr-get file key)))
          (xattr-list file))
    nil))

(cl-defmethod map-keys-apply (function (map xattr-map))
  "Return the result of applying FUNCTION to each key of MAP.
MAP is extended file attributes."
  (mapcar function (xattr-list (xattr-map-file map))))

(cl-defmethod mapp ((_map xattr-map))
  "Return non-nil if MAP is an extended attributes map."
  t)

(cl-defmethod map-empty-p ((map xattr-map))
  "Return non-nil if there are no extended attributes in MAP."
  (xattr-empty-p (xattr-map-file map)))

(cl-defmethod map-contains-key ((map xattr-map) key &optional _testfn)
  "Return non-nil if extended attributes MAP has KEY.
Test key with TESTFN, otherwise `string=' is used."
  (member key (xattr-list (xattr-map-file map))))

(cl-defmethod map-into ((map xattr-map) (_type (eql xattr-map)))
  "Convert extended attributes MAP into the same map.
This exists only to enable `map-merge' for extended attributes.
  There is no way to know how to create `xattr-map' from other
  types of map."  map)

(cl-defmethod map-put! ((map xattr-map) key value &optional _testfn)
  "Set KEY with VALUE to extended attributes MAP."
  (xattr-set (xattr-map-file map) key value))

(provide 'xattr-map)
;;; xattr-map.el ends here
