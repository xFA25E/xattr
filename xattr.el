;;; xattr.el --- Library to interact with extended attributes  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; Keywords: data, extensions, files, processes
;; Version: 0.0.3
;; URL: https://github.com/xFA25E/xattr
;; Package-Requires: ((emacs "27.1"))

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

;; This library helps to interact with extended attributes (xattrs from now on).
;; It exposes several functions to set, get, remove and list xattrs.  All of
;; them can signal different errors.

;; Functions: xattr-set, xattr-get, xattr-remove, xattr-list, xattr-empty-p

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'xattr-core)

(define-error 'xattr-error
  "Xattr error")

(define-error 'xattr-malloc
  "Failed to allocate memory in xattr-core"
  'xattr-error)

(define-error 'xattr-errno
  "Xattr errno"
  'xattr-error)

(define-error 'xattr-empty-name
  "Attribute name is empty"
  'xattr-error)

(define-error 'xattr-absent-attribute
  "Attribute is absent"
  'xattr-error)

(define-error 'xattr-absent-attribute-or-no-access
  "Attribute is absent or the process has no access to this attribute"
  'xattr-error)

(define-error 'xattr-immutable-or-append-only
  "File is marked as immutable or append-only"
  'xattr-error)

(define-error 'xattr-insufficient-space
  "Insufficient space to store xattr"
  'xattr-error)

(define-error 'xattr-insufficient-space-on-disc
  "Insufficient space on disc to store xattr"
  'xattr-error)

(define-error 'xattr-not-supported
  "Extended attributes are not supported by filesystem or are disabled"
  'xattr-error)

(define-error 'xattr-not-supported-or-invalid-prefix
  "Namespace prefix is invalid or extended attributes are not supported by filesystem or are disabled"
  'xattr-error)

(define-error 'xattr-name-or-value-too-big
  "The size of name or value exceeds fylesystem-specific limit"
  'xattr-error)

(define-error 'xattr-value-too-big
  "The size of the attribute value is larger than the maximum size allowed"
  'xattr-error)

(define-error 'xattr-list-too-big
  "The size of the list of extended attribute names is larger than the maximum size allowed"
  'xattr-error)

(define-error 'xattr-value-too-small
  "The size of the extended attribute value buffer is to small to hold the result"
  'xattr-error)

(define-error 'xattr-list-too-small
  "The size of the extended attribute list buffer is to small to hold the result"
  'xattr-error)

(defun xattr-place (place)
  "Get operation description for PLACE."
  (cl-case place
    (:set "Setting extended attribute")
    (:get "Getting extended attribute")
    (:remove "Removing extended attribute")
    (:list "Listing extended attributes")))

(defun xattr-signal-common (place err &rest arguments)
  "Try to signal an error common for all xattr functions.
Use PLACE to describe operation (:set :get :remove :list).  ERR
is the original xattr-errno error.  ARGUMENTS are the original
arguments passed to function."
  (let ((place (xattr-place place)))
    (pcase err
      ((seq symbol (and (pred integerp) errno) strerror)
       (signal symbol (cl-list* place errno strerror arguments)))
      ((seq _ (or 'EACCES 'ELOOP 'ENAMETOOLONG) strerror)
       (signal 'file-error (list place strerror (cl-first arguments))))
      ((seq _ 'ENOENT strerror)
       (signal 'file-missing (list place strerror (cl-first arguments)))))))

(defun xattr-set (file name value)
  "Set FILE's xattr with NAME to VALUE."
  (condition-case err
      (xattr-core-set file name value)
    (xattr-errno
     (xattr-signal-common :set err file name value)
     (pcase err
       ((seq _ 'EDQUOT _)
        (signal 'xattr-insufficient-space-on-disc (list name value)))
       ((seq _ 'ENOSPC _)
        (signal 'xattr-insufficient-space (list name value)))
       ((seq _ 'EPERM _)
        (signal 'xattr-immutable-or-append-only (list file)))
       ((seq _ 'ENOTSUP _)
        (signal 'xattr-not-supported-or-invalid-prefix (list file name)))
       ((seq _ 'ERANGE _)
        (signal 'xattr-name-or-value-too-big (list file name value)))
       ((seq _ 'EINVAL _)
        (signal 'xattr-empty-name (list name)))
       ((seq sym errno strerror)
        (signal sym (list (xattr-place :set) errno strerror file name value)))))))

(defun xattr-get (file name &optional error-on-absence)
  "Get FILE's xattr with NAME.
If ERROR-ON-ABSENCE is not nil, signal an error if attribute is
not present."
  (condition-case err
      (xattr-core-get file name)
    (xattr-errno
     (xattr-signal-common :get err file name)
     (pcase err
       ((seq _ 'E2BIG _)
        (signal 'xattr-value-too-big (list file name)))
       ((and (seq _ 'ENODATA _) (guard error-on-absence))
        (signal 'xattr-absent-attribute-or-no-access (list name)))
       ((and (seq _ 'ENODATA _))
        nil)
       ((seq _ 'ENOTSUP _)
        (signal 'xattr-not-supported-or-invalid-prefix (list file name)))
       ((seq _ 'ERANGE _)
        (signal 'xattr-value-too-small nil))
       ((seq _ 'EINVAL _)
        (signal 'xattr-empty-name (list name)))
       ((seq sym errno strerror)
        (signal sym (list (xattr-place :get) errno strerror file name)))))))

(defun xattr-remove (file name &optional error-on-absence)
  "Remove FILE's xattr with NAME.
If ERROR-ON-ABSENCE is not nil, signal an error if attribute is
not present."
  (condition-case err
      (xattr-core-remove file name)
    (xattr-errno
     (xattr-signal-common :remove err file name)
     (pcase err
       ((and (seq _ 'ENODATA _) (guard error-on-absence))
        (signal 'xattr-absent-attribute (list name)))
       ((and (seq _ 'ENODATA _))
        nil)
       ((seq _ 'ENOTSUP _)
        (signal 'xattr-not-supported-or-invalid-prefix (list file name)))
       ((seq _ 'EINVAL _)
        (signal 'xattr-empty-name (list name)))
       ((seq sym errno strerror)
        (signal sym (list (xattr-place :remove) errno strerror file name)))))))

(defun xattr-list (file)
  "List FILE's xattrs."
  (condition-case err
      (xattr-core-list file)
    (xattr-errno
     (xattr-signal-common :list err file)
     (pcase err
       ((seq _ 'E2BIG _)
        (signal 'xattr-list-too-big (list file)))
       ((seq _ 'ENOTSUP _)
        (signal 'xattr-not-supported (list file)))
       ((seq _ 'ERANGE _)
        (signal 'xattr-list-too-small nil))
       ((seq sym errno strerror)
        (signal sym (list (xattr-place :list) errno strerror file)))))))

(defun xattr-empty-p (file)
  "Return non-nil if FILE's xattrs is empty."
  (condition-case err
      (xattr-core-empty-p file)
    (xattr-errno
     (xattr-signal-common :list err file)
     (pcase err
       ((seq _ 'E2BIG _)
        (signal 'xattr-list-too-big (list file)))
       ((seq _ 'ENOTSUP _)
        (signal 'xattr-not-supported (list file)))
       ((seq sym errno strerror)
        (signal sym (list (xattr-place :list) errno strerror file)))))))

(provide 'xattr)
;;; xattr.el ends here
