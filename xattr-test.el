;;; xattr-test.el --- Tests for xattr                -*- lexical-binding: t; -*-

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

;; Tests for xattr

;;; Code:

(require 'ert)
(require 'xattr)

(defmacro xattr-test-with-temp-file (variable &rest body)
  "Wrap BODY in a let form with VARIABLE bind to a temporary file."
  `(let ((,variable (make-temp-file (expand-file-name "./test"))))
     (unwind-protect (progn ,@body)
       (delete-file ,variable t))))

(ert-deftest xattr-test-temp-file ()
  "Test creation of temparary random file."
  (let ((f nil))
    (xattr-test-with-temp-file file
      (should (file-exists-p file))
      (setq f file))
    (should-not (file-exists-p f))))

(ert-deftest xattr-test-set ()
  "Test setting xattrs"
  (xattr-test-with-temp-file file
    (let ((key "user.key") (value "value1"))
      (xattr-set file key value)
      (should (string= (xattr-get file key) value))
      (should (equal (xattr-list file) (list key)))
      (xattr-remove file key)
      (should-not (xattr-list file)))))

(ert-deftest xattr-test-set-empty-value ()
  "Test setting empty values"
  (xattr-test-with-temp-file file
    (let ((key "user.key") (value ""))
      (xattr-set file key value)
      (should (string= (xattr-get file key) value))
      (should (equal (xattr-list file) (list key)))
      (xattr-remove file key)
      (should-not (xattr-list file)))))

(ert-deftest xattr-test-set-gibberish-value ()
  "Test setting gibberish values"
  (xattr-test-with-temp-file file
    (let ((key "user.key") (value (concat "random" (string 34 3 252 0 65 2 3 4) "random")))
      (xattr-set file key value)
      (should (string= (xattr-get file key) value))
      (should (equal (xattr-list file) (list key)))
      (xattr-remove file key)
      (should-not (xattr-list file)))))

(ert-deftest xattr-test-set-gibberish-key ()
  "Test setting gibberish keys"
  (xattr-test-with-temp-file file
    (let ((key (concat "user." (string 34 3 252 46466 65 2 3 4))) (value "value"))
      (xattr-set file key value)
      (should (string= (xattr-get file key) value))
      (should (equal (xattr-list file) (list key)))
      (xattr-remove file key)
      (should-not (xattr-list file)))))

(ert-deftest xattr-test-set-file-missing ()
  "Test signaling file-missing in set"
  (should-error
   (xattr-set (make-temp-name (expand-file-name "./nofilehere")) "user.key" "value")
   :type 'file-missing))

(ert-deftest xattr-test-set-invalid-prefix ()
  "Test signaling invalid prefix error in set"
  (xattr-test-with-temp-file file
    (should-error
     (xattr-set file (concat (make-temp-name "surelynoprefixy") ".key") "value")
     :type 'xattr-not-supported-or-invalid-prefix)))

(ert-deftest xattr-test-set-empty-name ()
  "Test signaling empty name error in set"
  (xattr-test-with-temp-file file
    (should-error (xattr-set file "user." "value") :type 'xattr-empty-name)))

(ert-deftest xattr-test-get-absent ()
  "Test getting absent xattr with no error"
  (xattr-test-with-temp-file file
    (should-not (xattr-get file "user.absent"))))

(ert-deftest xattr-test-get-absent-error ()
  "Test getting absent xattr with error"
  (xattr-test-with-temp-file file
    (should-error (xattr-get file "user.absent" t)
                  :type 'xattr-absent-attribute-or-no-access)))

(ert-deftest xattr-test-get-invalid-prefix ()
  "Test signaling invalid prefix error in get"
  (xattr-test-with-temp-file file
    (should-error
     (xattr-get file (concat (make-temp-name "surelynoprefixy") ".key"))
     :type 'xattr-not-supported-or-invalid-prefix)))

(ert-deftest xattr-test-get-empty-name ()
  "Test signaling empty name error in get"
  (xattr-test-with-temp-file file
    (should-error (xattr-get file "user.") :type 'xattr-empty-name)))

(ert-deftest xattr-test-get-file-missing ()
  "Test signaling file-missing in get"
  (should-error
   (xattr-get (make-temp-name (expand-file-name "./nofilehere")) "user.key")
   :type 'file-missing))

(ert-deftest xattr-test-remove-absent ()
  "Test removing absent xattr with no error"
  (xattr-test-with-temp-file file
    (should-not (xattr-remove file "user.absent"))))

(ert-deftest xattr-test-remove-absent-error ()
  "Test removing absent xattr with error"
  (xattr-test-with-temp-file file
    (should-error (xattr-remove file "user.absent" t) :type 'xattr-absent-attribute)))

(ert-deftest xattr-test-remove-invalid-prefix ()
  "Test signaling invalid prefix error in remove"
  (xattr-test-with-temp-file file
    (should-error
     (xattr-remove file (concat (make-temp-name "surelynoprefixy") ".key"))
     :type 'xattr-not-supported-or-invalid-prefix)))

(ert-deftest xattr-test-remove-empty-name ()
  "Test signaling empty name error in remove"
  (xattr-test-with-temp-file file
    (should-error (xattr-remove file "user.") :type 'xattr-empty-name)))

(ert-deftest xattr-test-remove-file-missing ()
  "Test signaling file-missing in remove"
  (should-error
   (xattr-remove (make-temp-name (expand-file-name "./nofilehere")) "user.key")
   :type 'file-missing))

(ert-deftest xattr-test-list ()
  "Test a lot of keys in xattr"
  (xattr-test-with-temp-file file
    (let ((keys (cl-loop repeat 10 collect (make-temp-name "user.key"))))
      (mapc (lambda (key) (xattr-set file key "")) keys)
      (cl-set-difference (xattr-list file) keys :test #'string=))))

(ert-deftest xattr-test-list-file-missing ()
  "Test signaling file-missing in list"
  (should-error
   (xattr-list (make-temp-name (expand-file-name "./nofilehere")))
   :type 'file-missing))

(provide 'xattr-test)
;;; xattr-test.el ends here

;; Local Variables:
;; eval: (put 'xattr-test-with-temp-file 'lisp-indent-function 1)
;; End:
