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
(require 'xattr-map)

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

(ert-deftest xattr-test-empty-p ()
  "Test a empty xattrs"
  (xattr-test-with-temp-file file
    (should (xattr-empty-p file))
    (let ((keys (cl-loop repeat 10 collect (make-temp-name "user.key"))))
      (mapc (lambda (key) (xattr-set file key "")) keys)
      (should-not (xattr-empty-p file)))))

(ert-deftest xattr-test-empty-p-file-missing ()
  "Test signaling file-missing in empty-p"
  (should-error
   (xattr-empty-p (make-temp-name (expand-file-name "./nofilehere")))
   :type 'file-missing))

(defmacro xattr-test-with-map (var &rest body)
  "Create xattr map with default data.
Bind VAR to map and execute BODY."
  (let ((file (gensym "file")))
    `(xattr-test-with-temp-file ,file
       (let ((,var (xattr-map ,file)))
         (xattr-set ,file "user.key1" "value1")
         (xattr-set ,file "user.key2" "value2")
         (xattr-set ,file "user.key3" "value3")
         ,@body))))

(defmacro xattr-test-with-empty-map (var &rest body)
  "Create empty xattr map.
Bind VAR to map and execute BODY."
  (let ((file (gensym "file")))
    `(xattr-test-with-temp-file ,file
       (let ((,var (xattr-map ,file)))
         ,@body))))

(ert-deftest xattr-test-map-elt ()
  (xattr-test-with-map map
    (should (string= "value1" (map-elt map "user.key1")))
    (should (string= "value2" (map-elt map "user.key2")))
    (should (string= "value3" (map-elt map "user.key3")))
    (should-not (map-elt map "user.key4"))
    (should-not (map-elt map "user.key5"))
    (should-not (map-elt map "user.key6"))))

(ert-deftest xattr-test-map-elt-default ()
  (xattr-test-with-map map
    (should (string= "valueA" (map-elt map "user.key4" "valueA")))
    (should (string= "valueA" (map-elt map "user.key5" "valueA"))))
  (xattr-test-with-empty-map map
    (should (string= "valueA" (map-elt map "user.key6" "valueA")))))

(ert-deftest xattr-test-map-put! ()
  (xattr-test-with-map map
    (setf (map-elt map "user.key1") "valueA")
    (should (string= (map-elt map "user.key1") "valueA")))
  (xattr-test-with-map map
    (with-suppressed-warnings ((obsolete map-put))
      (map-put map "user.key1" "valueA"))
    (should (string= (map-elt map "user.key1") "valueA")))
  (xattr-test-with-map map
    (map-put! map "user.key1" "valueA")
    (should (string= (map-elt map "user.key1") "valueA"))))

(ert-deftest xattr-test-map-put!-new-keys ()
  "Test `map-put!' with new keys."
  (xattr-test-with-map map
    (setf (map-elt map "user.key4") "value4")
    (should (string= (map-elt map "user.key4") "value4"))))


(ert-deftest xattr-test-map-insert-error ()
  "`map-insert' should not be implemented for xattr."
  (xattr-test-with-empty-map map
    (should-error (map-insert map "user.key1" "valueA") :type 'map-inplace)))

(ert-deftest xattr-test-map-delete ()
  (xattr-test-with-map map
    (should (map-elt map "user.key1"))
    (should (eq map (map-delete map "user.key1")))
    (should-not (map-elt map "user.key1")))
  (xattr-test-with-map map
    (should-not (map-elt map "user.key4"))
    (should (eq map (map-delete map "user.key4")))
    (should-not (map-elt map "user.key4")))
  (xattr-test-with-empty-map map
    (should (eq map (map-delete map "user.key1")))))

(ert-deftest xattr-test-map-nested-elt ()
  (xattr-test-with-map map
    (should (map-nested-elt map ["user.key1"]))
    (should-error (map-nested-elt map ["user.key1" "user.key2"]))))

(ert-deftest xattr-test-mapp ()
  (xattr-test-with-empty-map map
    (should (mapp map)))
  (xattr-test-with-map map
    (should (mapp map))))

(ert-deftest xattr-test-map-keys ()
  (xattr-test-with-map map
    (should (equal (map-keys map) '("user.key1" "user.key2" "user.key3"))))
  (xattr-test-with-empty-map map
    (should-not (map-keys map))))

(ert-deftest test-map-values ()
  (xattr-test-with-map map
    (should (equal (map-values map) '("value1" "value2" "value3"))))
  (xattr-test-with-empty-map map
    (should-not (map-values map))))

(ert-deftest test-map-pairs ()
  (xattr-test-with-map map
    (should (equal (map-pairs map)
                   '(("user.key1" . "value1")
                     ("user.key2" . "value2")
                     ("user.key3" . "value3")))))
  (xattr-test-with-empty-map map
    (should-not (map-pairs map))))

(ert-deftest xattr-test-map-length ()
  (xattr-test-with-empty-map map
    (should (zerop (map-length map))))
  (xattr-test-with-map map
    (should (= 3 (map-length map)))))

(ert-deftest xattr-test-map-copy ()
  (xattr-test-with-map map
    (should-error (map-copy map))))

(ert-deftest xattr-test-map-apply ()
  (let ((fn (lambda (k v) (cons (intern k) v))))
    (xattr-test-with-map map
      (should (equal (map-apply fn map)
                     '((user.key1 . "value1") (user.key2 . "value2") (user.key3 . "value3")))))
    (xattr-test-with-empty-map map
      (should-not (map-apply fn map)))))

(ert-deftest xattr-test-map-do ()
  (let* (res
         (fn (lambda (k v)
               (push (list (intern k) v) res))))
    (xattr-test-with-empty-map map
      (should-not (map-do fn map))
      (should-not res))
    (xattr-test-with-map map
      (setq res nil)
      (should-not (map-do fn map))
      (should (equal res '((user.key3 "value3") (user.key2 "value2") (user.key1 "value1")))))))

(ert-deftest xattr-test-map-keys-apply ()
  (xattr-test-with-map map
    (should (equal (map-keys-apply (lambda (key) (concat key "A") ) map)
                   '("user.key1A" "user.key2A" "user.key3A"))))
  (xattr-test-with-empty-map map
    (let (ks)
      (should-not (map-keys-apply (lambda (k) (push k ks)) map))
      (should-not ks))))

(ert-deftest xattr-test-map-values-apply ()
  (xattr-test-with-map map
    (should (equal (map-values-apply (lambda (key) (concat key "A") ) map)
                   '("value1A" "value2A" "value3A"))))
  (xattr-test-with-empty-map map
    (let (vs)
      (should-not (map-values-apply (lambda (v) (push v vs)) map))
      (should-not vs))))

(ert-deftest xattr-test-map-filter ()
  (xattr-test-with-map map
    (should (equal (map-filter (lambda (_k v)
                                 (< 1 (string-to-number (substring v -1))))
                               map)
                   '(("user.key2" . "value2") ("user.key3" . "value3"))))
    (should (equal (map-filter (lambda (_k _v) t) map) (map-pairs map)))
    (should-not (map-filter #'ignore map)))
  (xattr-test-with-empty-map map
    (should-not (map-filter (lambda (_k _v) t) map))
    (should-not (map-filter #'ignore map))))

(ert-deftest xattr-test-map-remove ()
  (xattr-test-with-map map
    (should (equal (map-remove (lambda (_k v) (< 1 (string-to-number (substring v -1)))) map)
                   '(("user.key1" . "value1"))))
    (should (equal (map-remove #'ignore map) (map-pairs map)))
    (should-not (map-remove (lambda (_k _v) t) map)))
  (xattr-test-with-empty-map map
    (should-not (map-remove (lambda (_k _v) t) map))
    (should-not (map-remove #'ignore map))))

(ert-deftest xattr-test-map-empty-p ()
  (xattr-test-with-empty-map map
    (should (map-empty-p map)))
  (xattr-test-with-map map
    (should-not (map-empty-p map))))

(ert-deftest xattr-test-map-contains-key ()
  (xattr-test-with-empty-map map
    (should-not (map-contains-key map "user.key1"))
    (should-not (map-contains-key map "user.key2"))
    (should-not (map-contains-key map "user.key3")))
  (xattr-test-with-map map
    (should-not (map-contains-key map "user.key4"))
    (should (map-contains-key map "user.key1"))
    (should (map-contains-key map "user.key2"))))

(ert-deftest xattr-test-map-some ()
  (xattr-test-with-map map
    (should (eq (map-some (lambda (k _v) (and (string= k "user.key2") 'found)) map)
                'found))
    (should-not (map-some #'ignore map)))
  (xattr-test-with-empty-map map
    (should-not (map-some (lambda (_k _v) t) map))
    (should-not (map-some #'ignore map))))

(ert-deftest xattr-test-map-every-p ()
  (xattr-test-with-map map
    (should (map-every-p (lambda (_k _v) t) map))
    (should-not (map-every-p #'ignore map))
    (should-not (map-every-p (lambda (k _v) (string= "user.key1" k)) map)))
  (xattr-test-with-empty-map map
    (should (map-every-p (lambda (_k _v) t) map))
    (should (map-every-p #'ignore map))
    (should (map-every-p (lambda (k _v) (string= "user.key1" k)) map))))

(ert-deftest xattr-test-map-into ()
  (let* ((plist '(a 1 b 2))
         (alist '((a . 1) (b . 2)))
         (ht (map-into alist 'hash-table)))
    (should-error (map-into ht 'xattr-map) :type 'cl-no-applicable-method)
    (should-error (map-into alist 'xattr-map) :type 'cl-no-applicable-method)
    (should-error (map-into plist 'xattr-map) :type 'cl-no-applicable-method)
    (xattr-test-with-empty-map map
      (should (map-into map 'xattr-map)))))

(ert-deftest xattr-test-map-let ()
  (xattr-test-with-map map
    (map-let (("user.key1" key1) ("user.key2" key2) ("user.key4" key4)) map
      (should (string= key1 "value1"))
      (should (string= key2 "value2"))
      (should-not key4))))

(ert-deftest xattr-test-map-merge ()
  "Test `map-merge'."
  (should-error (map-merge 'xattr-map '("user.key1" "value1"))
                :type 'cl-no-applicable-method)
  (xattr-test-with-map map
    (should (equal (sort (map-pairs
                          (map-merge
                           'xattr-map map
                           '("user.key1" "valueA")
                           '(("user.key2" . "valueB")
                             ("user.key3" . "valueC"))
                           #s(hash-table data ("user.key2" "valueD"))))
                         (lambda (x y) (string< (car x) (car y))))
                   '(("user.key1" . "valueA")
                     ("user.key2" . "valueD")
                     ("user.key3" . "valueC"))))))

(ert-deftest xattr-test-map-merge-with ()
  (should-error (map-merge-with 'xattr-map #'concat '("user.key1" "value1"))
                :type 'cl-no-applicable-method)
  (xattr-test-with-map map
    (should (equal (sort (map-pairs
                          (map-merge-with
                           'xattr-map #'concat map
                           '("user.key1" "valueA")
                           '(("user.key2" . "valueB")
                             ("user.key3" . "valueC"))
                           #s(hash-table data ("user.key2" "valueD"))))
                         (lambda (x y) (string< (car x) (car y))))
                   '(("user.key1" . "value1valueA")
                     ("user.key2" . "value2valueBvalueD")
                     ("user.key3" . "value3valueC"))))))

(ert-deftest xattr-test-map-merge-empty ()
  "Test merging of empty maps."
  (xattr-test-with-empty-map map
    (should (equal (sort (map-pairs
                          (map-merge-with
                           'xattr-map #'concat map
                           '("user.key1" "valueA")
                           '(("user.key2" . "valueB")
                             ("user.key3" . "valueC"))
                           #s(hash-table data ("user.key2" "valueD"))))
                         (lambda (x y) (string< (car x) (car y))))
                   '(("user.key1" . "valueA")
                     ("user.key2" . "valueBvalueD")
                     ("user.key3" . "valueC"))))))

(ert-deftest xattr-test-map-pcase ()
  (xattr-test-with-map map
    (should (equal (pcase-let (((map ("user.key1" key1) ("user.key2" key2)) map))
                     (list key1 key2))
                   '("value1" "value2")))))

(provide 'xattr-test)
;;; xattr-test.el ends here

;; Local Variables:
;; eval: (dolist (sym '(xattr-test-with-temp-file xattr-test-with-map xattr-test-with-empty-map)) (put sym 'lisp-indent-function 1))
;; End:
