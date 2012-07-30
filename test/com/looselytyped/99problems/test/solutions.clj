(ns com.looselytyped.99problems.test.solutions
  (:require [clojure.test :refer :all]
            [com.looselytyped.99problems.solutions :refer :all]))

(deftest test-my-last
  []
  (are [act exp] [= act exp]
       nil (my-last [])
       1 (my-last [1])
       4 (my-last [1 3 4])))

(deftest test-my-but-last
  []
  (are [act exp] (= act exp)
       '(1 2) (my-but-last [1 2])
       '(3 4) (my-but-last [1 2 3 4])))

(deftest test-element-at
  []
  (is (= 12 (element-at [11 12 13] 1))))

(deftest test-my-count
  []
  (are [act exp] (= act exp)
       0 (my-count '())
       10 (my-count (range 10))))

(deftest test-my-reverse
  []
  (are [act exp] (= act exp)
       () (my-reverse ())
       '(1) (my-reverse [1])
       '(3 2 1) (my-reverse [1 2 3])))

(deftest test-palindrome
  []
  (are [act exp] (= act exp)
       true (palindrome? ()) ;empty list is a palindrome?
       false (palindrome? [1 2 3])
       true (palindrome? [3 2 3])
       false (palindrome? "Raju")
       true (palindrome? "RaaR")))

(deftest test-idiomatic-palindrome
  []
  (are [act exp] (= act exp)
       false (idiomatic-palindrome? [1 2 3])
       true (idiomatic-palindrome? [3 2 3])
       false (idiomatic-palindrome? "Raju")))


(deftest test-my-flatten
  []
  (are [act exp] (= act exp)
       () (my-flatten ())
       '(a) (my-flatten '(a))
       '(a b) (my-flatten '(a b))
       '(a b c) (my-flatten '(a b c))
       '(a b c) (my-flatten '(a b (c)))
       '(a b c d) (my-flatten '(a b (c d)))
       '(a b c d e) (my-flatten '(a (b (c d) e)))))

(deftest test-compress
  []
  (are [act exp] (= act exp)
       () (compress ())
       '(a b c d) (compress '(a b b c c c c d))))

(deftest test-pack
  []
  (are [act exp] (= act exp)
       () (pack '())
       '((a)) (pack '(a))
       '((a) (b b) (c c c c) (d) (a a) (b)) (pack '(a b b c c c c d a a b))))

(deftest test-myencode
  []
  (are [act exp] (= act exp)
       () (my-encode '())
       '((1 a)) (my-encode '(a))
       '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e)) (my-encode '(a a a a b c c a a d e e e e))))

(deftest test-myencode1
  []
  (are [act exp] (= act exp)
       () (my-encode1 '())
       '((1 a)) (my-encode1 '(a))
       '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e)) (my-encode1 '(a a a a b c c a a d e e e e))))

(deftest test-idiomatic-encode
  []
  (are [act exp] (= act exp)
       () (idiomatic-encode '())
       '((1 a)) (idiomatic-encode '(a))
       '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e)) (idiomatic-encode '(a a a a b c c a a d e e e e))))

(deftest test-my-encode-modified
  []
  (are [act exp] (= act exp)
       () (my-encode-modified ())
       '((4 a) b (2 c) (2 a) d (4 e)) (my-encode-modified '(a a a a b c c a a d e e e e))))

(deftest test-idiomatic-encode-modified
  []
  (are [act exp] (= act exp)
       () (idiomatic-encode-modified ())
       '((4 a) b (2 c) (2 a) d (4 e)) (my-encode-modified '(a a a a b c c a a d e e e e))))

(deftest test-decode
  []
  (are [act exp] (= act exp)
       () (decode ())
       '(a a a a b c c a a d e e e e) (decode '((4 a) b (2 c) (2 a) d (4 e)))))

(deftest test-idiomatic-decode
  []
  (are [act exp] (= act exp)
       () (idiomatic-decode ())
       '(a a a a b c c a a d e e e e) (decode '((4 a) b (2 c) (2 a) d (4 e)))))


