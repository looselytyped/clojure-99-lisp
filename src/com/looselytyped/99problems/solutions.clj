(ns com.looselytyped.99problems.solutions)

(defn my-last
  "P01. Find the last box of a list
  Example:
  * (my-last '(a b c d))
    (D)"
  [coll]
  (if (next coll)
    (recur (rest coll))
    (first coll)))

(defn idiomatic-last
  "P01. Find the last box of a list
  Example:
  * (my-last '(a b c d))
    (D)"
  [coll]
  (last coll))

(defn my-but-last
  "P02. Find the last but one box of a list
  Example:
  * (my-but-last '(a b c d))
    (C D)"
  [coll]
  (let [size (count coll)]
    (cond
     (= 2 size) coll
     :else (recur (rest coll)))))

(defn idiomatic-but-last
  "P02. Find the last but one box of a list
  Example:
  * (my-but-last '(a b c d))
    (C D)"
  [coll]
  (reverse (take 2 (reverse coll))))

(defn element-at
  "P03. Find the K'th element of a list
  Example:
  * (element-at '(a b c d e) 3)
    C"
  [coll index]
  (when (< (count coll) index) (throw (IndexOutOfBoundsException.)))
  (if (zero? index)
    (first coll) (recur (rest coll) (dec index))))

(defn my-count
  "P04. Find the number of elements of a list."
  [coll]
  (loop [c coll
         count 0]
    (if (seq c)
      (recur (rest c) (inc count))
      count)))

(defn idiomatic-count
  "P04. Find the number of elements of a list."  
  [coll]
  (count coll))

(defn my-reverse
  "P05. Reverse a list."
  [coll]
  (loop [c coll
         acc ()]
    (if (seq c)
      (recur (rest c) (cons (first c) acc))
      acc)))

(defn idiomatic-reverse
  "P05. Reverse a list."
  [coll]
  (reverse coll))

(defn palindrome?
  "P06. Find out whether a list is a palindrome."
  [coll]
  (if-let [c (seq coll)]
    (= c (my-reverse c))
    true))

(defn idiomatic-palindrome?
  "P06. Find out whether a list is a palindrome."
  [coll]
  (= (seq coll) (reverse coll)))

(defn- my-empty?
  "Private method to see if a collection is empty. My version,
  there is obviously empty? in clojure.core"
  [coll]
  (= 0 (my-count coll)))

(defn my-flatten
  "P07. Transform a list, possibly holding lists as elements 
  into a `flat' list by replacing each list with its elements (recursively).
  Example:
  * (my-flatten '(a (b (c d) e)))
    (A B C D E)"
  [coll]
  (if (my-empty? coll)
    coll
    (let [f (first coll)
          r (rest coll)]
      (if (seq? f)
        (concat (my-flatten f) (my-flatten r))
        (concat (list f) (my-flatten r))))))

(defn idiomatic-flatten
  "P07. Transform a list, possibly holding lists as elements 
  into a `flat' list by replacing each list with its elements (recursively).
  Example:
  * (my-flatten '(a (b (c d) e)))
    (A B C D E)"
  [coll]
  (flatten coll))

(defn compress
  "P08. Eliminate consecutive duplicates of list elements.
  Example:
  * (compress '(a a a a b c c a a d e e e e))
    (A B C A D E)"
  [coll]
  (loop [c coll 
         acc ()]
    (if (my-empty? c)
      acc
      (if (= (my-last acc) (first c))
        (recur (rest c) acc)
        (recur (rest c) (concat acc (list (first c))))))))

(defn idiomatic-compress
  "P08. Eliminate consecutive duplicates of list elements.
  Example:
  * (compress '(a a a a b c c a a d e e e e))
    (A B C A D E)"
  [coll]
  (seq (reduce (fn [acc val]
                 (if (not= (last acc) val)
                   (conj acc val) acc)) [] coll)))

(defn- my-butlast
  [coll]
  (loop [coll coll
         acc ()]
    (if (next coll)
      (recur (rest coll) (concat acc (list (first coll))))
      acc)))

(defn pack
  "P09. Pack consecutive duplicates of list elements into sublists.
  Example:
  * (pack '(a a a a b c c a a d e e e e))
    ((A A A A) (B) (C C) (A A) (D) (E E E E))"
  [coll]
  (loop [c coll
         acc ()]
    (if (my-empty? c)
      acc
      (let [f (first c)
            l (my-last acc)]
        (if (= (first l) f)
          (recur (rest c) (concat (my-butlast acc) (list (cons f l))))
          (recur (rest c) (concat acc (list (list f)))))))))

(defn idiomatic-pack
  "P09. Pack consecutive duplicates of list elements into sublists.
  Example:
  * (pack '(a a a a b c c a a d e e e e))
    ((A A A A) (B) (C C) (A A) (D) (E E E E))"
  [coll]
  (partition-by identity coll))

(defn my-encode
  "P10. Run-length encoding of a list.
  Example:
  * (encode '(a a a a b c c a a d e e e e))
    ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
  This version uses the previously defined 'pack' function.
  See my-encode1 that does pure recursion to get the same result"
  [coll]
  (loop [p (pack coll)
         acc ()]
    (if (my-empty? p)
      acc
      (recur (rest p) (concat acc (list (list (count (first p)) (first (first p)))))))))

(defn my-encode1
  "P10. Run-length encoding of a list.
  Example:
  * (encode '(a a a a b c c a a d e e e e))
    ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
  This version does not use 'pack'. Pure recursion only"
  [coll]
  (loop [coll coll
         acc ()]
    (if (my-empty? coll)
      acc
      (let [key (first coll)
            [last-count last-key] (my-last acc)]
        (if (and (not (my-empty? acc)) (= key last-key))
          (recur (rest coll) (concat (my-butlast acc) (list (list (+ 1 last-count) last-key))))
          (recur (rest coll) (concat acc (list (list 1 key)))))))))

(defn idiomatic-encode
  "P10. Run-length encoding of a list.
  Example:
  * (encode '(a a a a b c c a a d e e e e))
    ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
  Uses reduce. We can also use map (See idiomatic-encode1)"
  [coll]
  (reduce (fn [acc key]
            (let [[lc lk] (last acc)]
              (if (= key lk)
                (concat (butlast acc) (list (list (inc lc) lk)))
                (concat acc (list (list 1 key))))))
          '() coll))

(defn idiomatic-encode1
  "P10. Run-length encoding of a list.
  Example:
  * (encode '(a a a a b c c a a d e e e e))
    ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
  This uses the previously defined 'pack' along with map"
  [coll]
  (map #(list (count %) (second %)) (pack coll)))

(defn my-encode-modified
  "Modified run-length encoding
  Example:
  * (encode-modified '(a a a a b c c a a d e e e e))
    ((4 A) B (2 C) (2 A) D (4 E))"
  [coll]
  (loop [encoded (idiomatic-encode coll)
         acc ()]
    (let [[[count key] & rest] encoded]
      (if (my-empty? encoded)
        acc
        (if (= 1 count)
          (recur rest (concat acc (list key)))
          (recur rest (concat acc (list (list count key)))))))))

(defn idiomatic-encode-modified
  "Modified run-length encoding
  Example:
  * (encode-modified '(a a a a b c c a a d e e e e))
    ((4 A) B (2 C) (2 A) D (4 E))"
  [coll]
  (map (fn [[count key :as item]] (if (= 1 count) key item)) (idiomatic-encode coll)))

(defn decode
  "P12. Decode a run-length encoded list.
  Given a run-length code list generated as specified by 'encode-modified'
  return it's uncompressed version
  Example: 
  * (decode '((4 a) b (2 c) (2 a) d (4 e)))
    (a a a a b c c a a d e e e e)"
  [coll]
  (letfn [(_repeat [[count key]]
            (when (< 0 count)
              (cons key (_repeat (list (dec count) key)))))]
    (loop [coll coll
           acc ()]
      (if (my-empty? coll)
        acc
        (if (seq? (first coll))
          (recur (rest coll) (concat acc (_repeat (first coll))))
          (recur (rest coll) (concat acc (list (first coll)))))))))

(defn idiomatic-decode
  "P12. Decode a run-length encoded list.
  Given a run-length code list generated as specified by 'encode-modified'
  return it's uncompressed version
  Example: 
  * (decode '((4 a) b (2 c) (2 a) d (4 e)))
    (a a a a b c c a a d e e e e)"
  [coll]
  (flatten (map #(if (coll? %) (repeat (first %) (second %)) %) coll)))


(defn duplicate
  "P13. Duplicate the elements of a list.
   Example:
   * (dupli '(a b c c d))
   (A A B B C C C C D D)"
  [coll]
  (loop [c coll
         acc ()]
    (if (my-empty? c)
      acc
      (recur (rest c) (concat acc (list (first c) (first c)))))))

(defn idiomatic-duplicate
  "P13. Duplicate the elements of a list.
   Example:
   * (dupli '(a b c c d))
   (A A B B C C C C D D)"
  [coll]
  (flatten (map #(list % %) coll)))

(defn my-replicate
  "Replicate the elements of a list a given number of times.
   Example:
   * (repli '(a b c) 3)
   (A A A B B B C C C)"
  [coll n]
  )