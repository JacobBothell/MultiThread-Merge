(ns cs441
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  (:import (java.util.concurrent Executors)))

;reads in the file
; not the best/fastes way to do this
(defn get-lines [file]
  (map read-string
       (str/split-lines
         (slurp file))))

;;Partition numbers into x parts
(defn split
  [num x]
  (partition (/ (count num) x) num))


;;;Merge Sort Algorithm pulled from https://gist.github.com/alco/2135276
;;;On 4/26/2017

(defn merge-seqs
  "Merges two sorted sequences into a single sorted sequence"
  ([left right]
   (merge-seqs (list left right)))
  ([[left right]]
   (loop [l left, r right, result []]                            ;loops through the list
     (let [lhead (first l), rhead (first r)]                     ;pulls the first item
       (cond                                                     ;conditional for selecting next item
         (nil? lhead)     (concat result r)                      ;nil checks if the list is empty
         (nil? rhead)     (concat result l)                      ;concat dosent need a comment
         (<= lhead rhead) (recur (rest l) r (conj result lhead)) ;conditional to order the items
         true             (recur l (rest r) (conj result rhead)))))))

(defn mergesort
  "Produces a sorted sequence from an input sequence.
  Works best with vectors (since it uses 'count' internally)."
  [xs]
  ((fn mergesort-counted [xs n]
     (if (<= n 1)
       xs
       (let [middle (bit-shift-right n 1)]  ; fast division by 2
         (merge-seqs                        ; call to bring them together
           (map mergesort-counted           ; recursive call
                (split-at middle xs)        ; two pieces
                [middle (- n middle)])))))  ; count of each half
    xs (count xs)))

(print "# of lines in the file: " (count (get-lines "resources/numbers.txt")) "\n")

(print "Running Single Thread mergesort: ")
(time (mergesort (get-lines "resources/numbers.txt")))

;General Algorithm
;
;    1. Read in file
;    2. Split the data into x number of seperate items
;    3. perform a merge sort on the seperate items
;    4. allow them to run multiple threads
;    5. merge seperate items into one item
;    6. print execution time

(print "Running 2 Threads flatten mergesort: ")
(time (merge-seqs (pmap mergesort (split (get-lines "resources/numbers.txt") 2))))

(print "Running 4 Threads mergesort: ")
(time (merge-seqs (pmap mergesort (split (get-lines "resources/numbers.txt") 4))))

(print "Running 8 Threads mergesort: ")
(time (merge-seqs (pmap mergesort (split(get-lines "resources/numbers.txt") 8))))

(print "Running 16 Threads mergesort: ")
(time (merge-seqs (pmap mergesort (split (get-lines "resources/numbers.txt") 16))))

(print "Running 32 Threads mergesort: ")
(time (merge-seqs (pmap mergesort (split (get-lines "resources/numbers.txt") 32))))