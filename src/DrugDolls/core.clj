(ns DrugDolls.core
  (:import (java.io BufferedReader FileReader)))
(use 'clojure.java.io)

(defstruct doll :name :weight :value)



(declare find-best-combination-memoized)
(declare decide-where-to-get-input-from)

(defn request-input
  "Prints [question] on one line, and accepts a string from the user"
  [question]
  (println (format "%s" question))
  (read-line)
)

(defn request-numeric-input
  "Runs request-input and trys to convert it to an integer, if it fails, lets user reenter the value"
  [question]
  (try 
    (Integer. (re-find #"[1-9][0-9]*" (request-input question)))
    (catch NumberFormatException e 
      (do
        (println "Please only enter numbers greater than 0")
        (request-numeric-input question) 
      )
    )
  )
)

(defn create-doll
  []
  (hash-map :name    (request-input "Enter Doll Name:")
            :weight  (request-numeric-input "Enter Doll Weight:")
            :value   (request-numeric-input "Enter Doll Value:")
  )
)

(defn enter-doll-information
  [number-to-enter]
  (vec
    (doall
        (repeatedly number-to-enter create-doll)
    )
  )
)

(defn find-best-combination
  [dolls-information weight-restriction index]
  (if (zero? weight-restriction)
    [0 []]
  ;else
    (if (< index 0)
      [0 []]
    ;else
      (let [{doll-weight :weight doll-value :value} (get dolls-information index)]
        (if (> doll-weight weight-restriction)
          (find-best-combination-memoized dolls-information weight-restriction (- index 1))
        ;else
          (let [[vn sn :as no]   (find-best-combination-memoized dolls-information weight-restriction (- index 1))
                [vy sy :as yes]  (find-best-combination-memoized dolls-information (- weight-restriction doll-weight) (- index 1))]
            (if (> (+ vy doll-value) vn)
              [(+ vy doll-value) (conj sy index)]
            ;else
              no
            )
          )
        )
      )
    )
  )
)

(def find-best-combination-memoized (memoize find-best-combination))

(defn print-results
  [names weights values]
  (if (empty? names)
    true
    (do
      (println (format "%-8s%5d%6d" (first names) (first weights) (first values)))
      (print-results (rest names) (rest weights) (rest values))
    )
  )
)

(defn take-input-from-user 
  []
  (let [weight-limit (request-numeric-input "What is the maximum weight?")
        number-to-enter  (request-numeric-input "How many dolls is there?")
        dolls-information (enter-doll-information number-to-enter)
        [total-value selected-dolls] (find-best-combination-memoized dolls-information weight-limit (- (count dolls-information) 1))
        names (map (comp :name dolls-information) selected-dolls)
        weights (map (comp :weight dolls-information) selected-dolls)
        values (map (comp :value dolls-information) selected-dolls)]
        (println (format "\nTotal Value: %d\npacked dolls:\n\n" total-value))
        (println "name    weight value")
        (if (print-results names weights values)
          [total-value names weights values]
          0
        )
  )
)

(defn recognize-doll-information
  [entry]
  (try
    (let [[doll-name doll-weight doll-value] (clojure.string/split entry #"\s")]
      (hash-map :name    doll-name
                :weight  (Integer. (re-find #"[1-9][0-9]*" doll-weight))
                :value   (Integer. (re-find #"[1-9][0-9]*" doll-value))
      )
    )
    (catch NumberFormatException e 
      (do
        (println "Invalid Input")
        (decide-where-to-get-input-from)
      )
    )
    (catch NullPointerException e
      (println "NullPointerException")
      (decide-where-to-get-input-from)
    )
  )
)

(defn read-user-specified-file 
  [path]
  (try
    (with-open [rdr (BufferedReader. (FileReader. path))]
      (let [file-contents (line-seq rdr)]
        (try
          (let [weight-limit (Integer. (re-find #"[1-9][0-9]*" (first file-contents)))
                dolls-information (vec (doall (map recognize-doll-information (rest file-contents))))
                [total-value selected-dolls] (find-best-combination-memoized dolls-information weight-limit (- (count dolls-information) 1))
                names (map (comp :name dolls-information) selected-dolls)
                weights (map (comp :weight dolls-information) selected-dolls)
                values (map (comp :value dolls-information) selected-dolls)]
            (println (format "\nTotal Value: %d\npacked dolls:\n\n" total-value))
            (println "name    weight value")
            (if (print-results names weights values)
              [total-value names weights values]
              0
            )
          )
          (catch NumberFormatException e 
            (do
              (println "Invalid Input")
              (decide-where-to-get-input-from)
            )
          )
          (catch NullPointerException e
            (println "NullPointerException")
            (decide-where-to-get-input-from)
          )
        )
      )
    )
    (catch Exception e
      (println "File not found")
      (decide-where-to-get-input-from)
    )
  )
)

(defn get-file-path-from-user
  []
  (read-user-specified-file (request-input "Please enter the path to the input file"))
)

(defn decide-where-to-get-input-from 
  []
  (let [choice (request-numeric-input "How would you like to enter data?\n1) Enter Data Manually\n2) Use Text File")]
    (cond
      (= choice 1) (take-input-from-user)
      (= choice 2) (get-file-path-from-user)
      :else (do (println "Invalid input, please try again.") (decide-where-to-get-input-from))
    ) 
  )
)