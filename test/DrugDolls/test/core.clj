(ns DrugDolls.test.core
  (:use [DrugDolls.core])
  (:use [clojure.test]))

(deftest atomic-object-testdata
  (is 
  	(= (read-user-specified-file "./aoinput.txt") [1030 '("luke" "anthony" "candice" "dorothy" "puppy" "randal" "marc" "grumpkin" "dusty" "grumpy" "eddie" "sally") '(9 13 153 50 15 27 11 42 43 22 7 4) '(150 35 200 160 60 60 70 70 75 80 20 50)]) 
  )
)

(deftest weight-but-no-dolls-testdata
  (is 
  	(= (read-user-specified-file "./nodollsinput.txt") [0 '() '() '()]) 
  )
)

(deftest unl-professor-testdata
  (is 
  	(= (read-user-specified-file "./unlinput.txt") [26 '("Sammy" "Chris" "Brett" "Brick") '(5 9 2 4) '(8 10 3 5)])
  )
)

(deftest ntl-small-testdata
  (is 
  	(= (read-user-specified-file "./ntlworldsmallinput.txt") [1149 '("X19" "X28" "X29") '(5 5 5) '(381 386 382)] )
  )
)

(deftest ntl-medium-testdata
  (is 
  	(= (read-user-specified-file "./ntlworldmediuminput.txt") [1173 '("X58" "X93" "X97") '(9 10 8) '(384 390 399)] )
  )
)