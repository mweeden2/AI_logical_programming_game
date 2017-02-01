;;; Sample CLIPS program: Automobile diagnosis by Sandip Sen, sandip-sen@utulsa.edu
;;; Adapted from OPS5 program Provided by Michael Mauldin, mlm@cs.cmu.edu.

;(reset-ops)
;(watch 0)
;(strategy lex)

(deftemplate task
  (slot goal)) ; Task name

(deftemplate fact 
	(slot name) ; Question to ask user [Y/N]
	(slot value)) ; Answer to question

(deffacts messages
  (task (goal start))
  (fact (name "spark at spark plugs"))
  (fact (name "carburetor smells like gasoline"))
  (fact (name "fuel gauge shows empty"))
  (fact (name "headlights are dim or dead"))
  (fact (name "engine is turning"))
  (fact (name "key is off")))

(defrule initialize
    ?task <- (task (goal start))
    =>
    (printout t crlf crlf "****************************************" crlf)
    (printout t "** Automobile Diagnosis Expert System **" crlf)
    (printout t "****************************************" crlf crlf)
    (modify ?task (goal diagnose)))

;;; ask-user: Ask the user about a fact
(defrule ask-user
    (task (goal diagnose))
    ?fact <- (fact (name ?name) (value nil))
    =>
    (printout t crlf "Is this true:" ?name "? (q to quit) ")
    (bind ?input (read))
    (modify ?fact (value ?input)))

;;; make-yes-answer: Force a yes answer to be 'yes'
(defrule make-yes-answer
    (task (goal diagnose))
    ?fact <- (fact (value y))
    =>
    (modify ?fact (value yes)))

;;; make-no-answer: Force a no answer to be 'no'
(defrule make-no-answer
    (task (goal diagnose))
    ?fact <- (fact (value n))
    =>
    (modify ?fact (value no)))

;;; force-yes-or-no: Wipe out bad answers
(defrule force-correct-answer
    (task (goal diagnose))
    ?fact <- (fact (value ?wrong&~nil&~yes&~y&~no&~n&~q&~quit))
    =>
    (printout t crlf "Invalid response " ?wrong ", please answer yes or no")
    (modify ?fact (value nil)))

;;; quit: Quit

(defrule quit
    (task (goal diagnose))
    (fact (value q|quit))
    =>
    (halt))

(defrule key-is-off
    ?task <- (task (goal diagnose))
    (fact (name "key is off") (value yes))
    =>
;    (assert (fact (name "you must turn the key to start the car") (value yes)))
    (printout t crlf "Diagnosis: you must turn the key to start the car!" crlf)
    (modify ?task (goal clean)))

(defrule ignition-or-fuel
    (task (goal diagnose))
    (fact (name "key is off") (value no))
    (fact (name "engine is turning") (value yes))
    =>
    (assert (fact (name "problem is in fuel or ignition system") (value yes)))
    (printout t crlf "Diagnosis: problem is in fuel or ignition system" crlf))

(defrule bad-starting-system
    (task (goal diagnose))
    (fact (name "key is off") (value no))
    (fact (name "engine is turning") (value no))
    =>
    (assert (fact (name "problem is in starting system") (value yes)))
    (printout t crlf "Diagnosis: problem is in starting system" crlf))

(defrule out-of-gas
    ?task <- (task (goal diagnose))
    (fact (name "fuel gauge shows empty") (value yes))
    =>
;    (assert (fact (name "out of gas") (value yes)))
    (printout t crlf "Diagnosis: out of gas" crlf)
    (modify ?task (goal clean)))

(defrule engine-flooded
    ?task <- (task (goal diagnose))
    (fact (name "problem is in fuel or ignition system") (value yes))
    (fact (name "carburetor smells like gasoline") (value yes))
    (fact (name "spark at spark plugs") (value yes))
    =>
;    (assert (fact (name "engine is flooded: wait 15 minutes") (value yes)))
    (printout t crlf "Diagnosis: engine is flooded: wait 15 minutes" crlf)
    (modify ?task (goal clean)))

(defrule bad-ignition
    ?task <- (task (goal diagnose))
    (fact (name "problem is in fuel or ignition system") (value yes))
    (fact (name "headlights are dim or dead") (value no))
    (fact (name "spark at spark plugs") (value no))
    =>
;    (assert (fact (name "bad ignition system") (value yes)))
    (printout t crlf "Diagnosis: bad ignition system" crlf)
    (modify ?task (goal clean)))

(defrule bad-battery
    ?task <- (task (goal diagnose))
    (fact (name "headlights are dim or dead") (value yes))
    =>
;    (assert (fact (name "dead battery") (value yes)))
    (printout t crlf "Diagnosis: dead battery" crlf)
    (modify ?task (goal clean)))

(defrule bad-starter
    ?task <- (task (goal diagnose))
    (fact (name "problem is in starting system") (value yes))
    (fact (name "headlights are dim or dead") (value no))
    =>
;    (assert (fact (name "bad starter") (value yes)))
    (printout t crlf "Diagnosis: bad starter" crlf)
    (modify ?task (goal clean)))

(defrule bad-fuel-pump
    ?task <- (task (goal diagnose))
    (fact (name "problem is in fuel or ignition system") (value yes))
    (fact (name "carburetor smells like gasoline") (value no))
    (fact (name "fuel gauge shows empty") (value no))
    =>
;    (assert (fact (name "problem in fuel system; bad fuel pump or filter") (value yes)))
    (printout t crlf "Diagnosis: problem in fuel system; bad fuel pump or filter" crlf)
    (modify ?task (goal clean)))

(defrule clean-up
    (task (goal clean))
    =>
    (reset)
    (printout t crlf "*End of diagnosis*" crlf crlf))
