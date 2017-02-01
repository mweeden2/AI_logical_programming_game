;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; Set up templates
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(deftemplate objct 
  (slot name) (slot location) (slot edible?) (slot isa) (slot used))
(deftemplate person 
  (slot location) (slot Credits) (slot Salary) (slot Moves) (slot Ate) (slot Slept))
(deftemplate place 
  (slot name) (slot north) (slot south) (slot east) (slot west) (slot info))
(deftemplate door 
  (slot name) (slot from) (slot to) (slot status) (slot direction))
(deftemplate mode (slot status))
;(deftemplate input (slot command) (slot argument))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; Set up templates
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; YOU ARE ENCOURAGED TO ADD/ALTER PLACES AND THEIR "info" DESCRIPTIONS 
(deffacts TU
  (mode (status start))
  (place (name Graduation) (south ACAC)
	 (info "
	 ...\"Please rise for the national anthem.\"...
	 
	  Your cap feels strange over your wayword hair.
	  Mom rolls her eyes at your uncle's cigar.
	  You've stood and unfolded in these rains, but 
	  There's a new kind of storm on the radar."))
  (place (name Summer_Job) (east ACAC) (info "~Isn't this supposed to be a summer camp?~"))
  (place (name 4253) (east Keplinger) (info "~Most fun I have ever had in a course!!~"))
  (place (name 1043) (east McFarlin) (info "~What's a java?~"))
  (place (name Dorm_Room) (east McClure) (info "~All work and no sleep~"))
  (place (name Cafeteria) (east Skelly) (info "~Lots to eat; mind your cholesterol~"))
  (place (name Orientation) (north Skelly) (info "~Welcome to wonderland!~"))
  (place (name Skelly) (north McClure) (south Orientation) (east Kendall)
	 (west Cafeteria) (info "~Go Blue~"))
  (place (name McClure) (north McFarlin) (south Skelly) (east Registration) (west Dorm_Room)
	 (info "~Empty your pockets!! Right now!!~"))
  (place (name McFarlin) (north Keplinger) (south McClure) (east 2003) (west 1043)
	 (info "~A quiet place to watch YouTube~"))
  (place (name Keplinger) (north ACAC) (south McFarlin) (east 3123) (west 4253)
	 (info "~Home, sweet home~"))
  (place (name ACAC) (north Graduation) (south Keplinger) (east Job_Interview) 
	 (west Summer_Job) (info "~Everyone and your mail is here~"))
  (place (name Job_Interview) (west ACAC) (info "~Ah, yes. I know what you're saying~"))
  (place (name 3123) (west Keplinger) (info "~Another day, another course~"))
  (place (name 2003) (west McFarlin) (info "~Now you are coding~"))
  (place (name Registration) (west McClure) (info "Enlist and serve"))
  (place (name Kendall) (west Skelly) (info "~If you can act, sing, or dance; the stage awaits~"))
  (objct (name beet) (location Cafeteria) (isa food))
  (objct (name cookie) (location Cafeteria) (isa food))
  (objct (name map) (location Orientation) (isa paper))
  (objct (name schedule) (location Registration) (isa paper))
  (objct (name diploma) (location Graduation) (isa paper))
  (objct (name food) (edible? yes) (isa Object))
  (objct (name paper) (edible? no) (isa Object))
  (objct (name Object))
  (door (name sum_job) (from ACAC) (direction west) (status closed))
  (door (name job_int) (from ACAC) (direction east) (status closed))
  (door (name grad) (from ACAC) (direction north) (status closed))
  (person (location Skelly) (Credits 0.0) (Salary 0) (Moves 0) (Ate 0) (Slept 0))
  (more_class_credit_oneshot))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Production that detects a starting position and performs necessary makes to set up
;; the environment.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; modified

(defrule start
   ?mode <- (mode (status start))
   =>
   (modify ?mode (status run))
   (printout t crlf "    ***************************")
   (printout t crlf "    *                         *")
   (printout t crlf "    * College:  The Adventure *")
   (printout t crlf "    *          By             *")
   (printout t crlf "    *      Sandip Sen         *")
   (printout t crlf "    *                         *")
   (printout t crlf "    *       Edited By         *")
   (printout t crlf "    *      Matt Weeden        *")
   (printout t crlf "    ***************************" crlf crlf)
   (printout t crlf "Hint: All the freshmen are headed south.")
   (printout t crlf "Enter \"help\" for the help screen."))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Prompts the player for next input and reads it in
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; modified

(defrule Read
   ?mode <- (mode (status run))
   =>
   (modify ?mode (status run))
   (printout t crlf crlf crlf "*** What next?  ")
   (assert (input (explode$ (readline)))))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Response to an invalid command
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; not original

(defrule Dont-Understand
   ?input <- (input $?x)
   =>
   (retract ?input)
   (printout t crlf "Invalid command"))
   
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Extra responses to an invalid command
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule invalid_go
   ?input <- (input go $? ?verb)
   =>
   (retract ?input)
   (printout t crlf "There's no time for " ?verb " in college."))
   
(defrule invalid_see
   ?input <- (input see $? ?x)
   (objct (name ?x))
   =>
   (retract ?input)
   (printout t crlf "Try picking ip up instead."))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; Allows you to stop and restart very easy -- just type run again.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; modified

(defrule quit
 ?input <- (input stop|quit|q|exit|bye|halt)
  =>
 (retract ?input)
 (halt))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; Providing help to the player.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; modified

(defrule help
  ?input <- (input $? help|h $?)
  =>
  (printout t crlf "Help:" crlf)
  (printout t "     Goal: to graduate college with the highest score." crlf)
  (printout t crlf "     Tasks to complete:" crlf)
  (printout t "          Sleep" crlf)
  (printout t "          Eat" crlf)
  (printout t "          Get your schedule" crlf)
  (printout t "          Go to class" crlf)
  (printout t "          Go to a summer job" crlf)
  (printout t "          Go to a job interview" crlf)
  (printout t "          Pick up your diploma" crlf)
  (printout t crlf "     Hints:")
  (printout t crlf "        -Try dropping papers in relevent locations")
  (printout t crlf "        -Some doors open when you have a certain")
  (printout t crlf "         object or when you've visited a certain room")
  (printout t crlf "        -Some objects reappear after you leave a room")
  (printout t crlf "        -Prerequisites are strictly enforced")
  (printout t crlf "     Valid commands include:" crlf)
  (printout t crlf "         Command         Explanation")
  (printout t crlf "      - go south        try to go to the room to the south")
  (printout t crlf "      - grab that beet  try to pick up the beet")
  (printout t crlf "      - look around     describe current location and")
  (printout t crlf "                           neighboring locations if you")
  (printout t crlf "                           have the map")
  (printout t crlf "      - drop the map    drop the map if you have it")
  (printout t crlf "      - eat cookie      try to eat the cookie")
  (printout t crlf "      - go to sleep     try to go to sleep")
  (printout t crlf "      - inventory       print a list of possessions,")
  (printout t crlf "                          current score, and status")
  (printout t crlf "                          information")
  (printout t crlf "      - quit            quit College: The Adventure")
  (retract ?input))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Describing the current location of the player when he possesses the campus map.
;; NOTE: superfluous conditions and attributes have been added so that combined with
;; recency and specificity this rule fires before the immediately following ones.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; modified

(defrule location_info
   ?input <- (input describe|look $?)
   (person (location ?x)) 
   (place (name ?x) (info ?y&~nil))
   (objct (name map) (location player))
   (mode (status run))
   =>
   (printout t crlf "You are at " ?x crlf ?y crlf)
   (assert (see_directions ?x)))
   ;;(retract ?input))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;   The following productions prompt user about the places to the north, south, east
;; and west (if present) to the current location if he/she has the campus map.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule see_west
   ?x <- (see_directions ?l)
   (place (name ?l) (west ?w&~nil))
   =>
   (printout t crlf ?w " is to the west."))
   
(defrule see_south
   ?x <- (see_directions ?l)
   (place (name ?l) (south ?s&~nil))
   =>
   (printout t crlf ?s " is to the south."))
   
(defrule see_east
   ?x <- (see_directions ?l)
   (place (name ?l) (east ?e&~nil))
   =>
   (printout t crlf ?e " is to the east."))
   
(defrule see_north
   ?x <- (see_directions ?l)
   (place (name ?l) (north ?n&~nil))
   =>
   (printout t crlf ?n " is to the north."))
   
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  The less specific rule which removes the see_directions fact after it has been 
;; processed.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule remove_see
	?x <- (see_directions ?)
	=>
	(printout t crlf)
	(retract ?x))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Describing current location to the user if he does not have campus map. NOTE: there
;; is a superfluous 4th condition on LHS to make this rule fire before see_objcts.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; modified

(defrule without_map_info
   ?input <- (input describe|look $?)
   (person (location ?x))
   (place (name ?x) (info ?y&~nil))
   (not (objct (name map) (location player)))
   (mode (status run))
   =>
   (printout t crlf "You are at " ?x crlf ?y crlf))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Describes whatever the player can see in the current location.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule see_objcts
   ?input <- (input describe|look $?)
   (person (location ?l))
   (objct (name ?o) (location ?l))
   =>
   (printout t crlf "You can see the " ?o "."))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  The less specific rule which removes the input after it has been processed.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule remove_describe
   ?input <- (input describe|look $?)
   =>
   (retract ?input))
	
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Lists the objcts in possession of the player.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule have_objcts
   ?x <- (have_objects)
   (input status|score|inventory)
   (objct (name ?n) (location player))
   =>
   (printout t crlf "You have a " ?n "."))
   
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; Prints status information.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; not original

(defrule current_status
   (input status|score|inventory)
   (person (Credits ?c) (Salary ?s) (Ate ?a) (Slept ?sl) (Moves ?m))
   =>
   (printout t crlf "You have " ?c " credits.")
   (printout t crlf "Your salary is $" (* ?s 1000)) 
   (printout t crlf "You have eaten " ?a " times.")
   (printout t crlf "You have slept " ?sl " times.")
   (printout t crlf "You have made " ?m " moves." crlf)
   (assert (have_objects)))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	Less specific default rule to remove command after processing.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; not original

(defrule remove_status
   ?input <- (input status|score|inventory)
   ?x <- (have_objects)
   =>
   (retract ?input)
   (retract ?x))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Production to aid player if he loses campus map.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule lost
   ?x <- (just_dropped map)
   (person (location ?l))
   ?map <- (objct (name map) (location ?l))
   =>
   (printout t crlf crlf "Looks like you're lost.")
   (printout t crlf "Hint: It looks like the freshmen are headed south.")
   (retract ?map)  	;; retract the map so that the player must go to Orientation again
   (retract ?x))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The following productions processes the directional movement command by first
;;  changing the position as asked and then describing the new position with the help
;;  of other productions which describes current location of the player. Also increments
;;  the number of moves made by the player.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule go_south
   ?input <- (input $? south|s)
   ?person <- (person (location ?l) (Moves ?m))
   (place (name ?l) (south ?new))
   (place (name ?new))
   =>
   (modify ?person (location ?new) (Moves (+ ?m 1)))
   (retract ?input)
   (assert (input look)))

(defrule go_north
   ?input <- (input $? north|n)
   ?person <- (person (location ?l) (Moves ?m))
   (place (name ?l) (north ?new))
   (place (name ?new))
   =>
   (modify ?person (location ?new) (Moves (+ ?m 1)))
   (retract ?input)
   (assert (input look)))

(defrule go_east
   ?input <- (input $? east|e)
   ?person <- (person (location ?l) (Moves ?m))
   (place (name ?l) (east ?new))
   (place (name ?new))
   =>
   (modify ?person (location ?new) (Moves (+ ?m 1)))
   (retract ?input)
   (assert (input look)))

(defrule go_west
   ?input <- (input $? west|w)
   ?person <- (person (location ?l) (Moves ?m))
   (place (name ?l) (west ?new))
   (place (name ?new))
   =>
   (modify ?person (location ?new) (Moves (+ ?m 1)))
   (retract ?input)
   (assert (input look)))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The following production checks if a closed door prevents the player from 
;;  moving in the direction he desires, and prompts him so. NOTE: only 3 doors are
;;  closed and the player can be stopped by these if he tries to move either east, west
;;  or north from ACAC and the corresponding door is closed.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule closed_door_north
  ?input <- (input $? north|n)
  (person (location ?l))
  (door (from ?l) (direction north) (status closed))
  =>
  (printout t crlf "*BONK*" crlf "This door is locked.")
  (retract ?input))
   
(defrule closed_door_west
  ?input <- (input $? west|w)
  (person (location ?l))
  (door (from ?l) (direction west) (status closed))
  =>
  (printout t crlf "*BONK*" crlf "This door is locked.")
  (retract ?input))
  
(defrule closed_door_east
  ?input <- (input $? east|e)
  (person (location ?l))
  (door (from ?l) (direction east) (status closed))
  =>
  (printout t crlf "*BONK*" crlf "This door is locked.")
  (retract ?input))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	Less specific default rule to handle the case where it is not possible to
;;  carry out players request for directional movement (i.e., a wall blocks his way).
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; not original

(defrule wall
  ?input <- (input $? n|north|e|east|w|west|s|south)
  =>
  (printout t crlf "*BONK*" crlf "You hit a wall.")
  (retract ?input))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	Allows the player to sleep only if he is in his Dorm room; increments number of
;;  moves and number of times he slept.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule to_sleep
  ?input <- (input $? sleep)
  ?person <- (person (location Dorm_Room) (Slept ?s) (Moves ?m))
  =>
  (printout t crlf "You've fallen asleep..." crlf)
  (printout t crlf crlf "...nnngnggughhhh..." crlf)
  (printout t crlf "...BEEP! BEEP! BEEP! BEEP!")
  (modify ?person (Slept (+ ?s 1)) (Moves (+ ?m 1)))
  (retract ?input))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	Prevents player from sleeping at any other position. This less-specific 
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule cant_sleep
  ?input <- (input $? sleep)
  =>
  (printout t crlf "You try to sleep, but there's too much noise in here.")
  (retract ?input))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	If the player asks for a certain objct, and if the objct happens to be located
;;  at the same position as the player, then the location field of the objct is changed
;;  to `player', i.e., the player is given the objct. Increments number of moves.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule get_specific_objct
  ?input <- (input get|grab|pickup|take|snatch|swipe|gain|pick $? ?o)
  ?person <- (person (location ?l) (Moves ?m))
  ?objct <- (objct (name ?o) (location ?l))
  =>
  (printout t crlf "You've picked up the " ?o ".")
  (modify ?objct (location player))
  (modify ?person (Moves (+ ?m 1)))
  (retract ?input))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The objct asked for by the player is not at the same location.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule cannot_get_specific_objct
  ?input <- (input get|grab|pickup|take|snatch|swipe|gain|pick $? ?o)
  =>
  (printout t crlf "There's no " ?o " here.")
  (retract ?input))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	Handles the command without arguments, by finding any objct present at that
;;  location. Icrement number of moves.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule get_any_objct
  ?input <- (input get|grab|pickup|take|snatch|swipe|gain|pick)
  ?person <- (person (location ?l) (Moves ?m))
  ?objct <- (objct (name ?n) (location ?l))
  =>
  (printout t crlf "You've picked up the " ?n ".")
  (modify ?objct (location player))
  (modify ?person (Moves (+ ?m 1)))
  (retract ?input))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	There is no objct to pick up in response to a get command without arguments.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule cannot_get_any_objct
  ?input <- (input get|grab|pickup|take|snatch|swipe|gain|pick)
  =>
  (printout t crlf "There's nothing here to pick up.")
  (retract ?input))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The following productions regenerate campus map, and Cafeteria foods
;;  when one is taken by the player.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule regenerate_map
  (person (location ~Orientation))
  (not (objct (name map) (location Orientation)))
  =>
  (assert (objct (name map) (location Orientation) (isa paper))))
  
(defrule regenerate_beet
  (person (location ~Cafeteria))
  (not (objct (name beet) (location Cafeteria)))
  =>
  (assert (objct (name beet) (location Cafeteria) (isa food))))
  
(defrule regenerate_cookie
  (person (location ~Cafeteria))
  (not (objct (name cookie) (location Cafeteria)))
  =>
  (assert (objct (name cookie) (location Cafeteria) (isa food))))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The player is allowed to eat an objct if he possesses that objct and the
;; objct belongs to a class (isa) of objcts that is edible. 
;; Increments number of moves and number of times he ate.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule eat_specific_objct
  ?input <- (input eat|nibble|gobble|ingest|swallow|feed|digest|absorb|dine
  					 |inhale|bite|graze|munch|chew $? ?o)
  ?objct <- (objct (name ?o) (location player) (isa ?type))
  (objct (name ?type) (edible? yes))
  ?person <- (person (Ate ?a) (Moves ?m))
  =>
  (printout t crlf "Hmmmmmmm, that's a good " ?o ".")
  (modify ?person (Ate (+ ?a 1)) (Moves (+ ?m 1)))
  (retract ?objct)  ;; make the food disapear
  (retract ?input))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The player is prevented from eating an objct which belongs to a class of 
;;  objects that is not edible.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule not_edible_objct
  ?input <- (input eat|nibble|gobble|ingest|swallow|feed|digest|absorb|dine
  					 |inhale|bite|graze|munch|chew $? ?o)
  (objct (name ?o) (location player))
  =>
  (printout t crlf "\"Oh wait! Don't eat that!!\"") 
  (printout t crlf "A friend whacks it out of your mouth.")
  (retract ?input))
  
  
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The player is prevented from eating an objct that the player does not possess.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original (not in skeleton)

(defrule dont_have_objct_to_eat
  ?input <- (input eat|nibble|gobble|ingest|swallow|feed|digest|absorb|dine
  					 |inhale|bite|graze|munch|chew $? ?o)
  (objct (name ?o) (location ~player))
  (not (objct (name ?o) (location player)))
  =>
  (printout t crlf "You don't have a " ?o ".")
  (retract ?input))
  

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	Handles the command eat without any argument. Checks to see if the player has
;;  anything in his possession that belongs to a class of objcts which are edible.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule eat_anything
  ?input <- (input eat|nibble|gobble|ingest|swallow|feed|digest|absorb|dine
  					 |inhale|bite|graze|munch|chew)
  ?objct <- (objct (name ?n) (location player) (isa ?type))
  (objct (name ?type) (edible? yes))
  ?person <- (person (Ate ?a) (Moves ?m))
  =>
  (printout t crlf "Hmmmmmmm, that's a good " ?n ".")
  (modify ?person (Ate (+ ?a 1)) (Moves (+ ?m 1)))
  (retract ?objct)  ;; make the food disapear
  (retract ?input))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The player has nothing that he can eat.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule cannot_eat_anything
  ?input <- (input eat|nibble|gobble|ingest|swallow|feed|digest|absorb|dine
  					 |inhale|bite|graze|munch|chew $?)
  (not (objct (location player) (isa food)))
  =>
  (printout t crlf "You don't have anything to eat.")
  (printout t crlf "~I wonder where the Cafeteria is.~")
  (retract ?input))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The player allowed to drop a specific object if he has that in his possession.
;;  Increments the number of moves.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule drop_specific_objct
  ?input <- (input drop|give|submit|release|boot|kick|throw|launch|eject|set|lay|toss
  					  |ditch|leave|shake|lob|hurl|punt|fling|chuck $? ?o)
  ?objct <- (objct (name ?o) (location player))
  ?person <- (person (location ?l) (Moves ?m))
  =>
  (printout t crlf "You tossed the " ?o ".")
  (modify ?person (Moves (+ ?m 1)))
  (modify ?objct (location ?l))  ;; drop the object
  (assert (just_dropped ?o))     ;; assert fact to aid when dropping the map
  (retract ?input))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The player cannot drop anything that he does not possess.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule cannot_drop_specific_objct
  ?input <- (input drop|give|submit|release|boot|kick|throw|launch|eject|set|lay|toss
  					  |ditch|leave|shake|lob|hurl|punt|fling|chuck $? ?o)
  =>
  (printout t crlf "You don't have any " ?o ".")
  (retract ?input))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	In response to a drop command without arguments, finds the first thing in
;;  possession of the player and fires the more specific rule with this objct to drop.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule drop_any_objct
  ?input <- (input drop|give|submit|release|boot|kick|throw|launch|eject|set|lay|toss
  					  |ditch|leave|shake|lob|hurl|punt|fling|chuck)
  ?objct <- (objct (name ?n) (location player))
  ?person <- (person (location ?l) (Moves ?m))
  =>
  (printout t crlf "You tossed the " ?n ".")
  (modify ?person (Moves (+ ?m 1)))
  (modify ?objct (location ?l))  ;; drop the object
  (assert (just_dropped ?n))     ;; assert fact to aid when dropping the map
  (retract ?input))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	There is nothing in possession of the player to drop.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule cannot_drop_any_objct
  ?input <- (input drop|give|submit|release|boot|kick|throw|launch|eject|set|lay|toss
  					  |ditch|leave|shake|lob|hurl|punt|fling|chuck)
  =>
  (printout t crlf "You don't have anything to drop.")
  (retract ?input))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The following productions create the different grade objcts when the proper
;;  grade (for prerequisite course) or the class_schedule is dropped in a classroom.
;;  Also, assert (class_taken) to be used when adding Credit to the player.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule create_1043_grade
  ?objct <- (objct (name schedule) (location 1043))
  =>
  (assert (objct (name 1043Grade) (location 1043) (isa paper)))
  (assert (class_taken))
  (retract ?objct))
  
(defrule create_2003_grade
  ?objct <- (objct (name schedule|1043Grade) (location 2003))
  =>
  (assert (objct (name 2003Grade) (location 2003) (isa paper)))
  (assert (class_taken))
  (retract ?objct))
  
(defrule create_3123_grade
  ?objct <- (objct (name schedule|2003Grade) (location 3123))
  =>
  (assert (objct (name 3123Grade) (location 3123) (isa paper)))
  (assert (class_taken))
  (retract ?objct))
  
(defrule create_4253_grade
  ?objct <- (objct (name schedule|3123Grade) (location 4253))
  =>
  (assert (objct (name 4253Grade) (location 4253) (isa paper)))
  (assert (class_taken))
  (retract ?objct))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The door to summer job is opened when the player gets the grade for any course
;;  and the door is not already open. The job interview door is opened after the player
;;  visits the Summer_Job location. The graduation door is opened after the player has
;;  picked up the 4253Grade.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule open_summer_job
  (objct (name 1043Grade|2003Grade|3123Grade|4253Grade) (location player))
  ?door <- (door (name sum_job) (status closed))
  =>
  (modify ?door (status open)))
  
(defrule open_job_interview
  (person (location Summer_Job))
  ?door <- (door (name job_int) (status closed))
  =>
  (modify ?door (status open)))
  
(defrule open_graduation
  (objct (name 4253Grade) (location player))
  ?door <- (door (name grad) (status closed))
  =>
  (modify ?door (status open)))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	Graduation door closed if the player does not have 4253 grade with him
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; not original

(defrule close_graduation
  ?door <- (door (direction north) (status open))
  (objct (name 4253Grade) (location ?x&~player))
  =>
  (modify ?door (status closed)))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The following rules assign credits to the player. If the first course taken by
;;  the player is 1043 he gets one credit, other courses taken first fetches him .5 
;;  credits. Any subsequent course taken fetches an additional 1 credit.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule give_first_class_credit_1
  (objct (name 1043Grade) (location player))
  ?person <- (person (Credits ?c))
  (not (first_class_credit))
  =>
  (modify ?person (Credits (+ ?c 1.0)))
  (assert (first_class_credit)))

(defrule give_first_class_credit_2
  (objct (name 2003Grade|3123Grade|4253Grade) (location player))
  ?person <- (person (Credits ?c))
  (not (first_class_credit))
  (mode (status run))       ;; add superfluous requirements to help fire
  ;;(objct (name dimploma))   ;; before give_more_class_credit
  =>
  (modify ?person (Credits (- ?c 0.5)))   ;; subract from what give_more_class_credit adds
  (assert (first_class_credit)))
  
(defrule give_more_class_credit
  (objct (name 2003Grade|3123Grade|4253Grade) (location player))
  ?person <- (person (Credits ?c))
  (first_class_credit)    ;; confirm that this is not the first credit earned
  (not (more_class_credit_oneshot))
  ?class_taken <- (class_taken)
  =>
  (modify ?person (Credits (+ ?c 1.0)))
  (assert (more_class_credit_oneshot))
  (retract ?class_taken))
  
(defrule remove_More_class_credit_oneshot
  ?more_class_credit <- (more_class_credit_oneshot)
  ?class_taken <- (class_taken)  ;; this fact is asserted when a new grade is created
  =>
  (retract ?more_class_credit))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The following productions check to see if the player has ate and slept at least
;;  once, and eaten or slept a total of four times. If not, 1 is subtracted from the 
;;  number of credits.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule slept_once?
  ?person <- (person (Credits ?c) (Slept 1))
  (not (first_sleep_credit))
  => 
  (modify ?person (Credits (+ ?c 1)))
  (assert (first_sleep_credit)))
  
(defrule ate_once?
  ?person <- (person (Credits ?c) (Ate 1))
  (not (first_meal_credit))
  => 
  (modify ?person (Credits (+ ?c 1)))
  (assert (first_meal_credit)))

(defrule ate_slept_constraint
  ?person <- (person (Credits ?c) (Ate ?a) (Slept ?s))
  (test (>= (+ ?a ?s) 4))
  (not (fourth_meal_or_sleep_credit))
  => 
  (modify ?person (Credits (+ ?c 1)))
  (assert (fourth_meal_or_sleep_credit)))
  
(defrule not_enough_sleep
  ?person <- (person (Credits ?c))
  (objct (name diploma) (location player))
  (not (first_sleep_credit))
  (not (sleep_removed))
  =>
  (modify ?person (Credits (- ?c 1)))
  (assert (sleep_removed)))

(defrule not_enough_eating
  ?person <- (person (Credits ?c))
  (objct (name diploma) (location player))
  (not (first_meal_credit))
  (not (meal_removed))
  =>
  (modify ?person (Credits (- ?c 1)))
  (assert (meal_removed)))
  
(defrule not_enough_sleep_and_eating
  ?person <- (person (Credits ?c))
  (objct (name diploma) (location player))
  (not (fourth_meal_or_sleep_credit))
  (not (forth_m_o_s_removed))
  =>
  (modify ?person (Credits (- ?c 1)))
  (assert (forth_m_o_s_removed)))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	Salary fixed according to the number of credits earned once player moves into 
;;  the permanent job interview room. The salary should range from 0 to 40 in thousands
;;  of dollars. The salary is calculated as the number of current credits multiplied by 
;;  10,000.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; original

(defrule assign_salary
  ?person <- (person (location Job_Interview) (Credits ?c))
  (test (<= ?c 4))
  (not (salary_assigned))
  =>
  (modify ?person (Salary =(* 10 ?c)))
  (assert (salary_assigned)))

(defrule assign_salary_over_40
  ?person <- (person (location Job_Interview) (Credits ?c))
  (not (salary_assigned))
  =>
  (modify ?person (Salary 40))
  (assert (salary_assigned)))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	Once the player gets diploma, the game is over. He is congratulated, and
;;  his credits, salary, number of moves and computed score is printed out.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; modified

(defrule the_end
  (person (Credits ?c) (Salary ?s) (Moves ?m))
  (objct (name diploma) (location player))
  =>
  (printout t crlf crlf "Congratulations on completing your college adventure at TU!")
  (printout t crlf "You have " ?c " credits.")
  (printout t crlf "Your salary will be $" (* ?s 1000))
  (printout t crlf "You took " ?m " moves.")
  (printout t crlf "Your final score is : " (/ (* ?c ?s) ?m) crlf crlf)
  (halt))
