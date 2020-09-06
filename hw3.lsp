(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
; SOLUTION:
;	We can assume we are always given a valid state with a keeper. Thus, we simply check if there are any boxes or a keeper in the state.
(defun goal-test (s)
  (cond ((null s) t)
  		((atom s) (not (or (isKeeper s) (isBox s)))) ; if s is not a wall, blank, boxstar, or keeper star, return false
		(t (and (goal-test (first s)) (goal-test (rest s)))))
  );end defun

; get-col (row c)
; returns integer in column c of row
;
; SOLUTION:
;	Iterate through the row recursively.
;	Base case is if c = 0, which means we just take the first element of the row.
;	Otherwise, we look at (rest row) while decrementing c by 1.
;	If c is out of bounds, then we return wall immediately
(defun get-col (row c)
  (cond ((= c 0) (first row))
		((or (< c 0) (>= c (length row))) wall)
		(t (get-col (rest row) (- c 1)))))

; get-square (s r c)
; returns integer content of state s at square (r,c)
;
; SOLUTION:
;	Given s, we iterate through each element which corresponds to a row and pass each row to get-col
;	The base case is if r = 0, we return the element at the first row i.e. (first s) and column c.
;	Otherwise, we iterate to the next row (rest row) while decrementing r by 1.
;	If r is out of bounds, then we return wall immediately
(defun get-square (s r c)
  (cond ((null s) nil)
		((= r 0) (get-col (first s) c))
		((or (< r 0) (>= r (length s))) wall)
		(t (get-square (rest s) (- r 1) c))))

; set-col (row c v)
; returns a list representing a row, setting element in column c of row
; if c is out of bounds of row, return NIL
;
; SOLUTION:
;	The base case is if c = 0, then we replace the first element of the row and keep the rest the same.
;	Otherwise, we keep the first element the same and combine it with (rest row) with element c - 1 replaced.
;	If column is out of bounds, we return nil
(defun set-col (row c v)
  (cond ((= c 0) (cons v (rest row)))
  		;((or (< c 0) (>= c (length row))) nil)
		(t (cons (first row) (set-col (rest row) (- c 1) v)))))

; set-square (s r c v)
; returns a state where cell in row r, column c is set to v
;
; SOLUTION:
;	We first find the desired row by recursively iterating through s.
;	The base case is if r = 0, which means we want the first row and can set-col on column c
;	Otherwise, we combine the unchanged first row to (rest s) where row r-1 and column c is changed
(defun set-square (s r c v)
  (cond ((= r 0) (cons (set-col (first s) c v) (rest s)))
		(t (cons (first s) (set-square (rest s) (- r 1) c v)))))

; try-move (s dir)
; Returns the state if keeper (whose position is given by (x, y)) tried to move in a direction (dir).
; The keeper may be moving from a star...
; 	flag = 1, if keeper is moving from star square
;	flag = 0, if keeper is moving from blank square
; dir: 1 - left, 2 - down, 3 - right, 4 - up
;
; SOLUTION:
;	Given a direction dir, we first check the square one space in that direction
;	(1) If it is BLANK, we set that square to keeper, and set our original spot appropriately
;	(2) If it is a STAR, we set that square to keeperstar, and set our original spot appropriately
;	(3) If it is a BOX, check if square two spaces in dir is blank or star, and change as described above
;	(4) If it is a BOXSTAR, check if square two spaces in dir is blank or star, and change as described above
;	(5) Otherwise, we cannot move to that space, so we return nil
(defun try-move (s r c dir)
  (let ((flag (cond ((isKeeperStar (get-square s r c)) 1)
  					(t 0))))
	  (cond ((= dir 1) (let ((square-l (get-square s r (- c 1))))
						(cond ((isBlank square-l) (set-square (set-square s r (- c 1) keeper) r c (* flag star))) ; if cell left is blank ==> set that cell to keeper
							  ((isStar square-l) (set-square (set-square s r (- c 1) keeperstar) r c (* flag star))) ; if cell left is a star ==> set that cell to keeperstar
							  ((isBox square-l) (let ((square-l2 (get-square s r (- c 2))))
							  					  (cond ((isBlank square-l2) (set-square (set-square (set-square s r (- c 2) box) r (- c 1) keeper) r c (* flag star)))
													  	((isStar square-l2) (set-square (set-square (set-square s r (- c 2) boxstar) r (- c 1) keeper) r c (* flag star)))
													  	(t nil)))) ; if cell left is a box ==> check if cell left of box is blank/goal and move accordingly
							  ((isBoxStar square-l) (let ((square-l2 (get-square s r (- c 2))))
							  						  (cond ((isBlank square-l2) (set-square (set-square (set-square s r (- c 2) box) r (- c 1) keeperstar) r c (* flag star)))
													  	  	((isStar square-l2) (set-square (set-square (set-square s r (- c 2) boxstar) r (- c 1) keeperstar) r c (* flag star)))
													  	  	(t nil))))
							  (t nil))))
			((= dir 2) (let ((square-d (get-square s (+ r 1) c)))
						(cond ((isBlank square-d) (set-square (set-square s (+ r 1) c keeper) r c (* flag star))) ; if cell to right is blank ==> set that cell to keeper
							  ((isStar square-d) (set-square (set-square s (+ r 1) c keeperstar) r c (* flag star))) ; if cell to right is a star ==> set that cell to keeperstar
							  ((isBox square-d) (let ((square-d2 (get-square s (+ r 2) c)))
							  					  (cond ((isBlank square-d2) (set-square (set-square (set-square s (+ r 2) c box) (+ r 1) c keeper) r c (* flag star)))
													  	((isStar square-d2) (set-square (set-square (set-square s (+ r 2) c boxstar) (+ r 1) c keeper) r c (* flag star)))
													  	(t nil)))) ; if cell to right is a box ==> check if cell to right of box is blank/goal and move accordingly
							  ((isBoxStar square-d) (let ((square-d2 (get-square s (+ r 2) c)))
							  						  (cond ((isBlank square-d2) (set-square (set-square (set-square s (+ r 2) c box) (+ r 1) c keeperstar) r c (* flag star)))
													  	  	((isStar square-d2) (set-square (set-square (set-square s (+ r 2) c boxstar) (+ r 1) c keeperstar) r c (* flag star)))
													  	  	(t nil))))
							  (t nil))))
			((= dir 3) (let ((square-r (get-square s r (+ c 1))))
						(cond ((isBlank square-r) (set-square (set-square s r (+ c 1) keeper) r c (* flag star))) ; if cell below is blank ==> set that cell to keeper
							  ((isStar square-r) (set-square (set-square s r (+ c 1) keeperstar) r c (* flag star))) ; if cell below is a star ==> set that cell to keeperstar
							  ((isBox square-r) (let ((square-r2 (get-square s r (+ c 2))))
							  					  (cond ((isBlank square-r2) (set-square (set-square (set-square s r (+ c 2) box) r (+ c 1) keeper) r c (* flag star)))
													  	((isStar square-r2) (set-square (set-square (set-square s r (+ c 2) boxstar) r (+ c 1) keeper) r c (* flag star)))
													  	(t nil)))) ; if cell below is a box ==> check if cell below box is blank/goal and move accordingly
							  ((isBoxStar square-r) (let ((square-r2 (get-square s r (+ c 2))))
							  						  (cond ((isBlank square-r2) (set-square (set-square (set-square s r (+ c 2) box) r (+ c 1) keeperstar) r c (* flag star)))
													  	  	((isStar square-r2) (set-square (set-square (set-square s r (+ c 2) boxstar) r (+ c 1) keeperstar) r c (* flag star)))
													  	  	(t nil))))
							  (t nil))))
			((= dir 4) (let ((square-u (get-square s (- r 1) c)))
						(cond ((isBlank square-u) (set-square (set-square s (- r 1) c keeper) r c (* flag star))) ; if cell to left is blank ==> set that cell to keeper
							  ((isStar square-u) (set-square (set-square s (- r 1) c keeperstar) r c (* flag star))) ; if cell to left is a star ==> set that cell to keeperstar
							  ((isBox square-u) (let ((square-u2 (get-square s (- r 2) c)))
							  					  (cond ((isBlank square-u2) (set-square (set-square (set-square s (- r 2) c box) (- r 1) c keeper) r c (* flag star)))
													  	((isStar square-u2) (set-square (set-square (set-square s (- r 2) c boxstar) (- r 1) c keeper) r c (* flag star)))
													  	(t nil)))) ; if cell to left is a box ==> check if cell to left of box is blank/goal and move accordingly
							  ((isBoxStar square-u) (let ((square-u2 (get-square s (- r 2) c)))
							  						  (cond ((isBlank square-u2) (set-square (set-square (set-square s (- r 2) c box) (- r 1) c keeperstar) r c (* flag star)))
													  	  	((isStar square-u2) (set-square (set-square (set-square s (- r 2) c boxstar) (- r 1) c keeperstar) r c (* flag star)))
													  	  	(t nil))))
							  (t nil))))
			(t nil))))

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
;
; SOLUTION:
;	As suggested, I simply made result the combined list of try-move in all 4 directions
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (c (car pos))
	 (r (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s r c 4) (try-move s r c 2) (try-move s r c 1) (try-move s r c 3)))
	 )
    (cleanUpList result);end
   );end let
  );

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
; SOLUTION:
;	return 0
(defun h0 (s)
  0)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; SOLUTION:
;	Base case is if s is null, then there are no misplaced boxes
;	Otherwise, we count how many boxes are in the first row and add it to h1(rest s)
;
; Is h1 admissible?
; 	Yes, h1 is admissible because we only add 1 for each box not in a goal/star.
; 	The minimum cost to get a box to a goal is 1, so we know it will never overestimate the actual cost.
(defun h1 (s)
  (cond ((null s) 0)
  		(t (+ (count 2 (first s)) (h1 (rest s))))))

; check-box (s r c stars)
; Given a box (r, c), returns T if box is in a corner, NIL otherwise
; Assume (r, c) is always coordinate of a box since we only call it in our other functions knowing it is a box
;
; SOLUTION:
;	Assign flags for if there is a wall on each side of the box
;	We can determine if it's trapped if there are walls...
;		- above and right
;		- above and left
;		- below and right
;		- below and left
(defun check-box (s r c)
  (let ((wall-above (isWall (get-square s (- r 1) c)))
		(wall-below (isWall (get-square s (+ r 1) c)))
		(wall-left  (isWall (get-square s r (- c 1))))
		(wall-right (isWall (get-square s r (+ c 1)))))
		(or (and wall-above wall-left) (and wall-above wall-right) (and wall-below wall-left) (and wall-below wall-right))))

; cornered-boxes (s boxes)
; given a list of (r, c) coordinates representing boxes' position (boxes), return true if there is a box that is cornered in state s
;
; SOLUTION:
;	The base case if there are no boxes, then we return nil
;	Otherwise, we check the first box, and recursively call the function on the rest of the boxes
(defun cornered-boxes (s boxes)
  (cond ((null boxes) nil)
		(t (let ((first-box (first boxes)))
			  (or (check-box s (first first-box) (second first-box)) (cornered-boxes s (rest boxes)))))))

; check-boxpair
; Given state s, and two boxes (r1, c1) and (r2, c2), return true if they are adjacent and both aside a wall
;
; SOLUTION:
; 	First, check if the manhattan distance between the boxes is 1. If not, return, as they are either not adjacent or the same box
; 	Then, we check which axis the boxes are adjacent on.
; 	If they are horizontally adjacent, check if they either have boxes above both or below both.
;	Similarly for vertically adjacent, check if they either have boxes left or right of both
(defun check-boxpair (s r1 c1 r2 c2)
  (cond ((not (= 1 (manh-dist r1 c1 r2 c2))) nil) ; if the boxes are not adjacent, return nil
  		((= r1 r2) (or (and (isWall (get-square s (+ r1 1) c1)) (isWall (get-square s (+ r2 1) c2))) 
  					   (and (isWall (get-square s (- r1 1) c1)) (isWall (get-square s (- r2 1) c2)))))
		((= c1 c2) (or (and (isWall (get-square s r1 (+ c1 1))) (isWall (get-square s r2 (+ c2 1))))
					   (and (isWall (get-square s r1 (- c1 1))) (isWall (get-square s r2 (- c2 1))))))))

; check-adj-box (s box boxes)
; Given state s, box (r, c), and list of coordinates (boxes),
; return true if box and an element in boxes are adjacent and both aside a wall.
;
; SOLUTION:
;	The base case is that if boxes is empty, return NIL
;	Otherwise, we check adjacency using check-boxpair with box and the first element in boxes
;	and check adjacency of box with the rest of boxes.
(defun check-adj-box (s box boxes)
  (cond ((null boxes) nil)
		(t (let ((first-box (first boxes)))
			  (or (check-boxpair s (first box) (second box) (first first-box) (second first-box)) 
				  (check-adj-box s box (rest boxes)))))))

; walled-adj-boxes (s boxes)
; Given state (s) and list of box coordinates (boxes),
; return true if there are any two boxes that are adjacent and along a wall together.
;
; SOLUTION:
;	This function determines if s is in an unsolvable state based on two walls being adjacent and have a wall on the same side.
;	The base case is if there are no boxes, return NIL
;	We can iterate through boxes, but boxes is sorted as if you are traveling left to right, row-by-row in state.
;	Therefore, when we are looking at a box, we only need to compare it to the rest of the boxes after it.
;	We check the first box in boxes with the rest, and then recursively travel down boxes by calling walled-adj-boxes on (rest boxes)
(defun walled-adj-boxes (s boxes)
  (cond ((null boxes) nil)
		(t (or (check-adj-box s (first boxes) (rest boxes)) (walled-adj-boxes s (rest boxes))))))

; manh-dist (r1 c1 r2 c2)
; returns the Manhattan distance from two given squares (r1, c1) and (r2, c2)
;
; SOLUTION:
;	The Manhattan distance is |r1 - r2| + |c1 - c2| since we can only move in discrete up/down/left/right steps.
(defun manh-dist (r1 c1 r2 c2)
  (+ (abs (- r1 r2)) (abs (- c1 c2))))

; get-row-boxes (row r c)
; returns list of coordinates (r, c) of all boxes in row
; r and c are used to keep track of where we are in s
; c will be initialized as 0 in the calling function
;
; SOLUTION:
;	Base cases are if row is null, then we return NIL, and if row is a number and a box, we return its coordinates
;	Otherwise, we iterate through the row by recursively calling it on first and rest.
(defun get-row-boxes (row r c)
  (cond ((null row) nil)
  		((atom row) (cond ((isBox row) (list (list r c)))
  						  (t nil)))
		(t (append (get-row-boxes (first row) r c) (get-row-boxes (rest row) r (+ 1 c))))))

; boxes-list (s)
; returns list of coordinates (r, c) of all boxes in a state s
;
; SOLUTION:
;	Base case is if s is null, we return NIL
;	Otherwise, we get the boxes in the first row and append it with the box-list of the rest of s
;	i.e. we iterate through each row and combine all the box coordinates from each row
(defun get-box-list (s r)
  (cond ((null s) nil)
  		(t (append (get-row-boxes (first s) r 0) (get-box-list (rest s) (+ 1 r))))))

; row-stars (row r c)
; returns list of coordinates (r, c) of all stars in row
; r and c are used to keep track of where we are in s
;
; SOLUTION:
;	Base cases are if row is null, then we return NIL, and if row is a number and a star, we return its coordinates
;	Otherwise, we iterate through the row by recursively calling it on first and rest.
(defun get-row-stars (row r c)
  (cond ((null row) nil)
  		((atom row) (cond ((isStar row) (list (list r c)))
  						  (t nil)))
		(t (append (get-row-stars (first row) r c) (get-row-stars (rest row) r (+ 1 c))))))

; stars-list (s)
; returns list of coordinates (r, c) of all stars in a state s
;
; SOLUTION:
;	Base case is if s is null, we return NIL
;	Otherwise, we get the stars in the first row and append it with the stars-list of the rest of s
;	i.e. we iterate through each row and combine all the star coordinates from each row
(defun get-stars-list (s r)
  (cond ((null s) nil)
  		(t (append (get-row-stars (first s) r 0) (get-stars-list (rest s) (+ 1 r))))))

; manh-dist-list (r c stars)
; returns a list of Manhattan distances from (r, c) to each star coordinate in stars
; stars is a list of star coordinates e.g. '((1 2) (3 3) (2 5))
;
; SOLUTION:
;	Given (r, c), we iterate and check it against each star coordinate in stars.
;	Base case is if stars is null, return NIL
;	Otherwise, append the manhattan distance of (r,c) with the first star and the manh-dist-list against rest of the stars
(defun manh-dist-list (r c stars)
  (cond ((null stars) nil)
		(t (let ((first-star (first stars))) 
			  (cons (manh-dist r c (first first-star) (second first-star)) (manh-dist-list r c (rest stars)))))))

; min-list (l)
; returns minimum element in list l or 0 if l is NIL
;
; SOLUTION
;	The base case is if l is empty, return 0 as we assume l is a list of manhattan distances. If there are no relevant manhattan distances the heuristic should be 0
;	Also, if l has one element we return that element.
;	Otherwise we return the minimum between the first element of l and the minimum of rest l
(defun min-list (l)
  (cond ((null l) 0)
  		((= 1 (length l)) (first l))
		(t (min (first l) (min-list (rest l))))))

; manh-heuristic (boxes stars)
; Given a list of boxes and stars, we return the summation of the manhattan distance to the closest star for each box.
;
; SOLUTION
;	If boxes is null, we return 0 cost
;	Otherwise we return the closest manhattan distance of the first box added to the manh-heuristic of the rest boxes.
(defun manh-heuristic (boxes stars)
  (cond ((null boxes) 0)
		(t (let ((first-box (first boxes)))
			  (+ (min-list (manh-dist-list (first first-box) (second first-box) stars)) (manh-heuristic (rest boxes) stars))))))

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
; This heuristic checks if there are cornered boxes (unsolvable state), and if so, we give it an extremely high weight to remove it from consideration.
; Also, we check if there are two boxes that are adjacent and against a wall on the same side, which is also unsolvable and thus gets a high weight that removes it from consideration.
; Otherwise, for every box and keeper, we return the summation of the manhattan distance between them and the closest star
;
; The heuristic is admissible as for the unsolvable cases we can't "overestimate" since the state itself is unsolvable which technically has infinite cost.
; Thus, giving it a flat weight of 1000 will work for our purposes. Otherwise, the manhattan distance between a box/keeper and a star is the minimum cost
; to get to the goal state as we need to travel that distance to land on the goal. The calculation doesn't overestimate because
; it takes the smallest manhattan distance for each box/keeper, and the distances are independent (multiple boxes could be referencing the same star).
; Therefore, our calculation will give a lower bound on the cost to get to the goal state.
(defun h204966812 (s)
  (let ((box-list (get-box-list s 0)))
	(cond ((cornered-boxes s box-list) 1000)
		  ((walled-adj-boxes s box-list) 1000)
		  (t (let ((star-list (get-stars-list s 0))
		  		   (kpos (getKeeperPosition s 0)))
		  		(+ (min-list (manh-dist-list (second kpos) (first kpos) star-list)) (manh-heuristic box-list star-list)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
