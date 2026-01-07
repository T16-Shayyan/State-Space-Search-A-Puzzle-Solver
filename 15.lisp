;;; GENERAL-SEARCH      (general search function)
;;; GOAL-P              (goal testing predicate)
;;; BFS-ENQUEUER        (enqueues in breadth-first order)
;;; DFS-ENQUEUER        (enqueues in depth-first order)
;;; MANHATTAN-ENQUEUER  (enqueues by manhattan distance)
;;; NUM-OUT-ENQUEUER    (enqueues by number of tiles out of place)

;;; Accompanying this file are two other files: "utilities.lisp" and
;;; "queue.lisp".  utilities.lisp must be loaded first, then queue.lisp.
;;;  queue.lisp is what you need to take a look at: it's an implementation
;;; of three kinds of queues: LIFO stacks, FIFO queues, and priority queues.


;;; GeneralSearch(InitialState, GoalTest, EnqueueingFunction, MaxIterations)
;;;   make new empty queue
;;;   make new empty history
;;;   iterations <- 0
;;;   state <- InitialState
;;;
;;;   EnqueuingFunction(queue,state)
;;;   add the state's puzzle to history
;;;
;;;   loop:
;;;     iterations++
;;;     if (iterations > MaxIterations or queue is empty) return 'FAILED
;;;     state <- dequeue(queue)
;;;     if GoalTest(state)
;;;        print out number of iterations
;;;        return state
;;;     else for each child of the state
;;;        process child (as in Djikstra's)
;;;        if the child's puzzle is not in the history
;;;           EnqueuingFunction(queue,child [state])
;;;           add child [puzzle] to history



(defun make-initial-state (initial-puzzle-situation)
  "Makes an initial state with a given puzzle situation.
    The puzzle situation is simply a list of 9 numbers.  So to
    create an initial state with the puzzle
    2 7 4
    9 8 3
    1 5 6
    ...you would call (make-initial-state '(2 7 4 9 8 3 1 5 6))
    This has changed here to 16 so now you call somthing like (make-initial-state '(2 7 4 9 8 3 1 5 6 10 11 12 13 14 15 16))"
  (cons (concatenate 'simple-vector initial-puzzle-situation 
                     (list (position 16 initial-puzzle-situation))) nil))

(defmacro depth (state)
  "Returns the number of moves from the initial state 
    required to get to this STATE"
  `(1- (length ,state)))

(defmacro puzzle-from-state (state)
  "Returns the puzzle (an array of 10 integers) from STATE"
  `(first ,state))

(defmacro previous-state (state)
  "Returns the previous state that got us to this STATE"
  `(rest ,state))

(defmacro empty-slot (puzzle)
  "Returns the position of the empty slot in PUZZLE"
  `(elt ,puzzle 16))

(defmacro build-state (puzzle previous-state)
  "Builds a state from a new puzzle situation and a previous state"
  `(cons ,puzzle ,previous-state))

(defparameter *valid-moves* 
  #((1 4) (0 2 5) (1 3 6) (2 7) (0 5 8) (1 4 6 9) (2 5 7 10) (3 6 11) (4 9 12) (5 8 13 10) (6 9 14 11) (7 10 15) (8 13) (9 12 14) (13 10 15) (11 14))
  "A vector, for each empty slot position, of all the valid moves that can be made.
    The moves are arranged in lists.")

(defun swap (pos1 pos2 puzzle)
  "Returns a new puzzle with POS1 and POS2 swapped in original PUZZLE.  If
    POS1 or POS2 is empty, slot 16 is updated appropriately."
  (let ((tpos (elt puzzle pos1)) (puz (copy-seq puzzle)))
    (setf (elt puz pos1) (elt puz pos2))  ;; move pos2 into pos1's spot
    (setf (elt puz pos2) tpos)  ;; move pos1 into pos2's spot
    (if (= (elt puz pos1) 16) (setf (empty-slot puz) pos1)  ;; update if pos1 is 16
        (if (= (elt puz pos2) 16) (setf (empty-slot puz) pos2)))  ;; update if pos2 is 16
    puz))

(defun make-move (move puzzle)
  "Returns a new puzzle from original PUZZLE with a given MOVE made on it.
    If the move is illegal, nil is returned.  Note that this is a PUZZLE,
    NOT A STATE.  You'll need to build a state from it if you want to."
  (let ((moves (elt *valid-moves* (empty-slot puzzle))))
    (when (find move moves) (swap move (empty-slot puzzle) puzzle))))



(defun create-random-state (num-moves)
  "Generates a random state by starting with the
    canonical correct puzzle and making NUM-MOVES random moves.
    Since these are random moves, it could well undo previous
    moves, so the 'randomness' of the puzzle is <= num-moves"
  (let ((puzzle #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 15)))
    (dotimes (x num-moves)
      (let ((moves (elt *valid-moves* (empty-slot puzzle))))
        (setf puzzle (make-move (elt moves (random (length moves))) puzzle))))
    (build-state puzzle nil)))





(defmacro foreach-valid-move ((move puzzle) &rest body)
  "Iterates over each valid move in PUZZLE, setting
    MOVE to that move, then executing BODY.  Implicitly
    declares MOVE in a let, so you don't have to."
  `(dolist (,move (elt *valid-moves* (empty-slot ,puzzle)))
     ,@body))





(defmacro foreach-position ((pos puzzle) &rest body)
  "Iterates over each position in PUZZLE, setting POS to the
    tile number at that position, then executing BODY. Implicitly
    declares POS in a let, so you don't have to."
  (let ((x (gensym)))
    `(let (,pos) (dotimes (,x 16) (setf ,pos (elt ,puzzle ,x))
                   ,@body))))

(defun print-puzzle (puzzle)
  "Prints a puzzle in a pleasing fashion.  Returns the puzzle."
  (let (lis)
    (foreach-position (pos puzzle)
                      (if (= pos 16) (push #\space lis) (push pos lis)))
    (apply #'format t "~%~A~A~A~A~%~A~A~A~A~%~A~A~A~A~%~A~A~A~A" (reverse lis)))
  puzzle)

(defun print-solution (goal-state)
  "Starting with the initial state and ending up with GOAL-STATE,
    prints a series of puzzle positions showing how to get 
    from one state to the other.  If goal-state is 'FAILED then
    simply prints out a failure message"
  ;; first let's define a recursive printer function
  (labels ((print-solution-h (state)
             (print-puzzle (puzzle-from-state state)) (terpri)
             (when (previous-state state) (print-solution-h (previous-state state)))))
    ;; now let's reverse our state list and call it on that
    (if (equalp goal-state 'failed) 
        (format t "~%Failed to find a solution")
        (progn
          (format t "~%Solution requires ~A moves:" (1- (length goal-state)))
          (print-solution-h (reverse goal-state))))))




(defun goal-p (state)
  "Returns T if state is a goal state, else NIL.  Our goal test."

  
  (equalp (puzzle-from-state state) #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 15)))

(defun dfs-enqueuer (state queue)
  "Enqueues in depth-first order"

  
  ;;;stack LIFO
  (enqueue-at-front queue state))

(defun bfs-enqueuer (state queue)
  "Enqueues in breadth-first order"

  
  ;;;queue FIFO
  (enqueue-at-end queue state))                                         

(defun manhattan-enqueuer (state queue)
  "Enqueues by manhattan distance"

  
  (enqueue-by-priority queue
                       (lambda (state)
                         (let ((sum 0))
                           (dotimes (i 16 sum)
                             (let ((tile (elt (puzzle-from-state state) i)))
                               (if (not (= tile 16))
                                   (setf sum (+ sum (+ (abs (- (floor i 4) (floor (- tile 1) 4)))
                                                       (abs (- (mod i 4) (mod (- tile 1) 4)))))))))
                           (+ (depth state) sum)))
                       state))

(defun num-out-enqueuer (state queue)
  "Enqueues by number of tiles out of place"

  
  (enqueue-by-priority queue
                       (lambda (state)
                         (let ((count 0))
                           (dotimes (i 16 count)
                             (if (and (not (= (elt (puzzle-from-state state) i) 16))
                                      (not (= (elt (puzzle-from-state state) i) (1+ i))))
                                 (incf count)))
                           (+ (depth state) count)))
                       state))



(defun general-search (initial-state goal-test enqueueing-function &optional (maximum-iterations nil))
  "Starting at INITIAL-STATE, searches for a state which passes the GOAL-TEST
    function.  Uses a priority queue and a history list of previously-visited puzzles.
    Enqueueing in the queue is done by the provided ENQUEUEING-FUNCTION.  Prints 
    out the number of iterations required to discover the goal state.  Returns the 
    discovered goal state, else returns the symbol 'FAILED if the entire search 
    space was searched and no goal state was found, or if MAXIMUM-ITERATIONS is 
    exceeded.  If maximum-iterations is set to nil, then there is no maximum number
    of iterations."

  

  (let* ((queue (make-empty-queue))
         (history (make-hash-table :test #'equalp))
         (iterations 0)
         (state initial-state))
    (funcall enqueueing-function state queue)
    (setf (gethash (puzzle-from-state state) history) 'VISITED)
    (loop
      (incf iterations)
      (if (or (and maximum-iterations (> iterations maximum-iterations))
              (empty-queue? queue))
          (return 'FAILED))
      (setf state (remove-front queue))
      (if (funcall goal-test state)
          (progn (format t "~%Reached goal in ~a iterations" iterations)
                 (return state))
          (foreach-valid-move (move (puzzle-from-state state))
                              (let ((child-puzzle (make-move move (puzzle-from-state state))))
                                (if (and child-puzzle (not (gethash child-puzzle history)))
                                    (let ((child-state (build-state child-puzzle state)))
                                      (funcall enqueueing-function child-state queue)
                                      (setf (gethash child-puzzle history) 'VISITED)))))))))






;;; The five test examples.
(defparameter s nil)

#|
;;; Solves in 4 moves:
(setf s (create-random-state 4))
(print-puzzle (puzzle-from-state s))
;;;1  2  3  4
;;;5  6  7  8
;;;9  10    11
;;;13 14 15 12
(print-solution (general-search s #'goal-p #'bfs-enqueuer 200000))
;;;Reached goal in 15 iterations
;;;Solution requires 2 moves:
(print-solution (general-search s #'goal-p #'dfs-enqueuer 200000))
;;;Reached goal in 3 iterations
;;;Solution requires 2 moves:
(print-solution (general-search s #'goal-p #'num-out-enqueuer 200000))
;;;Reached goal in 3 iterations
;;;Solution requires 2 moves:
(print-solution (general-search s #'goal-p #'manhattan-enqueuer 200000))
;;;Reached goal in 3 iterations
Solution requires 2 moves:
1234
5678
910 11
13141512

1234
5678
91011
13141512

1234
5678
9101112
131415
|#

#|
;;; Solves in 8 moves:
(setf s (create-random-state 8))
(print-puzzle (puzzle-from-state s))
;;;1  2 3  4
;;;5  6  7  8
;;;   9  10 11
;;;13 14 15 12
(print-solution (general-search s #'goal-p #'bfs-enqueuer 200000))
;;;Reached goal in 51 iterations
;;;Solution requires 4 moves:
(general-search s #'goal-p #'dfs-enqueuer 200000)
;;;Reached goal in 14357 iterations
;;;taking too lon so didnt print the solutions
(print-solution (general-search s #'goal-p #'num-out-enqueuer 200000))
;;;Reached goal in 5 iterations
;;;Solution requires 4 moves:
(print-solution (general-search s #'goal-p #'manhattan-enqueuer 200000))
Reached goal in 5 iterations
Solution requires 4 moves:
1234
5678
 91011
13141512

1234
5678
9 1011
13141512

1234
5678
910 11
13141512

1234
5678
91011
13141512

1234
5678
9101112
131415
|#


#|
;;; Solves in 5 moves:
(setf s (create-random-state 5))
(print-puzzle (puzzle-from-state s))
;;;1  2  3  4
;;;5  6     8
;;;9  10 7  11
;;;13 14 15 12
(print-solution (general-search s #'goal-p #'bfs-enqueuer 200000))
;;;Reached goal in 35 iterations
;;;Solution requires 3 moves:
(general-search s #'goal-p #'dfs-enqueuer 200000)
;;;Reached goal in 4 iterations
(print-solution (general-search s #'goal-p #'num-out-enqueuer 200000))
;;;Reached goal in 4 iterations
;;;Solution requires 3 moves:
(print-solution (general-search s #'goal-p #'manhattan-enqueuer 200000))
Reached goal in 4 iterations
Solution requires 3 moves:
1234
56 8
910711
13141512

1234
5678
910 11
13141512

1234
5678
91011
13141512

1234
5678
9101112
131415
|#



#|
;;; Solves in 9 moves:
(setf s (create-random-state 9))
(print-puzzle (puzzle-from-state s))
;;;1  2  3  4
;;;5  6  7  8
;;;13 9  10 11
;;;   14 15 12
(print-solution (general-search s #'goal-p #'bfs-enqueuer 200000))
;;;Reached goal in 68 iterations
;;;Solution requires 5 moves:
(general-search s #'goal-p #'dfs-enqueuer 200000)
;;;Reached goal in 14356 iterations
(print-solution (general-search s #'goal-p #'num-out-enqueuer 200000))
;;;Reached goal in 6 iterations
;;;Solution requires 5 moves:
(print-solution (general-search s #'goal-p #'manhattan-enqueuer 200000))
Reached goal in 6 iterations
Solution requires 5 moves:
1234
5678
1391011
 141512

1234
5678
 91011
13141512

1234
5678
9 1011
13141512

1234
5678
910 11
13141512

1234
5678
91011
13141512

1234
5678
9101112
131415
|#


#|
;;; Solves in 9 moves:
(setf s (create-random-state 20))
(print-puzzle (puzzle-from-state s))
;;;1  2  3  4
;;;5     6  8
;;;13 10 7  11
;;;14 9  15 12
(print-solution (general-search s #'goal-p #'bfs-enqueuer 200000))
;;;Reached goal in 4868 iterations
;;;Solution requires 10 moves:
(print-solution (general-search s #'goal-p #'dfs-enqueuer 200000))
;;;Failed to find a solution
(print-solution (general-search s #'goal-p #'num-out-enqueuer 200000))
;;;Reached goal in 21 iterations
;;;Solution requires 10 moves:
(print-solution (general-search s #'goal-p #'manhattan-enqueuer 200000))
Reached goal in 16 iterations
Solution requires 10 moves:
1234
5 68
1310711
1491512

1234
51068
13 711
1491512

1234
51068
139711
14 1512

1234
51068
139711
 141512

1234
51068
 9711
13141512

1234
51068
9 711
13141512

1234
5 68
910711
13141512

1234
56 8
910711
13141512

1234
5678
910 11
13141512

1234
5678
91011
13141512

1234
5678
9101112
131415
|#
