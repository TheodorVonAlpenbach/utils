(defconst *segments*
  '((A B 6)
    (D A 6)
    (A D 6)
    (A F 1)
    (B C 1)
    (B D 5)
    (C D 5)
    (C E 1)
    (D E 1)
    (E F 1))
  "See network.xls")

;;Node structure: (A 134217727 (#B 4) (#D 6) (#F 2))

(defun read-segments (filename)
  (setq *segments* (africa-read-csv filename)))
;;(read-segments "c:/cygwin/usr/libs/emacs-21.3/site-lisp/mb-lisp/topology/afrika.csv")

(defun extract-nodes (segments)
  (remove-duplicates 
   (loop for segment in segments
	 collect (first segment)
	 collect (second segment))))
;;(extract-nodes *segments*)

(defun node-symbol (node)
  (first node))
;;(symbolp (node-symbol (first *network*)))

(defun node-string (node)
  (symbol-name (node-symbol)))
;;(symbolp (node-symbol (first *network*)))

(defun get-node (node-symbol network)
  (find node-symbol network :key #'first))

(defun set-neighbor-nodes (node segments network)
  (loop with node-symbol = (first node)
	for segment in segments
	for res = (remove node-symbol segment)
	if (= 2 (length res))
	collect (list (get-node (first res) network)
		      (third segment))))

(defun* init-network (segments &optional (init-distance most-positive-fixnum))
  (let ((network (mapcar #'(lambda (x) 
				  (list x init-distance nil ()))
			      (extract-nodes segments))))

    network))

(defun* fill-network (network segments &optional (init-distance most-positive-fixnum))
  (loop for node in network
	do (setf (fourth node)
		 (set-neighbor-nodes node segments network))))

(defun* reset-network (network &optional (init-distance most-positive-fixnum))
  (loop for node in network
	do (setf (second node) init-distance
		 (third node) nil)))

;;NBNBNBNBNB!!! Do not evaluete a filled node network, this will exhaust memory!
;;(setq qwe (africa-read-csv))
;;(setq *network* (init-network qwe))
;;(fill-network *network* qwe)
;;(print-network *network*)

(defun print-node (node)
  (list (first node) 
	(second node)
	(first (third node)) ;only node-symbol
	(loop for node-segment in (fourth node)
	      for node-neighbor = (first node-segment) collect
	      (list (first node-neighbor) (second node-segment)))))

(defun print-network (network)
  (loop for node in network collect
	(print-node node)))
;;(print-network *network*)

(defun dijkstra-1 (node-queue network)
  "Fills out distances from NODE to all other nodes in NETWORK. Result
is returned as a node network"
  (while node-queue
    (setq node-queue (sort* node-queue #'< :key #'second))
    (let ((node (pop node-queue)))
      (loop for node-segment in (fourth node)
	    for node-neighbor = (first node-segment)
	    for new-distance = (+ (second node) (second node-segment))
	    do (when (< new-distance (second node-neighbor))
		 (setf (second node-neighbor) new-distance)
		 (setf (third node-neighbor) node)
		 (pushnew node-neighbor node-queue))))))

(defun dijkstra (node-symbol network)
  "Fills out distances from NODE to all other nodes in SEGMENTS. Result
is returned as a node segments"
  (let* ((node (get-node node-symbol network)))    
    (reset-network network)
    (setf (second node) 0)
    (dijkstra-1 (list node) network)
    network))
;;(mapcar #'butlast (dijkstra 'dz *network*))

(defun dijkstra-path (a b network &optional dijkstra-run-p)
  (unless dijkstra-run-p (dijkstra a network))
  (loop for node = (get-node b network) then (third node)
	while node collect (first node)))
;;(dijkstra-path 'za 'gm *network*)

(defun dijkstra-distance (a b network &optional dijkstra-run-p)
  (unless dijkstra-run-p (dijkstra a network))
  (1- (length (dijkstra-path a b network dijkstra-run-p))))
;;(dijkstra-distance 'ls 'gm *network*)

(defun* dijkstra-vector (network &optional (node-a (first network)))
  (let ((a (node-symbol node-a)))
    (dijkstra a network)
    (loop for node-b in network
	  for b = (node-symbol node-b) 
	  collect (dijkstra-distance a b network t) into res
	  finally return (cons a res))))
;;(prin1 (dijkstra-vector *network* (get-node 'mq *network*)))

(defun dijkstra-matrix (network)
  (loop for node-a in network
	for i from 1 
	do (message "%d" i)
	collect (dijkstra-vector network node-a) into res
	finally return (cons (cons "" (mapcar #'node-symbol network)) res)))
;;(dijkstra-matrix *network*)
;; africa ni spm + alle grenser
