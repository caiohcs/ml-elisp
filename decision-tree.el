;; TODO: make it work with variable number of classes
;; TODO: remove hard coded variables
;; TODO: make it work for a random forest
;; TODO: use other impurity functions
;; TODO: function to measure accuracy
;; TODO: rename functions and variables
;; TODO: add unit tests

;;  INPUT

(cl-defun gini-impurity-leaf (x y)
  ;; TODO: optimization here
  "Returns 1 - px² - py²."
  (let* ((sum (* 1.0 (+ x y)))
	 (px (/ x sum))
	 (py (/ y sum)))
    (- 1 (* px px) (* py py))))

(cl-defun gini-impurity-leaves ((x y) (z w))
  "Returns (nxy / (nxy + nwz)) * (1 - px² - py²) +
(nzw / (nxy + nwz)) * (1 - pz² - pw²)."
  (let* ((sum-xy (* 1.0 (+ x y)))
	 (sum-zw (* 1.0 (+ z w)))
	 (sum (+ sum-xy sum-zw))
	 (w-xy (/ sum-xy sum))
	 (w-zw (/ sum-zw sum))
	 (gini-impurity-xy (gini-impurity-leaf x y))
	 (gini-impurity-zw (gini-impurity-leaf z w)))
    (+ (* w-xy gini-impurity-xy)
       (* w-zw gini-impurity-zw))))

(defun count-leaf (predicate limit attribute class seq)
  "Returns how many samples in a class are true according to the predicate
 applied to attribute using the limit."
  (loop
   for x in seq
   count (and (funcall predicate (nth attribute x) limit)
	      (eql (car (last x)) class))))

(defun count-class (data class)
  "Returns how many samples are of a given class."
  (loop
   for x in data
   count (eql (car (last x)) class)))

(defun best-limit-for-attribute (attributes data)
  "Calculates the best limit that separates classes for a given attribute."
  (loop
   for attribute in attributes
   collect
   (let* ((flower-data (copy-tree data))
	  (sorted-1 (sort flower-data (lambda (x y)
					(< (nth attribute x) (nth attribute y)))))
	  (limits-1 (cl-remove-if-not (lambda (x) x)
				      (mapcar* (lambda (x y)
						 (let ((x (nth attribute x))
						       (y (nth attribute y)))
						   (when (/= x y) (/ (+ x y) 2))))
					       sorted-1 (cdr sorted-1))))
	  (leaves-limits-1 (loop
			    for lim in limits-1
			    collect (list lim
					  (list (count-leaf #'< lim attribute 'versicolo sorted-1)
						;; Can I improve this?
						(count-leaf #'< lim attribute 'virginica sorted-1))
					  (list (count-leaf #'> lim attribute 'versicolo sorted-1)
						(count-leaf #'> lim attribute 'virginica sorted-1)))))
	  (limits-1-impurity (mapcar (lambda (x)
				       (list (first x)
					     (gini-impurity-leaves (second x) (third x))))
				     leaves-limits-1))
	  (limits-1-best (first (sort limits-1-impurity (lambda (x y)
							  (< (second x)
							     (second y)))))))
     (list :attribute attribute
	   :limit (first limits-1-best)
	   :impurity (second limits-1-best)
	   :attributes-unused (cl-remove-if (lambda (x)
					      (= x attribute))
					    attributes)))))

(defun tree-build-step (input-data attributes-unused)
  "Returns a property list with :attribute :limit :impurity :attributes-unused :data-1 :data-2."
  (let* ((best-attribute (first
			  (sort (best-limit-for-attribute attributes-unused input-data)
				(lambda (x y)
				  (< (plist-get x :impurity)
				     (plist-get y :impurity))))))
	 (data-1 (cl-remove-if-not (lambda (x)
				     (< (nth (plist-get best-attribute :attribute) x)
					(plist-get best-attribute :limit)))
				   input-data))
	 (data-2 (cl-remove-if-not (lambda (x)
				     (> (nth (plist-get best-attribute :attribute) x)
					(plist-get best-attribute :limit)))
				   input-data)))
    (append best-attribute (list :data-1 data-1 :data-2 data-2))))


(defun tree-build (input-data attributes-unused impurity)
  "Build a decision tree."
  (let ((num-versicolo (count-class input-data 'versicolo))
	(num-virginica (count-class input-data 'virginica)))
    (cond ((or (not attributes-unused)
	       ;; Stop because there are no more attributes to use.
	       (or (= 0 num-versicolo)
		   (= 0 num-virginica)))
	   ;; Stop because the data is all in one class.
	   (list :versicolo num-versicolo
		 :virginia num-virginica))

	  (t
	   (let ((current-step (tree-build-step input-data attributes-unused)))
	     (cond ((>= (plist-get current-step :impurity) impurity)
		    ;; Stop because the current impurity is higher than the previous impurity.
		    (list :versicolo num-versicolo
			  :virginia num-virginica))
		   (t
		    (message (prin1-to-string current-step))
		    (list (list :attribute (plist-get current-step :attribute)
				:limit (plist-get current-step :limit)
				:less-than (tree-build (copy-tree (plist-get current-step :data-1))
						 (plist-get current-step :attributes-unused)
						 (plist-get current-step :impurity))
				:greater-than (tree-build (copy-tree (plist-get current-step :data-2))
						(plist-get current-step :attributes-unused)
						(plist-get current-step :impurity)))))))))))

(let ((input-data (copy-tree flower-data)))
  (tree-build input-data '(0 1 2 3) 1))

;;  Output

((:attribute 3
	     :limit 1.75
	     :less-than ((:attribute 2
				     :limit 4.95
				     :less-than ((:attribute 0
							     :limit 4.95
							     :less-than ((:attribute 1
										     :limit 2.45
										     :less-than (:versicolo 1 :virginia 0)
										     :greater-than (:versicolo 0 :virginia 1)))
							     :greater-than (:versicolo 46 :virginia 0)))
				     :greater-than (:versicolo 2 :virginia 4)))
	     :greater-than ((:attribute 2
					:limit 4.85
					:less-than ((:attribute 0
								:limit 5.95
								:less-than (:versicolo 1 :virginia 0)
								:greater-than (:versicolo 0 :virginia 2)))
					:greater-than (:versicolo 0 :virginia 43)))))



