;;;; machinations.lisp - the brains

(in-package #:machinations)

(defun raw-p (item)
  "Is ITEM a raw material?"
  (if (member item *raws*) t))

(defun product-p (product recipe)
  "Return RECIPE if one of its outputs is PRODUCT, otherwise return nil."
  (if (find product (products recipe) :key #'car) recipe))

(defun recipes-for (product)
  "Return all recipes whose outputs contain PRODUCT."
  (remove-if-not (lambda (recipe)
		   (product-p product recipe))
		 *recipes*))

(defun recipe-for (product)
  "Return first recipe for PRODUCT. Useful for things that can only be made one way."
  ;; Naive version
  (first (recipes-for product)))

(defun quantity (thing-quantity-pair)
  "Trivial wrapper around #'second. A function to be used internally, not by user."
  (second thing-quantity-pair))

(defun name (thing-quantity-pair)
  "Trivial wrapper around #'first. A function to be used internally, not by user."
  (first thing-quantity-pair))

(defun quantity-required (ingredient recipe)
  "Given an ingredient name and a recipe object, return the ingredient's quantity
   within that recipe."
  (quantity (assoc ingredient (ingredients recipe))))

(defun quantity-required-for (ingredient product)
  "How much INGREDIENT is needed to make PRODUCT?"
  ;; Note only useful for products with a single recipe
  (quantity-required ingredient (recipe-for product)))

(defun d-over-n (product recipe)
  "Find the ratio of duration to the number of units made for one of the recipe's
   products (the one we're interested in)."
  (let ((n (quantity (assoc product (products recipe))))
	(d (duration recipe)))
    (/ d n)))

(defun machines-for (product rate)
  "How many machines do you need making PRODUCT to meet the desired RATE? Not recursive."
  (nth-value 0 (ceiling (* (d-over-n product (recipe-for product)) rate))))

(defun sum-duplicates (alist)
  "Merge multiple entries of the same name in alist, summing their lastcars."
  (let ((results nil))
    (dolist (x alist)
      (cond ((assoc (first x) results)
             (incf (second (assoc (first x) results)) (second x)))
            (t
             (push x results))))
    results))

(defun calculate (product &optional (rate *rate*))
  "Builds a tree detailing the number of machines that need to be making each
   ingredient. Also returns totals."
  (let (tally)
    (labels ((calculate-aux (product rate)
	       (let* ((recipe (recipe-for product))
		      (machines-needed (machines-for product rate))
		      (ingredients (remove-if #'raw-p (ingredients recipe) :key #'name)))
		 (setf tally (push `(,product ,machines-needed) tally))
		 (cons `(,product ,machines-needed)
		       (mapcar (lambda (ingredient)
				 (calculate-aux (name ingredient)
						(* rate (quantity ingredient))))
			       ingredients)))))
      (values (calculate-aux product rate)
	      (sum-duplicates tally)))))

(defun show (tree &optional (indent 0))
  "Learn to pretty-print already. I want the parens in there too."
  (flet ((fmt (tree indent)
	   (format t "~&~v@t~{~a ~}" indent tree))
	 (pair-p (x)
	   (and (= (length x) 2) (symbolp (first x)) (numberp (second x)))))
    (when tree
      (cond
	((atom tree)
	 (error "All leaves must be name/value pairs."))
	((pair-p tree)
	 (fmt tree indent))
	(t
	 (mapc (lambda (x)
		 (show x (+ indent 2)))
	       tree)))))
  nil)

(defun breakdown (product &optional (rate *rate*))
  "The main interface function."
  (multiple-value-bind (tree totals)
      (calculate product rate)
    (format t "~%  Total number of machines required for each component:~%~%")
    (format t "~{  ~{~a ~}~%~}~%" totals)
    (format t "  Full breakdown:~%~%")
    (show tree)))
    
