(defun pc-csv-parse-line (line)
	(split-string line "," t))

(defun pc-csv-parse-file (file)
	(with-temp-buffer
		(insert-file-contents file)
		(mapcar #'pc-csv-parse-line
						(split-string (buffer-string) "\n" t))))

;; convert CSV to hash table
(setq br (pc-csv-parse-file "~/Desktop/Bike_Reviews.csv")) ;; TODO currently has to be downloaded separately, but shouldn't have to be?

(defun pc-insert-hash-table (csv-line)
	(let ((name (nth 0 csv-line))
				(url (nth 1 csv-line)))
		(puthash name url bike-review-urls)))

(setq bike-review-urls (make-hash-table :test 'equal))

(mapcar #'pc-insert-hash-table br)

(setq bike-review-table (make-hash-table :test 'equal))

(defun fetch-html (url)
	(with-current-buffer (url-retrieve-synchronously url)
						 (goto-char (point-min))
						 (re-search-forward "review__facts-and-figures__table") ;; skip HTML body
						 (buffer-substring-no-properties (point) (point-max))))

(defun parse-html (html)
	(with-temp-buffer
		(insert html)
		(libxml-parse-html-region (point-min) (point-max))))

;; iterate over keys in the hash table:
;; just wait until memory usage gets to 1.2 ish gb, then you're fully loaded~
(setq output
			(maphash
			 (lambda (name url)
				 (let ((page (parse-html (fetch-html url))))
					 (puthash name page bike-review-table)))
			 bike-review-urls))

(require 'cl-lib)

(defun pc-extract-specs-from-table (table-node)
	"Extract bike specs from a <table> DOM node and return a plist.

Currently, it is unable to collate all relevant specs into one list, this needs fixing."
	(unless (and (listp table-node) (eq (car table-node) 'table))
		(error "Expected a <table> DOM node"))

	;; find the <tbody> node inside the table
	(let ((tbody (cl-find-if (lambda (n)
														 (and (listp n) (eq (car n) 'tbody)))
													 (cddr table-node))))
		(unless tbody
			(error "No <tbody> found in table"))

		(let ((specs '()))
			;; helper to extract (label . value) from <tr>
			(cl-labels ((extract-tr (tr-node)
										(when (and (listp tr-node) (eq (car tr-node) 'tr))
											(let* ((children (cddr tr-node))
														 (th-node (cl-find-if (lambda (n)
																										 (and (listp n) (eq (car n) 'th)))
																									 children))
														 (td-node (cl-find-if (lambda (n)
																										 (and (listp n) (eq (car n) 'td)))
																									 children))
														 (label (when th-node (car (last th-node))))
														 (value (when td-node
																			;; flatten strings in <td>, ignore nested tags
																			(mapconcat (lambda (x) (if (stringp x) x "")) (cdr td-node) ""))))
												(when (and label value)
													(cons label (string-trim value)))))))

				;; iterate over all <tr> nodes in <tbody>
				(dolist (child (cddr tbody))
					(let ((kv (extract-tr child)))
						(when kv
							;; convert label to keyword symbol
							(setq specs (plist-put specs
																		 (intern (concat ":" (replace-regexp-in-string " " "-" (downcase (car kv)))))
																		 (cdr kv)))))))
			specs)))


(setq bike-review-hashmap (make-hash-table :test 'equal))

(maphash
 (lambda (k v)
	 (puthash k
						;; TODO only works with individual table nodes (because manual tree traversal~)
						(append (pc-extract-specs-from-table (nth 3 (nth 2 (gethash k bike-review-table)))) (pc-extract-specs-from-table (nth 3 (nth 4 (nth 2 (gethash k bike-review-table))))) (pc-extract-specs-from-table (nth 3 (nth 8 (nth 2 (gethash k bike-review-table))))))
						bike-review-hashmap)
	 )
 bike-review-table)

(defun query-bikes-by-tag (tag op value &optional descending)
	"Return a list of lists (bike-name value url) where the PLIST property TAG satisfies OP VALUE, sorted by the property.
TAG should be a keyword like :seat-height.
OP should be a symbol: '<, '> , '<= , '>= , '=.
VALUE should be a number for numeric comparisons.
If DESCENDING is non-nil, sort in descending order."
	(let (results)
		(maphash
		 (lambda (k specs)
			 (let* ((raw (plist-get specs tag))
							;; convert string like "810mm" to number
							(num (when raw
										 (string-to-number (replace-regexp-in-string "[^0-9.-]" "" raw))))
							;; get URL for this bike
							(url (gethash k bike-review-urls)))
				 (when (and num
										(pcase op
											('<  (< num value))
											('>  (> num value))
											('<= (<= num value))
											('>= (>= num value))
											('=  (= num value))
											('<>  (and (< num value) (> num 1)))
											(_ (error "Unsupported operator: %S" op))))
					 (push (list k num url) results))))
		 bike-review-hashmap)
		;; sort results by the numeric property
		(sort results
					(if descending
							(lambda (a b) (> (nth 1 a) (nth 1 b)))
						(lambda (a b) (< (nth 1 a) (nth 1 b)))))))

;; Best Superbikes
(query-bikes-by-tag :max-power '> 200) 

;; MCN's Favourite A2 Bikes
(query-bikes-by-tag :max-power '< 47) 

;; Best Mile-Munching Tourers
(query-bikes-by-tag :tank-range '> 240) 

;; Best Bikes for Speed-Demons
(query-bikes-by-tag :top-speed '> 160) 

;; MCN's Favourite 750cc Bikes
(query-bikes-by-tag :engine-size '= 750) 

;; Best Bikes for Shorter Riders
(query-bikes-by-tag :seat-height '< 800) 

;; Best Lightweight Bikes
(query-bikes-by-tag :bike-weight '< 150) 

;; most fuel efficient bikes
(query-bikes-by-tag :average-fuel-consumption '> 100)

;; best new bikes under £5000
(query-bikes-by-tag :new-price '<> 5000)

;; best used bikes under £2000
(query-bikes-by-tag :used-price '<> 2000)

;; ??
(query-bikes-by-tag :annual-service-cost '<> 50)

;; TODO:
;; - DONE combine all three html tables into one plist (but parse them individually?)
;; - make search function more advanced/modular (ability to combine predicates)
;; - add date as a field in plist
;; - add bike make/model as a field in plist
;; - add MCN star rating and owners reviews score
;; - add ability for partial matches on a search (name.contains("kawasaki"))
;; - save hash-table locally
;; - learn tree traversal
;; - automate stealing data from Monday.com (for updates)
;; - integrate with LLM (via n8n) to generate pages/newsletters/the lot
;; - add page copy to plist? Worth doing a "body copy contains 'great' 'commuter'" filter?
