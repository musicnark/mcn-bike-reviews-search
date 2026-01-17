  (require 'cl-lib)

  (defvar mcn/download-from-live-site nil
	"If non-nil, fetch bike reviews from the live site.
Otherwise, load the local cached hashmap.")

  (setq bike-review-urls (make-hash-table :test 'equal))
  (setq bike-review-table (make-hash-table :test 'equal))
  (setq bike-review-hashmap (make-hash-table :test 'equal))

  (defun pc-csv-parse-line (line)
	(split-string line "," t))
  
  (defun pc-csv-parse-file (file)
	(with-temp-buffer
	  (insert-file-contents file)
	  (mapcar #'pc-csv-parse-line
			  (split-string (buffer-string) "\n" t))))

(defun fetch-html (url)
  "Fetch HTML from URL, or return nil on any failure."
  (condition-case err
      (let ((buf (url-retrieve-synchronously url)))
        (when (bufferp buf)
          (with-current-buffer buf
            (goto-char (point-min))
            (when (re-search-forward "review__facts-and-figures__table" nil t)
              (buffer-substring-no-properties (point) (point-max))))))
    (error
     (message "Skipping URL %s: %s" url err)
     nil)))
  
  (defun parse-html (html)
	(when html
    (condition-case err
        (with-temp-buffer
          (insert html)
          (libxml-parse-html-region (point-min) (point-max)))
      (error
       (message "Failed to parse HTML: %s" err)
       nil))))

  (defun pc-insert-hash-table (csv-line)
	(let ((name (nth 0 csv-line))
		  (url (nth 1 csv-line)))
	  (puthash name url bike-review-urls)))

  (defun pc-extract-specs-from-table (table-node)
	"Extract bike specifications from a HTML <table> DOM node.

TABLE-NODE should be a parsed <table> node as returned by `libxml-parse-html-region'.
Returns a property list (plist) where each key is a keyword version of the spec label
and each value is the corresponding table cell content as a string.

Example:
  (:engine-size \"125\" :seat-height \"765\" ...)

Raises an error if TABLE-NODE is not a <table> node or if no <tbody> is found.

See also `query-bikes' and `pc-eval-query'."
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

(defun query-bikes-by-tag (tag op value &optional descending)
  "NOTICE: This function is now depricated, only kept for posterity's sake.
Return a list of lists (bike-name value url) where the PLIST property TAG satisfies OP VALUE, sorted by the property.
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

(defun pc-eval-query (query specs)
  "Recursively evaluate QUERY against a SPECS property list.

QUERY is a compound expression containing the following forms:
  - (:tag OP VALUE) for comparison, e.g., (:seat-height > 800)
  - (and clause1 clause2 ...) for logical AND
  - (or clause1 clause2 ...) for logical OR
  - (not clause) for logical NOT

SPECS is a plist of bike specifications as returned by `pc-extract-specs-from-table'.

Returns t if the bike matches QUERY, nil otherwise.

Raises an error for invalid query expressions.

Designed to be called by `query-bikes' for end-users."
  (pcase query
    ;; Boolean operators
    (`(and . ,clauses)
     (cl-every (lambda (q) (pc-eval-query q specs)) clauses))
    (`(or . ,clauses)
     (cl-some (lambda (q) (pc-eval-query q specs)) clauses))
    (`(not ,clause)
     (not (pc-eval-query clause specs)))
    ;; Comparison: (:tag op value)
    (`(,tag ,op ,value)
     (let* ((raw (plist-get specs tag))
            (num (when raw
                   (string-to-number
                    (replace-regexp-in-string "[^0-9.-]" "" raw)))))
       (and num
            (funcall op num value))))
    (_ (error "Invalid query: %S" query))))

(defun query-bikes (query &optional sort-tag descending)
  "Return a list of (bike-name sort-value url) matching QUERY.

This function is not (yet) interactive, and is best called from `eshell' or `ielm'.

QUERY is a compound filter expression supporting <, >, =, etc.

SORT-TAG is a plist keyword (bike spec) to sort by (e.g., :max-power, :top-speed).
SORT-VALUE is nil if SORT-TAG is not specified.

Optionally, setting DESCENDING to non-nil sorts output high → low.

See this project's README for example usage.

See also `pc-eval-query' and `pc-extract-specs-from-table'."
  (let (results)
    (maphash
     (lambda (name specs)
       (when (pc-eval-query query specs)
         (let* ((url (gethash name bike-review-urls))
                (sort-val
                 (when sort-tag
                   (string-to-number
                    (replace-regexp-in-string
                     "[^0-9.-]" ""
                     (or (plist-get specs sort-tag) ""))))))
           (push (list name sort-val url) results)))) ;; sort-val needs changing?
     bike-review-hashmap)
    ;; optional sorting
    (if sort-tag
        (sort results
              (lambda (a b)
                ((if descending #'> #'<)
                 (nth 1 a) (nth 1 b))))
      (nreverse results))))

  (defun bike-search-initialise ()
  (interactive)
  ;; convert CSV to hash table
  (setq br (pc-csv-parse-file "~/.emacs.d/lisp/mcn-bike-reviews-search/Bike_Reviews.csv"))
  
  (mapcar #'pc-insert-hash-table br)
  
  ;; iterate over keys in the hash table:
  ;; Note: Initial load of all bike reviews uses ~1.3GB memory.
  (if mcn/download-from-live-site
	  (progn
		(maphash
		 (lambda (name url)
		   (let ((page (parse-html (fetch-html url))))
			 (when page
			   (puthash name page bike-review-table))))
		 bike-review-urls)
		;; concatenate the three specs tables from the web page
		(maphash
		 (lambda (k v)
		   (puthash k
					(append
					 (pc-extract-specs-from-table (nth 3 (nth 2 (gethash k bike-review-table)))) ;; spec table 1
					 (pc-extract-specs-from-table (nth 3 (nth 4 (nth 2 (gethash k bike-review-table))))) ;; spec table 2
					 (pc-extract-specs-from-table (nth 3 (nth 8 (nth 2 (gethash k bike-review-table)))))) ;; spec table 3
					;; // add more searchable fields here //
					bike-review-hashmap))
		 bike-review-table))
	(progn
	  (setq bike-review-hashmap
			(with-temp-buffer
			  (insert-file-contents-literally "~/.emacs.d/lisp/mcn-bike-reviews-search/bike-reviews-hashtable.el")
			  (read (current-buffer)))))))

;; MVP TODO:
;; - DONE combine all three html tables into one plist (but parse them individually?)
;; - DONE save hash-table locally
;; - DONE make search function more advanced/modular (ability to combine predicates)

;; Full version TODO:
;; - make it *interactive*
;; - extract specs from table generically/modularly, search for any table element and process them
;; - add date as a field in plist
;; - add bike make/model as a field in plist
;; - add MCN star rating and owners reviews score
;; - add ability for partial matches on a search (name.contains("kawasaki"))
;; - learn more tree traversal
;; - automate importing data from Monday.com (for updates)
;; - integrate with LLM (via n8n?) to generate pages/newsletters/the lot
;; - add page copy to plist? Worth doing a "body copy contains 'great' 'commuter'" filter?
(provide 'bike-reviews)
