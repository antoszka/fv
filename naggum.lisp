(defun delimited-substrings (string &rest delimiters)
  "Return as multiple values the substrings delimited by each delimiter."
  (loop
     with substrings = ()
     for delimiter in delimiters
     for start = 0 then (+ end length)	;after last delimiter ends
     for end = (search delimiter string :start2 start)
     for length = (length delimiter)
     do (push (subseq string start end) substrings)
     until (null end)			;the string was exhausted
     finally
     (when end				;the delimiters were exhausted
       (push (subseq string (+ end length)) substrings))
     (return (values-list (nreverse substrings)))))
