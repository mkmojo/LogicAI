
; Aug 7/12
;
; The code here is designed to convert image data for a particular image taken 
; from the MKAM site (e.g., selecting noahf/STTR Data/{a specific image}/results),
; and convert these to the list-structured form used for caption-image alignment.
;======================================================================
; REQUIREMENTS FOR RUNNING: The file is self-contained, not requiring
;   other functions. However, the hash table 
;      *epilog-predicates-corr-to-feature-value-pairs*
;   must exist, and this is created by doing
;      (load "/u/schubert/lbs/WORK/my-work/processing/caption-processing/
;      attribute-spectra-and-data.lisp")
;  (without the break, of course).
;
; For conversion to work, the MKAM data for each image needs to be in a file 
; such as "grandma-moshe" or "graduation2". See
;     "/u/schubert/lbs/WORK/my-work/processing/caption-processing/
;     image-data/{graduation2,graduation4,grandma-moshe, ...}"
; These should contain the data sets "moused over" directly from the MKAM
; site, selecting 
;       noahf/STTR Data/{a specific image}/results.
;======================================================================
; A simple test function for 'lispify-image-data' that can be used
; in this directory to test the function for names like 'ben', 
; 'ben-grandma', 'graduation1', etc.:
;
(defun lispify (file)
   (setq result (lispify-image-data
                   (string-downcase (format nil "./~s" file))))
   (format t "~%~%~s" result))
; The result will be displayed, and can be examined as the value of 
; result'. E.g., (lispify 'ben-grandma).
; =====================================================================
; 
; The initial data in ./image-data/ were obtained from the MKAM site Aug 6/12,
; by mousing over the information from the MKAM tabulations. Each table
; line generates a line in the resulting file.
; 
; Each file contains (along possibly with empty lines, which will be
; ignored):
; @ the name of the image = the filename with .jpg added to the end
; @ one or more labels of form "Human" with an attached integer,
;   where each such label is followed by
;   - (maybe) a table of (nonbinary) feature judgements by humans and the machine
;   - (maybe) a table of (binary) attribute judgements by humans and the machine
;   - a table of weights assigned by the machine to each (nonbinary) feature value
;   - a table of weights assigned by the machine to each (binary) attribute value
;
; NOTE: THE MKAM AGE DATA CONTAIN INITIAL ITEMS LIKE "fage: 78.1"; THESE
;       ARE HERE CHANGED TO CONTIGUOUS CHARACTERS READABLE AS A SYMBOL,
;       NAMELY "fage=78.1". Such a symbol occurring in a line of
;       probabilities is then ignored in the construction of the
;       lispified image data.
; 
; The original intention was to arrange image data and captions into
; the form
; 
;    ((image-name1 caption1 (human_i certi attr1 cert1 ... attrk certk)
;                           (human_j certj ...                     ...)
;                           ... )
;     (image-name2 caption2 (human_m certm ...                     ...)
;                           (human_n certn ...                     ...)
;                           ... )
; 
; But the "features and attributes" are now rendered instead as EPILOG
; predicates; e.g., the data 'AGE 12to17' become just 'teen.n', 
; 'HAIR BALD' becomes 'bald-headed.a', and 'RACE EINDIAN' becomes
; 'racially-east-indian'; the idea is to keep EPILOG knowledge in 
; English-like form, and convert MKAM image jargon to this form,
; rather than the other way around. We want EPILOG knowledge to be
; easily understandable. Note that the predicates are chosen
; to uniquely correspond to a "spectrum" like race, hair color, or
; eye color (whereas feature/attribute values like BLACK, DARK don't
; uniquely correspond to particular features/attributes.) Of course,
; it would be even more English-like to use non-atomic predicates like
; '(have-as-part.v ((attr black.a) hair.n)', but that would complicate
; inference unnecessarily at this point.
;
; Also the caption data will be omitted, as they are available separately 
; (see ../image-captions/), and are not needed for the image data
; "lispification".

; So for each image we create a simple list structure like the following,
; allowing straightforward alignment processing:
;   (tanya-moshe.jpg
;    (human2 
;     person.n .99
;     baby-or-toddler.n 0.1237 child.n 0.2482 teen.n 0.2266 young-adult.n 
;          0.1079  youngish-adult.n 0.0403 middle-aged.a 0.1355 senior.n 0.1178
;     male.n 0.3725 female.n 0.6275
;     racially-white.a 0.3914 racially-black.a 0.137 racially-asian.a 0.1504 
;          racially-hispanic.a 0.1194 racially-east-indian.a 0.2013 
;          racially-arabic.a 0
;     ...etc.)
;    (human5
;     person.n .99
;     baby-or-toddler.n 0.043 ...
;     ... etc.)
;    ... 
;   )  
; Here the certainty of the 'person.n' classification has been hallucinated,
; as it's currently not given in the MKAM data, but presumably we can't
; be sure the classification is correct -- e.g., consider monkeys in
; a zoo, snowmen, statues, posters, images on shirts, etc. Also in future 
; we may want to detect other entities, such as dogs, furniture, trees, etc.
;
; We provide a function 'lispify-image-data', which reads a file such as 
; ./image-data/reggi-ben, and returns a list formatted as above, ready
; for use in image-caption alignment.

(defun read-simple-file (file verbose); Aug 7/12
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Read the objects (atoms or other Lisp objects) in 'file' (a character
; string), forming the list of contents of the successive lines of
; 'file'. Any semicolon and the material following it is ignored.
; N.B.: Lisp objects ARE NOT ALLOWED TO RUN OVER MULTIPLE LINES
; IN 'FILE' (the program was written to read image data, which just
; consist of words and numbers, not bracketed lists, etc.) The contents
; of a line are represented as a list of objects read from that line.
; Thus we get a list of form
;   ((obj ... obj) (obj ... obj) ... (obj ... obj)).
;
; The program is also used to read files containing captions,
; expected to be a string object on a single line.  
;
; If 'verbose' is non-nil, then "Data file has been read" is printed, 
; along with the objects in the first and last line, and the total 
; number of lines (to give ; some sign of life when a long file has 
; been read).
;
; We create and use a stream *data-stream* for reading from 'file'.
; The call to 'read-line-of-objs' uses 'read-line' & 'read-from-string' 
; repeatedly, checking for end-of file, using (parameter) eof-error-p 
; = nil (i.e., no end of file error should be generated) and eof-value
; = '|eof|.
;
 (prog (objs-list)
       (with-open-file (*data-stream* file :direction :input)
         (do ((objs nil))
             ((equal objs '|eof|); my arbitrary choice of eof-symbol
              (princ "Data file has been read") (terpri) )
             (if objs (push objs objs-list))
             (setq objs (read-line-of-objs *data-stream* '|eof|)) ))
      (when verbose
      (princ "the objects in the first line are: ")
      (princ (if objs-list (car (last objs-list)) nil)) (terpri)
      (princ "the objects in the last line are: ")
      (princ (if objs-list (car objs-list) nil)) (terpri) )
    ; reverse the order of the sublists, and return the result (& comment)
    (setq objs-list (reverse objs-list))
    (when verbose
     (princ "number of lines = ") (princ (length objs-list)) (terpri) )
    ;
    (return objs-list)
 )); end of read-simple-file
    

(defun read-line-of-objs (istream eof-value) ; Aug 7/12
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; PURPOSE: To read a line from istream using read-line (thus obtaining
;        a character string), and to convert this into a list of Lisp
;        objects, using 'read-from-string'. Lines are read only up to 
;        the first semicolon, if any. 
;        N.B.: THE LINE SUPPLIED MUST CONSIST OF COMPLETE LISP OBJECTS.
; FORM OF OUTPUT: a list of Lisp objects
;
  (prog (line semi obj (result nil))
        (setq line (read-line istream nil eof-value))
        (if (equal line eof-value) (return eof-value))
        (setq semi (position #\; line))
        (if semi (setq line (subseq line 0 semi)))
        (if (string= line "") (return nil))

        ; Add an initial "(" and a final ")" to the string that is now
        ; the value of 'line', and use 'read-from-string' to turn this
        ; into a list of objects:
        (return (read-from-string (format nil "(~a)" line)))
 )); end of read-line-of-words


(defun lispify-image-data (data-from-one-image) ; Aug 7-10/12; tested on
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ; the initial 16 images;
; 'data-from-one-image' must be a file name (a character string),
; where this file contains the data for an image in the usual form
; used in MKAM (e.g., see noahf/STTR Data/{a specific image}/results
; at the MKAM website), i.e., (with each of the following bullet 
; items and each row of each table occupying one line):
;
; @ the name of the image = the filename with .jpg added to the end
; @ a label of form "Human" with an attached integer, followed by
;   successive rows of the following tables (with blank lines ignored):
;   - maybe a table of (nonbinary) feature judgements by humans and the machine
;   - maybe a table of (binary) attribute judgements by humans and the machine
;   - a table of weights assigned by the machine to each (nonbinary) feature value
;   - a table of weights assigned by the machine to each (binary) attribute value
; @ possibly another label of form "Human" with an attached integer, followed by
;   the above (up to) 4 types of tables;
; @ possibly another label of form "Human" with an attached integer, followed by
;   the above (up to) 4 types of tables; 
; @ etc.  
;
; As noted the tables need not be separated -- the program here will 
; figure out what rows belong to what tables, based on the table headers.
; 
; The program will extract and reshape the image data into the form
; needed for use in the 'align' algorithm; the first two tables above 
; are ignored if present -- 'align' cares only about the feature/
; attribute probabilities (and of course using the human judgements,
; if included, would be cheating); the result is of form (where the 
; predi are EPILOG predicates such as male.n, teen.n, bald-headed.a, etc.)
; 
;    (image-name (human_i person.n .99 certi pred1 cert1 ... predk certk)
;                (human_j person.n .99 certj ...                     ...)
;                ... )
; 
; (The caption can be obtained separately under the same file name
; in a different directory -- see ./image-captions/. It isn't needed
; for lispification of image-derived information.)
;
; Method: We first use 'read-simple-file' to obtain a list of lines
;   from the file 'data-from-one-image', with each line represented
;   as a list of atoms (image name, feature/attribute names & values,
;   and certainty factors). Then we use knowledge about the structure
;   of the data and the keywords used in them to form a list structure
;   of the desired form.
;
 (prog (humans-and-features image-name result human partial-result 
        line1 line2 spectrum preds)
       (setq humans-and-features (read-simple-file data-from-one-image t))
       ; delete empty lines (whch show up as nil's):
       (setq humans-and-features (remove nil humans-and-features))
       (if (null humans-and-features) ; unexpected empty data set
           (return-from lispify-image-data nil))
       ; Do some sanity checks on the image name, expected as 1st item:
       (setq image-name (pop humans-and-features)); expect singleton list
       (if (> (length image-name) 1)
           (format t "~%~** WARNING: ~s IS AN UNEXPECTED IMAGE NAME"
                     image-name)
           (setq image-name (car image-name))); lift out the singleton
       (if (listp image-name); still a list?
           (format t "~%~** WARNING: ~s IS AN UNEXPECTED IMAGE NAME"
                     image-name))
       (when (null humans-and-features); no data for the image?
             (format t "~%~** NO DATA PROVIDED FOR IMAGE ~s" image-name)
             (return nil))

       ; Process 'humans-and-features' line by line (each line is a
       ; top-level list element). The outer loop is for successive
       ; humans (and we add the 'person.n .99', 'Human...' and image
       ; name at the end); the second-level loop is for discarding
       ; irrelevant tables (using a 3rd-level loop for successive rows)
       ; and incorporating relevant tables into the result for each human.
       (loop (if (null humans-and-features) 
                 (return nil)); break out of loop (go to end)
             ; Do some sanity checks on the 'human...' name,
             ; expected as header before the tabular data:
             (setq partial-result nil); results for 1 human
             (setq human (pop humans-and-features)); (singleton expected)
             (if (> (length human) 1)
                 (format t "~%~** WARNING: ~s IS AN UNEXPECTED ~
                           HUMAN-DESIGNATOR IN ~s" human image-name)
                 (setq human (car human)))
             (if (listp human); still a list?
                 (format t "~%~** WARNING: ~s IS AN UNEXPECTED ~
                           HUMAN-DESIGNATOR IN ~s" human image-name))
             ; No tabular data or unexpected header format?
             (if (or (null humans-and-features)
                     (< (length (car humans-and-features)) 3))
                 (format t "~%** UNEXPECTED OR DEGENERATE TABLE HEADER ~
                           FORMAT ~%   ~S ~%   ETC. IN ~S DATA" 
                     (car humans-and-features image-name)))
             (loop ; Process tabular data:
                   (if (null humans-and-features) 
                       (return nil)); break out of loop (go to end)
                   ; If a line containing 'Human' is seen (indicating a
                   ; table containing human judgements plus most-probable
                   ; computer choices), pop off successive lines until 
                   ; the *first remaining* line contains at least 3 items, 
                   ; where none is 'x' (except perhaps the first -- 
                   ; allowing for the unlikely event that 'x' gets 
                   ; used as a feature/attribute, i.e., a spectrum,
                   ; in future), and the *second remaining* line contains
                   ; numeric information:
                   (when (member 'human (car humans-and-features))
                         (pop humans-and-features)
                         (loop (if (null humans-and-features)
                                   (return nil)); break out of loop
                               (if (or (< (length (car humans-and-features)) 3)
                                       (< (length (second humans-and-features)) 2)
                                       (find-if-not #'numberp 
                                          (cdr (second humans-and-features))))
                                   (pop humans-and-features)
                                   ; Actual image data have been reached, so
                                   ; break out of the table deletion loop:
                                   (return nil))))
                  (if (null humans-and-features)
                       (return nil))
                  (setq line1 (pop humans-and-features))
                  (when (null humans-and-features)
                        (format t "~%** DATA FILE ENDED W/O PROBABILITIES ~
                           FOR FEATURES ~%   ~s OF ~s" line1 image-name)
                        (return nil)); break out of loop
                  (setq line2 (pop humans-and-features))
                  ; check if these are numbers (except perhaps initial symbol):
                  (cond ((find-if #'symbolp (cdr line2)); unexpected symbol?
                         (format t "~%** VALUES ~s ~%   IN ~s SHOULD ALL BE ~
                           PROBABILITIES CORRESPONDING TO ~%   ~s" 
                           line2 image-name line1)
                         (format t "~%** THEREFORE SKIPPING THE 2 DATA LINES")
                         (if (= (length (car humans-and-features)) 1)
                             (return nil))); new 'human...' -- exit loop
                        (t ; no unexpected symbols; drop initial 
                           ; symbol (such as 'fage=78.1'), if any:
                         (if (symbolp (car line2))
                             (setq line2 (cdr line2)))
                         ; Iff |(cdr line1)| = |line2|, integrate them into
                         ; the partial-result:
                         (when (= (length (cdr line1)) (length line2))
                               (setq spectrum (car line1))
                               (setq preds
                                 (mapcar 
                                   #'(lambda (x) 
                                      (gethash (list spectrum x)
                              *epilog-predicates-corr-to-feature-value-pairs*))
                                   (cdr line1)))
                              (push (apply #'append (mapcar #'list preds line2))
                                    partial-result))
                         (if (= (length (car humans-and-features)) 1)
                             (return nil)) ))); new 'human...' -- exit loop
                              
             ; Data ('partial-result') for one (human) entity completed;
             ; need to reverse and "flatten" the list of lists (each
             ; sublist containing the data from 2 consecutive rows)
             ; and prepend the designator 'human...', and 'person.n .99':
             (setq partial-result 
                   (append `(,human person.n .99)
                            (apply #'append (reverse partial-result))))
             (push partial-result result))

       ; All (human) entity data have been processed; we return
       ; the reverse of 'result', prepended with the image name:
       (return (cons image-name (reverse result))) 
 )); end of lispify-image-data

