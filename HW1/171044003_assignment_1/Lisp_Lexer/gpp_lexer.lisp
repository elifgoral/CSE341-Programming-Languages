
(defstruct Lexer
      name
      token
)

(defvar Keywords nil)
(defvar Operators nil)
(defvar Comments nil)

(setq digitList nil)
(setq charList nil)

(setq startWithDigit 0)
(setq includeUnknown 0)

(defvar resultList nil) ; result list for output file

(defun buildList(na to listName)
      (setq bufferStruct (make-Lexer
            :name na
            :token to
            )
      )

      (if(equal listName "Keywords")
            (push bufferStruct Keywords)
      )
      (if(equal listName "Operators")
            (push bufferStruct Operators)
      )
      (if(equal listName "Comments")
            (push bufferStruct Comments)
      )
)

(defun buildKeywords()
      (setq listName "Keywords")

      (buildList "and" "KW_AND" listName)
      (buildList "or" "KW_OR" listName)
      (buildList "not" "KW_NOT" listName)
      (buildList "equal" "KW_EQUAL" listName)
      (buildList "less" "KW_LESS" listName)
      (buildList "nil" "KW_NIL" listName)
      (buildList "list" "KW_LIST" listName)
      (buildList "append" "KW_APPEND" listName)
      (buildList "concat" "KW_CONCAT" listName)
      (buildList "set" "KW_SET" listName)
      (buildList "deffun" "KW_DEFFUN" listName)
      (buildList "for" "KW_FOR" listName)
      (buildList "if" "KW_IF" listName)
      (buildList "exit" "KW_EXIT" listName)
      (buildList "load" "KW_LOAD" listName)
      (buildList "disp" "KW_DISP" listName)
      (buildList "true" "KW_TRUE" listName)
      (buildList "false" "KW_FALSE" listName)
)

(defun buildOperators()
      (setq listName "Operators")

      (buildList "+" "OP_PLUS" listName)
      (buildList "-" "OP_MINUS" listName)
      (buildList "/" "OP_DIV" listName)
      (buildList "*" "OP_MULT" listName)
      (buildList "(" "OP_OP" listName)
      (buildList ")" "OP_CP" listName)
      (buildList "**" "OP_DBLMULT" listName)
      (buildList "“" "OP_OC" listName)
      (buildList "”" "OP_CC" listName)
      (buildList "," "OP_COMMA" listName)
)

(defun buildComments()
      (setq listName "Comments")
      (buildList ";;" "COMMENT" listName)
)

(defun buildSyntax()
    (buildKeywords)
    (buildComments)
    (buildOperators)
)

#||
    paramter key: charlist.
    We are scanning all elements in the keywords list.
    If the length of the keyword and the length of the charlist are the same,
    it checks the letters one by one, and returns the token if it matches completely.
    otherwise it returns nil.
||#
(defun isKeyword(key)
      (setq counter 0)
      (dolist (buffer Keywords)
          (if(= (length (Lexer-name buffer)) (length key))
                (progn
                        (dotimes (n  (length key))
                             (if(equal (nth n key) (aref (Lexer-name buffer) n))
                                      (setq counter (+ counter 1))
                              )
                        )
                        (if(= counter (length key))
                              (return-from isKeyword (Lexer-token buffer))
                        )
                )

          )
          (setq counter 0)
      )
      (return-from  isKeyword nil)
)

#||
    if that function finds the operator, return its token,
    otherwise return nil.
||#
(defun isOperator(op)
    (dolist (buffer Operators)
        (if(equal (Lexer-name buffer) op)
              (return-from isOperator (Lexer-token buffer))
        )
    )
    (return-from  isOperator nil)
)

#||
    if that function finds the comment, return its token,
    otherwise return nil.
||#
(defun isComment(cm)
    (dolist (buffer Comments)
        (if(equal (Lexer-name buffer) cm)
              (return-from isComment (Lexer-token buffer))
        )
    )
    (return-from  isComment nil)
)

#||
    That function checks the number is digit or not.
||#
(defun isDigit (charCode)
      (if(and (< charCode 58) (> charCode 47))
          (return-from isDigit 1)
      )
      (return-from isDigit 0)
)

(defun isChar(charCode)
    (if(and (<= charCode 90) (>= charCode 65))
        (return-from isChar 1)
    )

    (if(and (<= charCode 122) (>= charCode 97))
        (return-from isChar 1)
    )
    (return-from isChar 0)
)

(defun isUnKnown(charCode)
    (if(and (<= charCode 47) (>= charCode 33))
        (return-from isUnKnown 1)
    )
    (if(and (<= charCode 64) (>= charCode 58))
        (return-from isUnKnown 1)
    )
    (if(and (<= charCode 96) (>= charCode 91))
        (return-from isUnKnown 1)
    )
    (if(and (<= charCode 126) (>= charCode 123))
        (return-from isUnKnown 1)
    )
    (return-from isUnKnown 0)
)

(defun resetLists()
    ;; if the identifier contains both numbers and characters then I'm looking at what it starts with.
    ;; error if it starts with a number, if it contains numbers, identifier
    (when(and (not(eq digitList nil)) (not(eq charList nil)))
          (when(= startWithDigit 1)
                (format t "ERROR:           Identifier can not start with digit.~%")
                (push "ERROR:           Identifier can not start with digit." resultList)
          )
          (when(= startWithDigit 0)
                (format t "IDENTIFIER~%")
                (push "IDENTIFIER" resultList)
          )
          (setq digitList nil)
          (setq charList nil)
          (setq startWithDigit 0)
    )
    ;; Returns an error if the first element of value is zero
    (when(not(eq digitList nil))
          (setq counter 0)
          (setq temp " ")
          (dolist (buffer (reverse digitList))
                (if(= counter 0)
                      (setq temp buffer)
                )
                (setq counter (+ counter 1))
          )
          (when(and (= temp 0) (/= counter 1))
                (format t "ERROR:          value can not be start with 0.~%")
                (push "ERROR:           value can not be start with 0." resultList)
          )
          (unless(and (= temp 0) (/= counter 1))
                (format t "VALUE~%")
                (push "VALUE" resultList)
          )
          (setq digitList nil)
    )
    (when(not(eq charList nil))
          ;;; identifier
          (when(equal (isKeyword (reverse charList)) nil)
                (push "IDENTIFIER" resultList)
                (format t "IDENTIFIER~%")
          )
          ;;;keyword
          (unless(equal (isKeyword (reverse charList)) nil)
                (push (isKeyword (reverse charList)) resultList)
                (format t "~a~&"(isKeyword (reverse charList)))
          )
          (setq charList nil)
    )
)

(defun readInput(line)
      (setq isCm 0)
      (loop for index from 0 to (- (length line) 1)
            do(
                   format t ""
                   (when(< index (- (length line) 1))
                          ;; comment or not control
                         (setq buffer (isComment(subseq line index (+ index 2))))
                         (unless(equal buffer nil)
                              (push buffer resultList)
                              (format t "~a~%" buffer)
                              (setq isCm 1)
                         )
                         ;; for ** operator
                         (setq buffer (isOperator (subseq line index (+ index 2))))
                         (unless(eq buffer nil)
                               (resetLists)
                               (push buffer resultList)
                               (format t "~a~%"buffer)
                               (setq index (+ index 2))
                         )
                   )

                   (if(= isCm 1)
                            (return)
                            ;;;if not the comment line
                            (progn
                                        (setq buffer (isOperator (subseq line index (+ index 1))))
                                        ;;; if it is operator
                                        (unless(eq buffer nil)
                                              (resetLists)
                                              (push buffer resultList)
                                              (format t "~a~%"buffer)
                                        )
                                        (when(= (isDigit (char-code (aref line index))) 1)
                                              (setq buffer (- (char-code (aref line index)) 48))
                                              (push buffer digitList)
                                        )
                                        (when(and (= (isUnKnown (char-code (aref line index))) 1) (eq buffer nil) )
                                                (resetLists)
                                                (format t "ERROR:         Unknown character~%")
                                                (push "ERROR:          Unknown character" resultList)
                                        )
                                        (when(= (isChar (char-code (aref line index))) 1)
                                              (if(and (not(eq digitList nil)) (eq charList nil))
                                                  (setq startWithDigit 1)
                                              )
                                              (setq buffer (aref line index))
                                              (push buffer charList)
                                        )
                                        (when(= (char-code (aref line index)) 32)
                                              (resetLists)
                                        )
                            )
                   )
            )
      )
      (resetLists)
)

(defun gppinterpreter( readFileName &optional writeFileName)
      (buildSyntax)
      (when(string= readFileName nil)
              (loop
                    (setq input (read-line))
                    (readInput input)
                    (writeToFile "parsed_lisp.txt")
                    (when (string= input "") (return ))
              )
      )
      (unless(string= readFileName nil)
          (readFile readFileName)
          (writeToFile "parsed_lisp.txt")
      )
)

(defun readFile(fileName)
        (let ((in (open fileName :if-does-not-exist nil)))
              (when in
                  (loop for line = (read-line in nil)
                        while line do
                              (readInput line)
                  )
                  (close in)
              )
        )
)

(defun writeToFile(fileName)
        (with-open-file (stream fileName :direction :output)
              (dolist (buffer (reverse resultList ) )
                  (format stream "~a~%"buffer)
              )
              (close stream)
        )
)

(defun startProgram()
      (setq fileName (nth 0 *args*))
      (gppinterpreter fileName)
)

(startProgram)
