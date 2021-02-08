
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

(defvar resultList nil) ; list of the results of output file.
(defvar parserLine nil) ; elements of line
(defvar listForRead nil)


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

(defun convertToNumber (num)
    (setq digit 0)
    (setq result 0)

    (dolist (buffer num)
        (setq result (+ result (* buffer (expt 10 digit))))
        (setq digit (+ digit 1))

    )
    (return-from convertToNumber result)
)

(defun resetLists()

    ;; if the identifier contains both numbers and characters then I'm looking at what it starts with.
    ;; error if it starts with a number, if it contains numbers, identifier
    (when(and (not(eq digitList nil)) (not(eq charList nil)))
          (when(= startWithDigit 1)
                (format t "ERROR:           Identifier can not start with digit.~%")
                (push "ERROR:           Identifier can not start with digit." resultList)
                (push "error" parserLine)
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
                (push "error" parserLine)
          )
          (unless(and (= temp 0) (/= counter 1))
                (format t "VALUE~%")
                (push "VALUE" resultList)
                (push (convertToNumber digitList) parserLine)
          )
          (setq digitList nil)
    )
    (when(not(eq charList nil))
          ;;; identifier
          (when(equal (isKeyword (reverse charList)) nil)
                (push "IDENTIFIER" resultList)
                (format t "IDENTIFIER~%")     
                (setq identifierStr (myConcat charList))
                (push identifierStr parserLine) 
          )
          ;;;keyword
          (unless(equal (isKeyword (reverse charList)) nil)
                (push (isKeyword (reverse charList)) resultList)
                (format t "~a~&"(isKeyword (reverse charList)))
                (push (isKeyword (reverse charList)) parserLine)
          )
          (setq charList nil)
    )
)

(defun myConcat(charList)
    (setq str "")
    (dolist (temp (reverse charList))
          (setq str (concatenate 'string str (string-downcase (read-from-string (string temp)))))
    )
    (format t "str:~a~%" str)
    (return-from myConcat str)
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
                              (push (subseq line index (+ index 2)) parserLine)
                              (format t "~a~%" buffer)
                              (setq isCm 1)
                         )
                         ;; for ** operator
                         (setq buffer (isOperator (subseq line index (+ index 2))))
                         (unless(eq buffer nil)
                               (resetLists)
                               (push (subseq line index (+ index 2)) parserLine)
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
                                              (push (subseq line index (+ index 1)) parserLine)
                                              (format t "~a~%"buffer)
                                        )
                                        (when(= (isDigit (char-code (aref line index))) 1)
                                              (setq buffer (- (char-code (aref line index)) 48))
                                              (push buffer digitList)
                                        )
                                        (when(and (= (isUnKnown (char-code (aref line index))) 1) (eq buffer nil) )
                                                (resetLists)
                                                (push "error" parserLine)
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
                    (evaluate)
                    (when (string= input "") (return ))
              )
      )
      (unless(string= readFileName nil)
          (readFile readFileName)
      )
)

(defun readFile(fileName)
        (let ((in (open fileName :if-does-not-exist nil)))
              (when in
                  (loop for line = (read-line in nil)
                        while line do
                            (readInput line)
                            (evaluate)
                            (write2)

                  )
                  (close in)
              )
        )
)
(defun write2()
  (with-open-file (stream "parsed_lisp.txt" :direction :output)
              (dolist (buffer (reverse listForRead) )
                  (format stream "~a~%"buffer)
              )
              (close stream)
        )
)

(defun writeToFile(fileName)
        (with-open-file (stream fileName :direction :output)
              (format stream "(")
              (dolist (buffer (reverse opElements) )
                  (format stream " ~a "buffer)
              )
              (format stream ")")
              (close stream)
        )
)

(defun isSyntaxOk(num)
  ;; resultList contains error -> 0;
  ;; resultlist does not contains error -> 1
  (defvar errorList nil)
  (setq error1 "ERROR:          Unknown character" )
  (setq error2 "ERROR:           Identifier can not start with digit." )
  (setq error3 "ERROR:           value can not be start with 0." )
  (push error3 errorList)
  (push error2 errorList)
  (push error1 errorList)

  (dolist (buffer (reverse resultlist))
      (dolist(buffer2 errorList)
          (if (equal buffer buffer2)
              (return-from isSyntaxOk 0)
          )
      )     
  )
  (return-from isSyntaxOk 1)
)

(setq flag 0)
(defvar elements nil)

; 44 = comma
; that function converts the elements of list to the string
(defun convert (myList)
    (when myList(concatenate 'string (write-to-string(car myList)) (string (code-char 44)) (convert (cdr myList)))
    )
)

(defun myConcatKeyword(myList)
    (when myList(concatenate 'string (write-to-string(car myList)) (string (code-char 32)) (myConcatKeyword (cdr myList)))
    )
)

(defun evaluate()
    (setq isAppend 0)
    (setq isAdd 0)
    (setq isMinus 0)
    (setq isMult 0)
    (setq isDiv 0)
    (setq isList 0)
    (setq isAnd 0)
    (setq isOr 0)
    (setq isEqual 0)
    (setq isLess 0)
    (setq isConcat 0)

    (setq result 0)
    (setq op "")
    (defvar opElements nil)
    (setq syntaxControl (isSyntaxOk buffer))
    ; If there is an error
    (when(= syntaxControl 0)
        (return-from evaluate 0)
    )
    ; If there is no error.
    (unless(= syntaxControl 0)
        (setq str (convert (reverse parserLine)))
        (dolist(buffer (reverse parserLine))
            (unless(equal 0 (keywordEval buffer))
                (when(equal (keywordEval buffer) "append")
                      (setq isAppend 1)
                      (setq str  (subseq  str (+ 11 (position #\K str))))
                      (format t "( ")
                )
                (when(equal (keywordEval buffer) "list")
                      (setq isList 1)
                      (setq str  (subseq  str (+ 9 (position #\K str))))
                )
                (when(equal (keywordEval buffer) "and")
                      (setq isAnd 1)
                      (setq str  (subseq  str (+ 8 (position #\K str))))
                )
                (when(equal (keywordEval buffer) "or")
                      (setq isOr 1)
                      (setq str  (subseq  str (+ 7 (position #\K str))))
                )
                (when(equal (keywordEval buffer) "equal")
                      (setq isEqual 1)
                      (setq str  (subseq  str (+ 10 (position #\K str))))
                )
                (when(equal (keywordEval buffer) "less")
                      (setq isLess 1)
                      (setq str  (subseq  str (+ 9 (position #\K str))))
                )
                (when(equal (keywordEval buffer) "concat")
                      (setq isConcat 1)
                      (setq str (subseq  str (+ 11 (position #\K str))))
                )
            )
            (when(and (not (equal (keywordEval buffer) "append"))(equal isAppend 1))
                (when(and (not(equal buffer "(")) (not(equal buffer ")")))
                    (format t "~a " buffer)
                )
            )
            (when(equal 0 (keywordEval buffer))
                (setq op (isOperator buffer))
                ;keyword ve op değilse.
                (when(eq op nil)
                    (push buffer opElements)
                )
                ; operationsa.
                (unless(eq op nil)
                    (when(string= op "OP_PLUS") (setq isAdd 1))
                    (when(string= op "OP_MINUS") (setq isMinus 1))
                    (when(string= op "OP_MULT") (setq isMult 1))
                    (when(string= op "OP_DIV") (setq isDiv 1))                 
                )
            )
                    
        )
        (if(equal isAdd 1)(setq result (helperEvalForOperator "OP_PLUS" opElements)) )
        (if(equal isMinus 1)(setq result (helperEvalForOperator "OP_MINUS" opElements)) )
        (if(equal isMult 1)(setq result (helperEvalForOperator "OP_MULT" opElements)) )
        (if(equal isDiv 1)(setq result (helperEvalForOperator "OP_DIV" opElements)) )
        (if(equal isList 1)(format t "~a~%" (reverse opElements)) (format t ")~%"))
        (if(equal isAnd 1)(setq result (helperEvalForKeyword "and" opElements)) )
        (if(equal isOr 1)(setq result (helperEvalForKeyword "or" opElements)) )       
        (if(equal isEqual 1)(setq result (helperEvalForKeyword "equal" opElements)) )       
        (if(equal isLess 1)(setq result (helperEvalForKeyword "less" opElements)) )       
        (if(equal isConcat 1)(setq result (helperEvalForKeyword "concat" opElements)) )       
    
        (unless(or (equal isList 1) (equal isAppend 1))
            (format t "result: ~a~%" result)
        )  
        (when(or (equal isList 1) (equal isAppend 1))
            (writeToFile "parsed_lisp.txt")
            (push (reverse opElements) listForRead)
        )        
    )  

    (setq opElements nil)
    (setq parserLine nil)
)

; It works for only 2 numbers
(defun helperEvalForOperator(op opElements)
    (with-open-file (stream "parsed_lisp.txt" :direction :output)
        (when(string= op "OP_PLUS")
            (setq num1 (car(reverse opElements)))
            (setq num2 (cadr(reverse opElements)))
            (setq result (+ num1 num2))
            (format stream "result: ~a~%" result)
            (push result listForRead)
            (close stream)
            (return-from helperEvalForOperator result)
        )

        (when(string= op "OP_MINUS")
            (setq num1 (car(reverse opElements)))
            (setq num2 (cadr(reverse opElements)))
            (setq result (- num1 num2))
            (push result listForRead)
            (format stream "result: ~a~%" result)
            (close stream)
            (return-from helperEvalForOperator result)
        )
        
        (when(string= op "OP_MULT")
            (setq num1 (car(reverse opElements)))
            (setq num2 (cadr(reverse opElements)))
            (setq result (* num1 num2))
            (push result listForRead)
            (format stream "result: ~a~%" result)
            (close stream)
            (return-from helperEvalForOperator result)
        )

        (when(string= op "OP_DIV")
            (setq num1 (car(reverse opElements)))
            (setq num2 (cadr(reverse opElements)))
            (setq result (/ num1 num2))
            (push result listForRead)
            (format stream "result: ~a~%" result)
            (close stream)
            (return-from helperEvalForOperator result)
        )
    )
)

(defun helperEvalForKeyword(key opElements)
    (with-open-file (stream "parsed_lisp.txt" :direction :output)
        (when(string= key "and")
            (setq val1 (car(reverse opElements)))
            (setq val2 (cadr(reverse opElements)))
            (setq result (and val1 val2))
            (push result listForRead)
            (format stream "result: ~a~%" result)
            (close stream)
            (return-from helperEvalForKeyword result)
        )
        (when(string= key "or")
            (setq val1 (car(reverse opElements)))
            (setq val2 (cadr(reverse opElements)))
            (setq result (or val1 val2))
            (push result listForRead)
            (format stream "result: ~a~%" result)
            (close stream)
            (return-from helperEvalForKeyword result)
        )
        (when(string= key "equal")
            (when(> (length opElements) 2)
                (format t "Error. statement should has 2 element.~%")
            )
            (unless(> (length elements) 2)
                (setq val1 (car(reverse opElements)))
                (setq val2 (cadr(reverse opElements)))
                (when(equal val1 val2)
                  (push result listForRead)
                  (format stream "true~%")
                  (close stream)
                  (return-from helperEvalForKeyword "true")
                )
                (unless(equal val1 val2)
                  (format stream "false~%")
                  (push result listForRead)
                  (close stream)
                  (return-from helperEvalForKeyword "false")
                )
            )              
        )
        (when(string= key "less")
            (when(> (length opElements) 2)
                (format t "Error. statement should has 2 element.~%")
            )
            (unless(> (length elements) 2)
                (setq val1 (car(reverse opElements)))
                (setq val2 (cadr(reverse opElements)))
                (when(< val1 val2)
                  (format stream "true~%")
                  (push result listForRead)
                  (close stream)
                  (return-from helperEvalForKeyword "true")
                )
                (unless(< val1 val2)
                  (format stream "false~%")
                  (push result listForRead)
                  (close stream)
                  (return-from helperEvalForKeyword "false")
                )
            )          
        )
        (when(string= key "concat")
            (setq val1 (car(reverse opElements)))
            (setq val2 (cadr(reverse opElements)))
            (setq result (concatenate 'string (write-to-string val1) (write-to-string val2)))
            (format stream "result: ~a~%" result)
            (push result listForRead)
            (close stream)
            (return-from helperEvalForKeyword result)
        )
    )

)

(defun keywordEval(key)
    (when(equal key "KW_AND")               ;done
        (return-from keywordEval "and")
    )(when(equal key "KW_OR")               ;done
        (return-from keywordEval "or")
    )(when(equal key "KW_NOT")
        (return-from keywordEval "not")
    )(when(equal key "KW_EQUAL") 
        (return-from keywordEval "equal")
    )(when(equal key "KW_LESS") 
        (return-from keywordEval "less")
    )(when(equal key "KW_NIL")
        (return-from keywordEval "nil")
    )(when(equal key "KW_LIST")             ;done
        (return-from keywordEval "list")
    )(when(equal key "KW_APPEND")
        (return-from keywordEval "append")  ;done
    )(when(equal key "KW_CONCAT")
        (return-from keywordEval "concat")
    )(when(equal key "KW_SET")
        (return-from keywordEval "set")
    )(when(equal key "KW_DEFFUN")
        (return-from keywordEval "deffun")
    )(when(equal key "KW_FOR")
        (return-from keywordEval "for")
    )(when(equal key "KW_IF")
        (return-from keywordEval "if")
    )(when(equal key "KW_EXIT")
        (return-from keywordEval "exit")
    )(when(equal key "KW_LOAD")
        (return-from keywordEval "load")
    )(when(equal key "KW_DISP")
        (return-from keywordEval "disp")
    )(when(equal key "KW_TRUE")
        (return-from keywordEval "true")
    )(when(equal key "KW_FALSE")
        (return-from keywordEval "false")
    ) 
    (return-from keywordEval 0)
)

(defun startProgram()
      (setq fileName (nth 0 *args*))
      (gppinterpreter fileName)
)

(startProgram)



