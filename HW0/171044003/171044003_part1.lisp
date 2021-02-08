
(setq digitList nil) ; I keep the digits
(setq numList nil) ; I keep the numbers

#||
   First of all, I am evaluating whether the character I read is number, space, "(" and ")".
   In this case, I put the character I read directly to the numlist.
   If the character I read is digit, I convert the number from ascii value to number value and push it to digitList.
   If the character I read is a space and the digitlist is not nil, so if I put a number in the digitlist before;
   I combine the characters and convert them to numbers and put them in the numList.
   Then I make the digitlist nil in case you can create a new number.
   When all the conditions are over, if the digitlist is still full, I convert it to a number and put it in numList.
   Thus, the reading process ends.
||#
(defun readFromFile (fileName)
      (let ((in (open fileName :if-does-not-exist nil)))
            (when in
                (loop for line = (read-line in nil)
                      while line do
                        (loop for index from 0 to (- (length line) 1)
                                do(
                                       format t ""
                                       (cond
                                            ;;; if they are not numbers and spaces
                                            ((and (= (isDigit (char-code (aref line index))) 0) (/= (char-code (aref line index)) 32))
                                                  ;;; "(" ve ")" değilse
                                                  (if(and  (/= (char-code (aref line index)) 40)  (/= (char-code (aref line index)) 41))
                                                        (push (aref line index) numList)
                                                  )
                                            )
                                            ;;; if it is a number
                                            ((= (isDigit (char-code (aref line index))) 1)
                                                  (setq buffer (- (char-code (aref line index)) 48))
                                                  (push buffer digitList)
                                            )
                                            ;;; if a space and the digitlist is not nil
                                            ((and (= (char-code (aref line index)) 32) (not(eq digitList nil)) )
                                                  (push (convertToNumber digitList) numList)
                                                  (setq digitList nil)
                                            )
                                       )
                                )
                          )
                          ;;; I turn the remaining digits into numbers in the last digitlist and finish the process.
                          (when(not(eq digitList nil))
                              (push (convertToNumber digitList) numList)
                              (setq digitList nil)
                          )
                )
                (close in)
            )
      )
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

#||
    that function convert the digits to number
||#
(defun convertToNumber (num)
    (setq digit 0)
    (setq result 0)

    (dolist (buffer num)
        (setq result (+ result (* buffer (expt 10 digit))))
        (setq digit (+ digit 1))

    )
    (return-from convertToNumber result)
)

#||
    I'm writing the numbers in the numlist to the file.
||#
(defun writeToFile (fileName)
    (with-open-file (stream fileName :direction :output)
        (format stream "( ")

        (dolist (buffer (reverse numList))
              (format stream "~a " buffer)
        )
        (format stream ")")
    )
)

(readFromFile "nested_list.txt")
(writeToFile "flattened_list.txt")
