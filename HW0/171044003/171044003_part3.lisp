

(defparameter numbers (make-array 5)) ; array that holds numbers
(setq counter 0) ; Number of elements in array

(defvar digitList nil)
(defvar resultList nil)

#||
    With this function, we put the returned element in the first empty place of array.
||#
(defun fillNumberArray (num)
    (setf (aref numbers counter) num)
    (setq counter (+ counter 1))
    #||
    (format t "counter: ~a~%" counter)
    (dotimes (n counter)
        (format t "~a th element of list = ~a ~%" n (aref numbers n))
    )
    ||#

)

#||
    In the read section, I read the characters up to the blank one by one and added a list.
    In this function, I multiply the elements in that list by their place values and find their real values and return.
||#
(defun convertToNumber (num)
    (setq digit 0)
    (setq result 0)

    (dolist (buffer num)
        (setq result (+ result (* buffer (expt 10 digit))))
        (setq digit (+ digit 1))

    )
    ;;;(format t "result:  ~a ~%" result)
    (return-from convertToNumber result)
)

#||
    I'm reading the file character by character. Every time I get into the space I put it in the digitList.
    This shows the numbers for my number. In the question, I find the number value of it by assigning it to the convertToNumber function
    and filling the array with the fillNumberArray function. In this way, I add the numbers written in the file to the array.
    IMPORTANT NOTE:
    There is no space at the end of the integer_input.txt file you submit. so I advanced my algorithm accordingly.
     When it sees a space, it goes to the other number. So please don't enter a txt with a space at the end.
||#
(defun readNumbers (fileName)
    (with-open-file (stream fileName)
        (do (
                (char (read-char stream nil)
                     (read-char stream nil)
                )

            )
            ((null char))

            (when (eq (digit-char-p char) nil)
                (when(< counter 5)
                    (fillNumberArray (convertToNumber digitList))
                    (setq digitList nil)
                )

            )
            (unless (eq (digit-char-p char) nil)
                  (push (digit-char-p char) digitList)
             )
        )
    )
)


#||
    Recursive was more logical and easy so I did Recursive, but I couldn't write because it was recursive.
    So I put them in the resultList and sent them to the write function to print them.
||#
(defun collatzSequenceRecursive (num index)
      (cond
            ((= num 1)
                    (push num resultList)
                    (return-from collatzSequenceRecursive 0)
            )
            ((evenp num)
                    (push num resultList)
                    (collatzSequenceRecursive (/ num 2) index)
            )
            ((oddp num)
                    (push num resultList)
                    (collatzSequenceRecursive (+ (* 3 num) 1) index)
            )
    )
)

#||
    With this function I read the elements from the array and send them to the collatzSequenceRecursive function.
    there these numbers are written again according to this sequence and put into resultList.
    Then I print the items in this list to file.
||#
(defun writeToFile(fileName)

    (with-open-file (stream fileName :direction :output)
        (dotimes (index counter)
              (format stream "~a : " (aref numbers index))
              (collatzSequenceRecursive (aref numbers index) index)

              (dolist (buffer (reverse resultList))
                  (format stream "~a "buffer)
              )
              (terpri stream)
              (setq resultList nil)
          )
          (close stream)
    )
)

(readNumbers "integer_inputs.txt")

(writeToFile "collatz_outputs.txt")
