
    (defvar num1 nil)
    (defvar num2 nil)
    (setq counter 1)

    #||
      My main prime function
    ||#
    (defun isPrimeRecursive (number divisor)
            (cond
                  ((not(integerp number)) ; girilen sayının integer olup olmadığını kontrol ettim.
                      (format t "~a is not integer number" number)
                  )

                  ((= divisor 1) ; bölen sayı birse çıkıyor.
                      1
                  )

                  ((< number 2) ; sayı 2'den küçükse 0
                      0
                  )

                  ((= (mod number 2) 0 ) ; sayının modu 2 ise 0
                      0
                  )

                  ((= (mod number divisor) 0 )
                      0
                  )
                  ((/= (mod number divisor) 0 )
                      (isPrimeRecursive number (- divisor 1))
                  )
            )
    )

    #||
        In this function I set number and divisor and send to my main function.
    ||#
    (defun isPrime (number)
        (setq divisor  (floor (/ number 2)))
        (setq result (isPrimeRecursive number divisor))

        (cond
            ((= result 1)
              1
            )
            ((/= result 1)
              0
            )
        )
    )

    #||
        That method checks the number is semiprime or not.
    ||#
    (defun isSemiPrime (number)

        (setq counter 0) ; the number of prime numbers divided by
        (setq index 2)

        (loop while (and (< counter 2) (< index number)) do
            (when(= (isPrime index) 1)
              (loop while(= (mod number index) 0) do
                  (setq number (/ number index))
                  (setq counter (+ counter 1))
              )
            )
            (setq index (+ index 1))
        )
        (if (> number 1)
            (setq counter (+ counter 1))
        )
        (cond
              ((= counter 2)
                1
              )
              ((/= counter 2)
                0
              )
        )
    )

    #||
        That method converts the digits to number
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
        This function checks whether the numbers between borders are semiprime or prime and writes it to the file.
    ||#
    (defun searchBetweenBorders (border1 border2)
        (with-open-file (stream "primedistribution.txt" :direction :output)
            (dotimes (n (+ 1 (- border2 border1)))
                (cond
                    ((= (isPrime (+ n border1)) 1)
                        (format stream "~a is Prime number~%" (+ n border1))

                    )
                    ((= (isSemiPrime (+ n border1)) 1)
                        (format stream "~a is Semi-prime number~%" (+ n border1))
                    )
                )
            )
        )
    )

    #||
        reading character character. If the character I read is nil, it means the number is over and there is a space.
        I convert all the characters until I see the first space to numbers and throw a list number 1.
        I put the ones after the blank in the number 2 list then I find their real values with the convertToNumber function.
    ||#
    (defun readFromFile (fileName)
        (with-open-file (stream fileName)
            (do (
                    (char (read-char stream nil)
                         (read-char stream nil)
                    )
                )
                ((null char))

                (when (eq (digit-char-p char) nil)
                      (setq counter 2)
                )
                (unless (eq (digit-char-p char) nil)
                  (cond
                        ((= counter 1)
                             (push (digit-char-p char) num1)
                         )
                         ((= counter 2)
                             (push (digit-char-p char) num2)
                         )
                  )
                )
            )
            (searchBetweenBorders (convertToNumber num1)  (convertToNumber num2))
        )
    )

    (readFromFile "boundries.txt" )
