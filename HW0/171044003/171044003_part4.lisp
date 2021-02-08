
(defparameter letterArray (make-array '(94 2))) ; first part ascii code, second part frequency
(setq lengthOfArray 0)  ; length of letterArray

(defparameter sortedLetterArray (make-array '(94 2))) ; my sorted letter array

(defparameter codeResult (make-array '(94 2))) ; array which has codes

(setq lengthOfCodeResultArray 0) ; length of codeResult array

(defparameter sortedCodeResult (make-array '(94 2))) ; my sorted codeResult array.

(defstruct Node
      name
      frequency
      leftNode
      rightNode
)

(setq root (make-Node))

(defvar HuffmanNodes nil) ; List of Huffman Tree's nodes

#||
    I read the file sent as parameter in this function. I throw every letter I see into the letterArray.
    If it has it, I increase its frequency. This means that I am increasing the second parameter.
||#
(defun readParagraph (fileName)
      (let ((in (open "paragraph.txt" :if-does-not-exist nil)))
            (when in
                (loop for line = (read-line in nil)
                      while line do
                          (loop for index from 0 to (- (length line) 1)
                                do(
                                       format t ""
                                       ;;; eğer array'imde yoksa arraye ekliyorum.
                                       (when(= (isInTheArray (char-code (aref line index))) 0)
                                            (setf (aref letterArray lengthOfArray 0) (char-code (aref line index)))
                                            (setf (aref letterArray lengthOfArray 1) 0)
                                            (setq lengthOfArray (+ lengthOfArray 1))
                                       )
                                       ;;; eğer array'imde varsa frekansı arttıroyrum.
                                      (unless(= (isInTheArray (char-code (aref line index))) 0)
                                            (increaseFrequency (char-code (aref line index)))
                                      )
                                )
                          )
                )
                (close in)
            )
      )
)

#||
    This function scans the name sent with the parameter in the letterArray and increases its frequency.
||#
(defun increaseFrequency (name)
      (dotimes (x lengthOfArray)
            (when(eq (aref letterArray x 0) name)
                  (setf (aref letterArray x 1) (+ (aref letterArray x 1) 1))
            )
      )
)

#||
    This function checks whether the value sent with the parameter is in the letterArray or not.
||#
(defun isInTheArray (asciiValue)
    (dotimes (x lengthOfArray)
        (when(eq (aref letterArray x 0) asciiValue)
            (return-from isInTheArray 1)
        )
    )
    (return-from isInTheArray 0)
)

#||
    This function prints my letterArrays.
||#
(defun printArray (letterArray)
      (setq a 0)
      (dotimes (x lengthOfArray)
            (format t "~a : ~a ~%"  (code-char (aref letterArray x 0)) (aref letterArray x 1) )
            (setq a (+ a (aref letterArray x 1)))
      )
      (terpri)
      (format t "total character number from printArray function: ~a ~%" a)
  )

#||
    This function copies the array.
||#
(defun copyArray (arr)
    (dotimes (x lengthOfArray)
          (setf (aref arr x 0) (aref letterArray x 0))
          (setf (aref arr x 1) (aref letterArray x 1))
    )
)

#||
    This function sorts my letterArray by frequency.
||#
(defun mysortForFrequency (arr)
      (dotimes (x 40)
            (setq positionMax x)
            (loop for y from (+ x 1) to 40
                  do (
                      format t ""
                      (if(> (aref arr y 1) (aref arr positionMax 1) )
                            (setq positionMax y)
                      )
                  )
            )
            (setq bufferName (aref arr x 0))
            (setq bufferFreq (aref arr x 1))

            (setf (aref arr x 0) (aref arr positionMax 0))
            (setf (aref arr x 1) (aref arr positionMax 1))

            (setf (aref arr positionMax 0) bufferName)
            (setf (aref arr positionMax 1) bufferFreq)
      )
)

#||
    This function prints my huffman tree.
||#
(defun printList (list)
      (dolist (buffer list)
            (format t "~a ~%"buffer)
      )
)

#||
    This function pulls the two smallest elements of the huffman tree and merges them back into the tree.
||#
(defun helperMethod (a)
          (setq x (pop HuffmanNodes))
          (setq y (pop HuffmanNodes))

          (setq freq (+ (Node-frequency x) (Node-frequency y)))
          (setq name freq)

          (setq newNode (make-Node
                    :name name
                    :frequency freq
                    :leftNode x
                    :rightNode y
                    )
          )

          (setq root newNode)

          (setq list1 nil)
          (setq list2 nil)
          (dolist (buffer HuffmanNodes)
                (when(>= freq (Node-frequency buffer))
                      (push buffer list1)
                )
                (when(< freq (Node-frequency buffer))
                      (push buffer list2)
                )
          )

          (setq HuffmanNodes nil)

          (dolist (buffer list2)
                (push buffer HuffmanNodes)
          )
          (push newNode HuffmanNodes)

          (dolist (buffer list1)
                (push buffer HuffmanNodes)
          )
)

#||
    This function prints the code that comes out according to the huffman tree and puts them in my codeResult array.
||#
(defun printCode (root str)
    (if(and (eq (Node-leftNode root) nil) (eq (Node-rightNode root) nil))
          (when(not (numberp (Node-name root)))
                ;;;(format t "CODE ~a : ~a~%" (Node-name root) str)
                (setf (aref codeResult lengthOfCodeResultArray 0) (Node-name root))
                (setf (aref codeResult lengthOfCodeResultArray 1) str)
                (setq lengthOfCodeResultArray (+ lengthOfCodeResultArray 1))

                (return-from printCode 0)
          )
    )
    (printCode (Node-leftNode root) (concatenate 'string str "0"))
    (printCode (Node-rightNode root) (concatenate 'string str "1"))
)

#||
    This function reads the elements from my array and creates a node structure for each.
    He puts them on the huffmanNodes list. Then helperMethod is called to create a huffman tree.
||#
(defun buildHuffmanTree(lengthOfArray)
      (dotimes (n lengthOfArray)
            (setq bufferStruct (make-Node
                      :name (code-char(aref sortedLetterArray n 0))
                      :frequency (aref sortedLetterArray n 1)
                      :leftNode nil
                      :rightNode nil
                      )
            )
            (push bufferStruct HuffmanNodes)
            (setq bufferStruct nil)
      )
      (dotimes(n (- lengthOfArray 1))
          (helperMethod 0)
      )
)

#||
    This function reads from my sortedCodeResult array and writes it to the file.
||#
(defun writeToFile (fileName)
    (with-open-file (stream fileName :direction :output)
          (dotimes (x lengthOfCodeResultArray)
                (format stream "~a : ~a ~%"  (aref sortedCodeResult x 0) (aref sortedCodeResult x 1))
          )
          (close stream)
    )
)

#||
    Bu fonksiyonda kodları uzunluklarına göre sıralıyorum.
    Uzunluğu bir olandan uzunluğu n olana kadar bulup sortedCodeResult array'ime atıyorum.
||#
(defun sortCodeResult (n)
    (setq counterIndex 0)
    (dotimes(y n)
        (dotimes (x lengthOfCodeResultArray)
              ;;;(format t "~a : ~a ~%"  (aref codeResult x 0) (aref codeResult x 1))
              (when(= (length (aref codeResult x 1) ) y)
                  (setf (aref sortedCodeResult counterIndex 0) (aref codeResult x 0))
                  (setf (aref sortedCodeResult counterIndex 1) (aref codeResult x 1))
                  (setq counterIndex (+ counterIndex 1))
              )
        )
    )
)

#||
    In this function, I sort the codes according to their length.
    I find it from one of length to n of length and add it to my sortedCodeResult array.
||#
(defun printCodeArray (lengthOfCodeResultArray)
      (dotimes (x lengthOfCodeResultArray)
            (format t "~a : ~a ~%"  (aref sortedCodeResult x 0) (aref sortedCodeResult x 1))
      )
)

(readParagraph "paragraph.txt")
(copyArray sortedLetterArray)
(mysortForFrequency sortedLetterArray)
(buildHuffmanTree lengthOfArray)
(printCode root "")

;;; I sent 15 to check the maximum number of digits in the code.
(sortCodeResult 15)

;;;(printCodeArray lengthOfCodeResultArray)

(writeToFile "huffman_codes.txt")
