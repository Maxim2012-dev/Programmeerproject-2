#lang r6rs

(import (rnrs base)
        (rnrs io simple)
        (a-d sorting internal comparative bubble-sort))

(vector (vector 1 "One More Time- Aerodynamic" "8:03" "Daft Punk")
        (vector 2 "Face to Face- Harder Better Faster Stronger" "4:55" "Daft Punk")	
        (vector 3 "Too Long" "5:10"	"Daft Punk ")
        (vector 4 "Around the World- Harder Better Faster Stronger" "7:27" "Daft Punk")	
        (vector 5 "Steam Machine" "1:39" "Daft Punk")
        (vector 6 "Crescendolls-Too Long- High Life" "7:41" "Daft Punk")	
        (vector 7 "Television Rules the Nation" "2:47" "Daft Punk")	
        (vector 8 "Technologic" "5:29" "Daft Punk ")
        (vector 9 "Superheroes- Human After All" "6:13" "Daft Punk")
        (vector 10 "Da Funk" "5:59" "Daft Punk")
        (vector 11 "The Brainwasher- The Primetime of Your Life- Steam Machine" "12:37" "Daft Punk")
        (vector 12 "Robot Rock-Oh Yeah" "6:36" "Daft Punk"))

(vector (vector 1 2 3 4 5 6 7 8 9 10 11 12)
        (vector "One More Time- Aerodynamic" "Face to Face- Harder Better Faster Stronger"
                "Too Long" "Around the World- Harder Better Faster Stronger"
                "Steam Machine" "Crescendolls-Too Long- High Life" "Television Rules the Nation"	
                "Technologic" "Superheroes- Human After All" "Da Funk"
                "The Brainwasher- The Primetime of Your Life- Steam Machine" "Robot Rock-Oh Yeah")
        (vector "8:03" "4:55" "5:10" "7:27" "1:39" "7:41" "2:47" "5:29" "6:13" "5:59" "12:37" "6:36")
        (vector "Daft Punk" "Daft Punk" "Daft Punk" "Daft Punk" "Daft Punk" "Daft Punk"
                "Daft Punk" "Daft Punk" "Daft Punk" "Daft Punk" "Daft Punk" "Daft Punk"))

(define persons (vector (cons "Paul" 41) (cons "Anna-Mae" 44) (cons "Selma" 23) 
                        (cons "Kenny" 68) (cons "Roger" 41) (cons "Paul" 22)))

(sort persons (lambda (p1 p2) (string<? (car p1) (car p2))))
(display persons)(newline)
(sort persons (lambda (p1 p2) (< (cdr p1) (cdr p2))))
(display persons)(newline)

(vector 5 7 2 3 8 10 9 12 4 6 1 11)