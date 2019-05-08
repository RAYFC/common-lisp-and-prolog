;CMPUT 325 LEC B01 (LAB H04)ASSIGNMENT 1 
;Ruifeng Chen
;1473869


#| Question 1.
returns T if a is a member of b
otherwise returns NIL

the algorithm is easy:base case: 1.check if list b is null(if it is a won;t be in the list)
2.check if a equals the first element
3.run recursion
|#
(defun xmember (a b)
	(if (null b)
		NIl
		(if (equal a (car b))
			T
			(xmember a (cdr b))
	)
	)
)
#| Question 2.
It flattens all elements in a list
testcase:
>(flatten '(a (b c) d))
(a b c d)

>(flatten '((((a))))) 
(a)

>(flatten '(a (b c) (d ((e)) f)))
(a b c d e f)
|#


(defun flatten (L)
    (if (null L)
        nil
        (if (atom (car L))
            (cons (car L) (flatten (cdr L)))
            (append (flatten (car L)) (flatten (cdr L))))))
            
#| Question 3.  
as the function name says, remove all duplicates
Example.

(remove-duplicate '(a b c a d b)) ==>
(a b c d)
or

(c a d b)  
|#      
(defun remove-duplicate (L)
	(if (null L)
		nil
		(if (xmember (car L) (cdr L))
		(remove-duplicate (cdr L))
		(cons (car L)(remove-duplicate (cdr L)))
		)
	)
)

#|QUESTION4
mixes the elements of L1 and L2 into a single list, 
 by choosing elements from L1 and L2 alternatingly  
 
 >(mix '(a b c) '(d e f))
(a d b e c f)
The algorithm is check if L1 L2 are empty. if yes, just write the other list.
if not,cons the first element to the answer.
the recursion should shift the 2 list so that we can choose elements from L1 and L2 alternatingly 
|#
(defun mix (L1 L2)
	(cond
		((null L1) L2)
		((null L2) L1)
		(t (cons (car L1) (mix L2 (cdr L1))))
	)
)

#|QUESTION 5
Find all subsets of a set
>(allsubsets '(a b))
>(nil (a) (b) (a b))
|#

(defun allsubsets (L)
  (if (null L)
      '(nil)
      (add (car L) (allsubsets (cdr L)))
  )
)
;the add function is to pair a single element to all existed subsets.
;also, after getting all new subsets,we should also add the  existed subsets.    
;for the add function, I got some inspiration from stack-overflow(There is a quite similar func,but I didn't copy anything fron that website) 
(defun add (x y)
  (if (null y)
      nil
      (cons (car y)
            (cons (cons x (car y))
                  (add x (cdr y))
                  )
       )
  )
)


      
#|QUESTION 6
This is the part A of question 6
A web page A containing a link to another one B is represented by a pair, (A  B). 
Give a list L of such pairs, write two Lisp functions:
(reached x L) 
where x is a web page, L is a list of pairs representing linkage,
and the function returns a list of all web pages that can be reached from x (x should not be part of the result). 
The order of the web pages in the resulting list is unimportant.
A: (reached 'a '((b b))) ==> NIL
B: (reached 'a '((b a))) ==> NIL
C: (reached 'a '((a b)(a c)(a d))) ==> (b c d)
D: (reached 'a '((c d)(d a)(a b)(b c))) ==> (b c d)
E: (reached 'a '((e x)(b x)(x e)(a a)(a b))) ==> (b e x) 
|#


(defun reached (x L)

	(if(null L) 
		nil
		(remove x (r-reached (list x) L))
	)
)

;in this function, if the elments needs searching is not undated,the recursion should end.
;otherwise,update the list of elements and reach them again.
;also, we have to sort the list in a specific order so that we can compare.
(defun r-reached (lx L)

	(if(equal (sort (remove-duplicates (reach_first lx L)) 'string-lessp) lx) 
		lx
		(r-reached (sort (remove-duplicates (reach_first lx L)) 'string-lessp) L)
	)

)
;This function is for reaching a list of elements
;idea:call simple_reach many times
(defun reach_first (lx L)
	(if(null (car lx)) 
		nil
		(append (simple_reach(car lx) L) lx (reach_first(cdr lx) L))
	)
)

;This is for reaching a single element

(defun simple_reach (x y)
	(if (null y)
		NIL
		(if (eq x (caar y))
			(if (eq x (cadar y))
			(simple_reach x (cdr y))
			(append (cdar y) (simple_reach x (cdr y)))
			)
		(simple_reach x (cdr y))
		)
	)
)

#|
This is the part b of question 6
rank S L
where S is a list of atoms naming web pages, and L is a list of pairs representing linkage. 
The function returns a permutation of S such that the web pages are ordered according to the criterion above,
i.e., the most referred web page is the first in the list, and so on. 
If two web pages are equally important in terms of references, then it doesn't matter how they are ordered.	

F: (rank '(a b) '((a a)(b b)(a b)(c a))) ==> (a b) OR (b a)
G: (rank '(a b c) '((a b)(a c)(a d))) ==> (b c a) OR (c b a)
H: (rank '(a b c d) '((d c)(a c)(b d)(c c)(a a)(d b))) ==> (c b d a) OR (c d b a)
I: (rank '(b a k) '((k b)(b k)(a k)(k a))) ==> (k a b) OR (k b a)
J: (rank '(m n) '((n m)(m m)(t n)(s t)(t s))) ==> (m n) OR (n m)

This idea is that I can reverse each pair in lists.i.e ((a b) (d c)) ==>((b a) (c d))
Then I can run simple_search in 6(A)
Define a count function called num to get how many webs connected to the specific web
Finally, use the sort function given in eclass
|#
(defun rank(L1 L2)
	(list_out(get_order L1 L2))
)
(defun pair_reverse (L)
	(if (null L)
		nil
		(append (cons (cons (cadar L)(cons (caar L) NIL))NIL) (pair_reverse(cdr L)))
	)	
)

(defun num (L)
	(if (null L)
		0
		(+ 1 (num (cdr L)))
	)
)

(defun multi_reach (L1 L2)
	(if (null L1)
		nil
		(append (cons (cons (car L1) (simple_reach (car L1) L2))NIL) (multi_reach (cdr L1) L2))
	)
)
;it returns a list of webs refered.
(defun rank_count(L1 L2)
	(if (null L1)
		nil
		(append (cons (cons (caar(multi_reach L1 L2)) (cons (num(cdar (multi_reach L1 L2)))nil))nil) (rank_count(cdr L1) L2) )
	)
)

;The 2 functions are from eclass
(defun mySort (L)
       (sort L 'greaterThan)
)

(defun greaterThan (L1 L2)
      (> (cadr L1) (cadr L2))
)
;The last 2 functions are for printing out the results in format.
(defun get_order(L1 L2)
	(mysort (rank_count L1 (pair_reverse L2)))
)
(defun list_out (L)
	(if (null L)
		nil
		(append(cons (caar L) NIL) (list_out (cdr L)))
	)
)