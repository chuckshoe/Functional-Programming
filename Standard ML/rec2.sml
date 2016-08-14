(*to use a file: use "filename.sml"; *)
(* Defining functions *)
(* Use of fun keyword: fun <identifier> (<parameter list>) = expression; *)
fun square (x) =x*x;
val f=fn x => x* x
(* There are many types in ML:
Integers (Same as other languages except for the negative numbers. Negative numbers are represented with ~)
Reals
Boolean (true or false) [ML is case sensitve. So True is not the same as true]
String (Example: "Hello World")
Characters (# followed by a character string of length 1. E.q. #"a")*)

(* Setting the type of the arguments*)
fun square(x:real)=x*x;

(* Setting the type of the return value*)
fun square (x):real=x*x;

(* Variable declaration
	val <identifier>=<value>
*)

val x=3;

(*Function type: fn <domain type> -> <range type> *)

(* Function with more than 1 parameter*)
(* Effectively it is tuple *)
fun max2(a,b)= if a > b then a else b;
fun max2 ((op >),a,b)= if a > b then a else b;
fun max2 (op >)(a,b)= if a > b then a else b;
fun max2 (op >) a b = if a > b then a else b;
fun max2 (op >)(a,b)= a > b;

fn (L1,L2) => if length L1 < length L2 then  L1 else L2
fun comp2Lists (op <) L1 L2=L1 < L2;
val rec col= fn (x::xs,nil) => false | 
				(nil,y::ys) => false |
				(nil,nil) => true |
				(x::xs,y::ys) => if x < y then col(xs,ys) else false;
comp2Lists col [1,2] [2,3]
comp2Lists (op <) 2 3

(* Patterns in function definition *)
(* fun <identifier> (< first pattern>) = <first expression>
	| <identifier> (< second pattern>) = <second expression>
	| ..
	| <identifier> (< last pattern>) = <last expression>;
	
	Identifiers must all be the same and the types of the values produced by the expressions on the right of the equal signs must all be the same.
	Types of the patterns must all be the same but they can differ from the type of the values produced.
*)

fun reverse (nil)=nil
| reverse (x::xs) =reverse (xs) @ [x]

fun merge (nil, x::xs)=x::xs
| merge (y::ys, nil)=y::ys
| merge (x::xs, y::ys)=
if x < y then x::merge(xs,y::ys) else y::merge(x::xs,ys);

fun merge (nil, M)=M
| merge (L, nil)=L
| merge (L as x::xs, M as y::ys)=
if x < y then x::merge(xs,M) else y::merge(L,ys);



fun sumLists(nil) =0
| sumLists(nil::YS) = sumLists (YS)
|sumLists((x::xs)::YS)=x+sumLists(xs::YS);

fun length(nil)=0
| length(x::xs)=1+length(xs);

(* Anonymous variables: The symbol _ can be used in patterns to stand for anonymous or wildcard variable, which is a variable whose name we do not know and do not need to know*)
fun check (x,_,z) = x+z;


(* Local environment using let*)
(* let val <first variable>= <first expression>;
	val <second variable>= <second expression>;
	..
	val <last variable>= <last expression>;
	in
		<expression>
	end;
	
	Expression following in may use the variables defined after let. 
	DO NOT FORGET to use the keywords val.
	Semicolons following the declarations are optional
	Do not omit keywords in and end
*)
fun f a b =   
	let val x = a + b
		in x * x
     end;

	 
fun f2 a b =   
	let val x = a + 1
		val y = b + 2
		val z= x * y
		in z * z
     end;
 
 
fun heronsformula (a, b, c) =
let val s = (a + b + c) / 2.0
	in
		Math.sqrt(s*(s-a)*(s-b)*(s-c))
	end;
(* Let also enables us to have nested functions*)
fun lfib n=
let 
	fun fib 0 x y=x
	| fib 1 x y=y
	| fib n x y = fib (n-1) y (x+y) 
in
 fib n 0 1 
end;


	
(*Anonymous functions*)
fn x => x+1
fn x => if x >= 0.0 then x else ~x;

(* 'a is a valid identifier but it cannot be assigned to values like 3, 4.0, "hi". It can be applied only be bound to a type. Usually ML chooses the identifier 'a to represent the type of something whose value can be of any type. *)

(* Example of type inference *)
fun foo (a,b,c,d) = if a=b then c+1 else if a > b then c else b+d;

fun compose f g x = f (g x);

fun swap(x, y) = (y,x);
fun swap x y = (y,x);

fun separate (sep,nil) = nil 
| separate(sep,[x]) = [x]
| separate (sep,x::xs) = x::sep::(separate (sep,xs));

fun identity (x)=x;

(*Curried functions*)
(* Function name is followed by the list of its parameters with no paranthesis or commas*)
(* Example of uncurried form of function *)
fun exponent1(x,0) = 1.0
| exponent1 (x,y) = x*exponent1(x,y-1);

fun exponent2 x 0 = 1.0
| exponent2 x y = x * exponent2 x (y-1);

(* operator -> associates from right *)
(* real -> int -> real implies a function that takes real as an argument and returns a function from integers to reals *)

 fun add3 x = fn y => fn z => x + y + z;
 fun add3 x y z = x + y + z;
 fun add3 (x,y,z) = x + y + z;

(* Example of 2 functions curried and nested functions in let*) 
fun compose F G=
let 
	fun C x=G(F(x))
	in 
		C
	end;

fun F x=x+3;
fun G y=y*y+2*y;
val H=comp F G;
H 10;

(*fun max2 (op >)(a,b)= if a > b then a else b;*)
fun member compare (k, [])= false
	| member compare (k,x::y) = if  compare (x,k) then 
				true 
			    else
				member compare (k,y);

fun insert compare (item,lis)= if member compare(item,lis)=false then
							 item::lis 
						else
							 lis;
fun remove compare (k,[])=[]
| remove compare (k,x::y)= if compare (k,x) then
		              y
		  		  else
		              x::remove compare (k,y);

fun intersect compare ([],y)=[]
| intersect compare (p::q,y) =if member compare (p,y) then
			    			p::intersect compare (q,y)
					  else
			    			intersect compare (q,y);

fun comp(x,y)= if x=y then true else false;		

(* foldr ML's verion of reduce operator
foldr definition:
fun foldr _ b [] = b
| foldr f b (x::xs) = f(x, foldr f b xs)
*)
foldr (op +) 0 [1,2,3];

fun ins (n, []) = [n]
    | ins (n, ns as h::t) = if (n<h) then n::ns else h::(ins (n, t));
val insertionSort = foldr ins []
    						            
(*Function to delete the ith element in a list*)
fun deleteielement [] i =[]
|deleteielement (x::xs) 1=xs
|deleteielement L i=if length(L) < i
then L
else let val (x::xs)=L 
in 
x::(deleteielement (xs) (i-1)) 					
end;


(* insert for insertion sort polymorphic function *)
fun insert (op <) [] x=[x]
| insert (op <) (y::ys) x= if y < x then y::(insert (op <) ys x)
else 
(x::y::ys);

 fun insertionSort []=[]
 | insertionSort (x::xs)= 
 let val L =[x]
 fun iterate S []=S
 | iterate S (y::ys) =iterate (insert (op <) S y) ys 
 in
 iterate L xs
 end;

 
(* Mutual recursion*)
(* fun 
		<definition of first function>
	and 
		<definition of second function>
	and 
		..
	and	
		<definition of nth function>;
*)

fun f 0 = 1
 |   f  x  =  x * g (x-1)
and  g 0 = 1
 |      g y = y * f (y-1);

 (*Datatypes*)
 
 (* Enumerated types *)
 datatype suit = Spades | Hearts | Diamonds | Clubs;
 (*no significance to the ordering of the constructors in the declaration*)
 fun outranks (Spades, Spades) = false
  | outranks (Spades, _) = true
  | outranks (Hearts, Spades) = false
  | outranks (Hearts, Hearts) = false
  | outranks (Hearts, _) = true
  | outranks (Diamonds, Clubs) = true
  | outranks (Diamonds, _) = false
  | outranks (Clubs, _) = false;
  
datatype fruit=Banana|Pear|Grape;

fun isPear(x)=(x=Pear);

isPear(Banana);
isPear(Pear);
isPear(Apple);

(* Data only at leaves *)
(* Type variables can be used parametrize the datatype and data constructors can take arguments*)
datatype 'a tree=leaf of 'a |node of 'a tree list;
val myTree = node [node [node [leaf [4,2,14],leaf [9,83,32],leaf [96,123,4]],node [leaf [47,71,82]],node [leaf [19,27,10],leaf [111,77,22,66]],leaf [120,42,16]],leaf [83,13]];

fun fringe tree= let fun f (leaf x)= [x]
							| f (node [])=[]
							| f (node (x::nil)) = f(x)
							| f (node (x::xs)) = f(x) @ g(xs)
							and  g [] =[]
							| g (y::nil) = f(y)
							| g (y::ys)= f(y) @ g(ys)
							in 
								f tree
							end;


(* Data at leaves and internal nodes*)
datatype 'a ltree=node of 'a * 'a ltree list;							
val myLTree=node(3,[node(4,nil),node(5,[node(7,nil)]),node(6,nil)]);
fun sum(node (a,nil))=a
| sum(node(a,t::ts))=sum(t)+sum(node(a,ts));


(* You can also have mutually recursive datatypes*)
datatype 'a tree = Empty | Node of 'a * 'a forest
and      'a forest = Nil | Cons of 'a tree * 'a forest

(* Example of Search in a binary search tree*)
datatype 'a bst = Empty | node of 'a * 'a bst * 'a bst|leaf of 'a
 fun  searchBTree Empty y=false
 | searchBTree (leaf x) y= if x=y then true else false
 | searchBTree (node (x,l,r)) y = 
 if x=y then true
 else
 if y < x then
 searchBTree l y
 else 
 searchBTree r y
 ;

 