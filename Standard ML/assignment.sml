(* 
   ML Programming Assignment 
*)

Control.Print.printDepth := 100;
Control.Print.printLength := 100;

(* Q 1 *)

 fun partition x []=([],[])
 | partition x (y::ys)=let
 val (L,M)= partition x ys
 in if y<x then (y::L,M) else (L,y::M)
 end

(* Q 2 *)

 fun partitionSort []=[]
 | partitionSort [x]=[x]
 | partitionSort (y::ys) =let
 val (L,M)= partition y ys
 in (partitionSort L)@ [y] @ (partitionSort M)
 end

(* Q 3 *)

 fun Sort (op <) []=[]
 | Sort (op <) [x]=[x]
 | Sort (op <) (x::xs) = let
   fun partition (op <) pivot []=([],[])
   | partition (op <) pivot (y::ys) =let
   val (L,M) =partition (op <) pivot ys in
   if y < pivot then (y::L,M)
   else (L,y::M)
   end
 val (P,Q)=partition (op <) x xs in
 (Sort (op <) P)@ [x] @ (Sort (op <) Q)
 end

(* Q 4 *)

 datatype 'a tree = empty|leaf of 'a | node of 'a * 'a tree * 'a tree

(* Q 5 *)

 exception e

 fun maxTree (op <) empty = raise e
 | maxTree (op <) (leaf x) = x
 | maxTree (op <) (node (data,empty,empty)) = data  
 | maxTree (op <) (node (data,empty,r)) = 
    let 
       val right_max = maxTree (op <) r
       in if data < right_max then right_max else data
    end
 | maxTree (op <) (node (data,l,empty)) =
    let 
       val left_max = maxTree (op <) l
       in if data < left_max then left_max else data
    end

 | maxTree (op <) (node (data,l,r)) =
    let 
       val right_max = maxTree (op <) r
       val left_max = maxTree (op <) l
       in if data < left_max then 
	     if right_max < left_max then left_max 
	     else right_max 
	  else 
	     if right_max < data then data 
	     else right_max
    end


(* Q 6 *)

 fun preorder value left right = [value] @ left @ right

 fun postorder value left right = left @ right @ [value]

 fun inorder value left right = left @ [value] @ right

 fun Labels order empty = []
 | Labels order (leaf x) = [x]
 | Labels order (node(value,left,right)) = order value (Labels order left) (Labels order right)


(* Q 7 *)

 fun lexLess (op <) [] [] = false
 | lexLess (op <) [] (x::xs) = true
 | lexLess (op <) (x::xs) [] = false
 | lexLess (op <) (x::xs) (y::ys) = 
     if x < y then true 
     else 
	     if y < x then false
	     else lexLess (op <) xs ys  


(* Q 8 *)


 fun  sortTreeList (op <) L =  Sort (fn (A,B) => lexLess (op <) (Labels inorder A) (Labels inorder B) ) L
 

