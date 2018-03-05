(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Laura Sullivan-Russett
* lbsrussett@gmail.com
*
***************************************************************)

(* set datatype to hold a group of elements *)
datatype 'a set = Empty | Set of 'a * 'a set;

(* isMember function to determine if an input item exists inside of a set *)
fun isMember e Empty = false | (* if the set is empty, return false*)
  isMember e (Set(head, tail)) = e = head (* if the input = first item in the set return true*)
  orelse isMember e tail; (* otherwise, recurse on the tail of the set*)

(* isListMember function to determine if an input item exists inside of a list *)
fun isListMember e []= false | 
  isListMember e (head::tail) = 
   if e = head then true
  else isListMember e tail;

(* list2Set function to convert an ml list into a set *)
fun list2Set [] = Empty | (* if the list is empty, create an Empty set *)
  list2Set(head::tail) = 
  (* if the head of the list is in the list, don't add it and recurse on the tail of the list *)
  if isListMember head tail then list2Set tail 
  (* otherwise, add the head to the set and recurse on the tail *)
  else Set(head, list2Set tail); 

(* union function to return the mathematical union of two input sets *)
fun union Empty s2 = s2 | (* if one set is Empty, return the other *)
 union (Set(head, tail)) s2 = 
 (* if the head of the first set exists in the second set, recurse on the tail of the first set *)
 if isMember head s2 then union tail s2
 (* otherwise add the head of the first set to the second set and recurse on the tail of the first set *)
 else Set(head, union tail s2);

(* intersect function to find the mathematical union of two input sets*)
fun intersect Empty s2 = Empty | (* if one set is Empty, return Empty *)
  intersect (Set(head, tail)) s2 = 
  (* if the head of one set exists in the other, add the head to a set and recurse on the tail *)
  if isMember head s2 then Set(head, intersect tail s2) 
  (* otherwise recurse on the rest of the first set *)
  else intersect tail s2;

(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

list2Set [1, 3, 2];
list2Set [#"a", #"b", #"c"];
list2Set [];
list2Set [6, 2, 2];
list2Set ["x", "y", "z", "x"];

fun f [] = [] |
  f (x::xs) = (x + 1) :: (f xs);

(* Question 1 *)
f [3, 1, 4, 1, 5, 9];

(* Question 5 *)
val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
