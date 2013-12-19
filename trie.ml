

type 'a trie_node = 
   | Empty 
   | Node of 'a * 'a trie_node * 'a trie_node * bool;;
            (* data, sibling, down, word *)

let rec insert lst trie = 
   match lst, trie with 
   | [], Empty -> Empty 
   | [], Node (elem, sibling, down, _) -> Node (elem, sibling, down, true)
   | h::t, Empty -> Node (h, Empty, insert t Empty, false)
   | h::t as l, Node (elem, sibling, down, word) ->
         if elem = h then Node (elem, sibling, insert t down, word)
         else Node (elem, insert l sibling, down, word)
;;

let rec contains lst trie = 
   match lst, trie with 
   | [], Empty -> true
   | [], Node (_, _, _, word) -> word
   | h::t, Empty -> false
   | h::t as l, Node (elem, sibling, down, word) ->
         if h = elem then contains t down
         else contains l sibling
;;


let data = [[1; 2; 3]; [1; 2; 4]; [1; 2]];;
let trie = List.fold_left (fun x y -> insert y x) Empty data;;

let test lst = print_endline (string_of_bool (contains lst trie));;

test [1; 2; 3];;
test [1; 2];;
test [1; 2; 4];;
test [1; 4];;
test [1; 3];;
test [1];;

