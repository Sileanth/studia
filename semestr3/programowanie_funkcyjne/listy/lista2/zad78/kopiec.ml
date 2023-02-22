exception Foo of string

type 'a tree = 
| Leaf
| Node of { len : int; left : 'a tree; right : 'a tree; value : 'a }



let construct a b v = 
  match a ,b with
  | Leaf , Leaf -> Node {len = 1; left = a; right = b; value = v}
  | Leaf , Node _ -> Node {len = 1 ; left = b; right = a ; value = v}
  | Node _ , Leaf -> Node {len = 1  ; left = a; right = b ; value = v}
  | Node {len = la} , Node {len = lb} when la <= lb -> Node {len = 1 + la ; left = b ; right = a; value = v}
  | Node {len = la} , Node {len = lb} -> Node {len = 1 + lb ; left = a ; right = b; value = v}

  
let rec union a b =
  match a , b with
  | Leaf , Leaf -> Leaf
  | Leaf , Node _ -> b
  | Node _ , Leaf -> a
  | Node {value = va; left =la ; right = ra} , Node {value = vb ; left = lb; right = rb} 
      when va <= vb -> construct la (union ra b) va
  | Node {value = va; left =la ; right = ra} , Node {value = vb ; left = lb; right = rb} 
      -> construct lb (union rb a) vb


let add v t =
  union t (construct Leaf Leaf v)


let pop = function
  | Leaf -> Leaf
  | Node {left ; right} -> union left right

let empty = function
  | Leaf -> true
  | Node _ -> false

let value = function
  | Leaf -> raise(Foo "Pusty kopiec")
  | Node {value} -> value


let null = Leaf