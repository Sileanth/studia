type 'a ltree = 'a ltree_data Lazy.t
and 'a ltree_data ={ left : 'a ltree; elem : 'a; right : 'a ltree } 

type wym = int * int