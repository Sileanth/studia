open Logic
open Formula

module EmptyTheory : Theory = struct
        type axiom = |
        let axiom _ = assert false
end



module EmptyLogic = Logic.Make(EmptyTheory) 



