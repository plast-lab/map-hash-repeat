type Node = N of int * List<int>

type Graph = 
    | G of List<Node>
    | Empty

let insert g n = 
    match g with 
    | G(nodes) -> G(List.append nodes [n])
    | Empty -> G([n])
        
//gets two nodes and make edge
let connect n1 n2 = 
    match n1,n2 with
    | N(v1,neighbors1), N(v2,_)  -> N(v1,List.append neighbors1 [v2])
        
           
    
let n0 = ref (N(0,[]))
let n1 = ref (N(1,[]))
let n2 = ref (N(2,[]))
let n3 = ref (N(3,[]))
let n4 = ref (N(4,[]))
let n5 = ref (N(5,[]))
let n6 = ref (N(6,[]))
let n7 = ref (N(7,[]))

n0 := connect n0.Value n2.Value
n2 := connect n2.Value n1.Value
n2 := connect n2.Value n5.Value
n2 := connect n2.Value n3.Value
n4 := connect n4.Value n7.Value
n4 := connect n4.Value n2.Value
n6 := connect n6.Value n7.Value
n6 := connect n6.Value n5.Value
n7 := connect n7.Value n3.Value

let g = ref Empty
    
g := insert g.Value n0.Value
g := insert g.Value n1.Value
g := insert g.Value n2.Value
g := insert g.Value n3.Value
g := insert g.Value n4.Value
g := insert g.Value n5.Value
g := insert g.Value n6.Value
g := insert g.Value n7.Value



