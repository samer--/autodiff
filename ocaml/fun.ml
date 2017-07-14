type ('a,'b) either = Left of 'a | Right of 'b

module Fun = struct
  (* Bicartesian category of functions on products and sums *)
  let id x = x
  let ( >> ) f g x = g (f x)

  let intro x () = x
  let elim _ = ()

  let uniti x = ((),x)
  let unite ((),x) = x
  let swap (x,y) = (y,x)
  let fst (x,y) = x
  let snd (x,y) = y
  let first f (x,y) = (f x, y)
  let assocl (x,(y,z)) = ((x,y),z)
  let assocr ((x,y),z) =(x,(y,z))
  let dup x = (x,x)

  let inl x = Left x
  let inr x = Right x
  let merge = function | Left x -> x | Right y -> y
  let mirror = function | Left x -> Right x | Right y -> Left y
  let left f = function | Left x -> f x | Right y -> y

  let second f x = (swap >> first f >> swap) x
  let right f x = (mirror >> left f >> mirror) x
  let ( ** ) f g x = (first f >> second g) x
  let ( && ) f g x = (dup >> (f ** g)) x
  let ( ++ ) f g x = (left f >> right g) x
  let ( || ) f g x = ((f ++ g) >> merge) x 
end
