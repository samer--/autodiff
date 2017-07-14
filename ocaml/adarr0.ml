(* Automatic differentiation *)
(* Simple version - just functions fwd and backwd.
 * No higher order derivatives.
 *
 * 861 862 910
 * Launcher7 colour: 2d443c
 *)
open Utils

let unleft (Left x) = x
let unright (Right x) = x

module AD = struct
  type ('a,'b) dfn = 'a -> 'b * ('b -> 'a)

  let id x       = (x, Utils.id)
  let const y _  = (y, fun _ -> 0.0) (* wrong type! need polymorphic zero! *)
  let fst (x,y)  = (x, fun dx -> (dx, 0.0))
  let dup x      = ((x,x), fun (d1,d2) -> d1 +. d2)
  let merge      = function | Left x -> (x, inl) | Right y -> (y, inr)
  let swap (x,y) = Utils.Pair.(swap (x,y), swap)
  let mirror x   = Utils.(mirror x, mirror)
  let tag_e c x  = if c x then (Left x, unleft) else (Right x, unright)

  let ( >> ) f g x =
    let open Utils in 
    let (y,df) = f x in
    let (z,dg) = g y in
    (z, compose df dg)

  let first f (x,y) = 
    let (z, df) = f x in 
    let sens (dz,dy) = (df dz, dy) in
    ((z,y), sens) 

  let exl (x,()) = (x, fun dx -> (dx,()))

  let left f = function 
    | Left x -> let (z,df) = f x in (Left z, Utils.left df)
    | Right y -> (Right y, Utils.id)

  (* derived plumbing *)
  let exr = swap >> exl >> swap
  let second f x = (swap >> first f >> swap) x
  let right f = (mirror >> left f >> mirror)
  let ( << ) f g x = (g >> f)  x
  let ( ** ) f g x = (first f >> second g) x
  let ( && ) f g x = (dup >> (f ** g)) x
  let ( ++ ) f g x = (left f >> right g) x
  let ( || ) f g x = ((f ++ g) >> merge) x
  let cond c f g x = (tag_e c >> (f || g)) x

  let log x      = (Pervasives.log x, fun dy -> dy /. x)
  let exp x      = let y = Pervasives.exp x in (y, ( *. ) y)
  let neg x      = (~-. x, ( ~-. ))
  let add (x,y)  = (x +. y, Utils.dup)
  let mul (x,y)  = (x *. y, fun dz -> (y *. dz, x *. dz))
  let grad f x = let (y,df) = f x in (y, df 1.0)
end

