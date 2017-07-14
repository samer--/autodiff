(* Automatic differentiation 
 *
 * Type is now recursive. 1st order derivatives are arrows
 * too, but not the right kind of arrows for higher order
 * derivatives.
 * *)

open Utils

type ('a,'b) dres = DR of ('b * ('b -> ('b,'a) dres))
type ('a,'b) darr = 'a -> ('a,'b) dres

module HAD = struct
  let rec id x       = DR (x, id)
  let rec swap : 'a 'b. (('a * 'b), ('b * 'a)) darr = fun (x,y) -> DR (Utils.Pair.swap (x,y), swap)
  let rec uniti x      = DR (((),x), unite)
      and unite ((),x) = DR (x, uniti)
  let rec assocl (x,(y,z))  = DR (((x,y),z), assocr)
      and assocr ((x,y),z)  = DR ((x,(y,z)), assocl)

  let rec inl x = DR (Left x, merge)
      and inr x = DR (Right x, merge)
      and merge = function 
                  | Left x -> DR (x, inl) 
                  | Right y -> DR (y, inr)

  let rec mirror x = DR (Utils.mirror x, mirror)

  let rec ( >> ) : 'a 'b 'c. ('a -> ('a,'b) dres) -> ('b -> ('b,'c) dres) -> 'a -> ('a,'c) dres = fun f g x ->
    let DR (y,df) = f x in
    let DR (z,dg) = g y in
    DR (z, fun dz -> (dg >> df) dz)

  let rec left f = function 
    | Left x -> let DR (z,df) = f x in DR (Left z, left df)
    | Right y -> DR (Right y, id)

  let rec first : 'a 'b 'c. ('a,'b) darr -> ('a * 'c, 'b * 'c) darr = fun f (x,y) ->
    let DR (z, df) = f x in
    DR ((z,y), first df)

  let second f x = (swap >> first f >> swap) x
  let right f x = (mirror >> left f >> mirror) x
  let ( ** ) f g x = (first f >> second g) x
  let ( ++ ) f g x = (left f >> right g) x
  (* let ( || ) f g x = ((f ++ g) >> merge) x *)

  let rec neg x     = DR (~-. x, neg)
  let rec scale k x = DR (k *. x, scale k)
  let log x = DR (Pervasives.log x, scale (1.0 /. x))
  let exp x = let y = Pervasives.exp x in DR (y, scale y)

  let grad f x = 
    let DR (y,df) = f x in 
    let DR (dy_dx,_) = df 1.0 in
    (y, dy_dx)
  
  let run f x = let DR (y,_) = f x in y
  let ( |>> ) x f = run f x 
end

module type ADD = sig
  type t
  val zero : t
  val add : t * t -> t
end

module Unit = struct
  type t = unit
  let zero = ()
  let add ((), ()) = ()
end

module Float = struct
  type t = float
  let zero = 0.0
  let add (x,y) = x +. y
end

module Product (M : ADD) (N: ADD) = struct
  type t = M.t * N.t
  let zero = (M.zero, N.zero)
  let add ((x1,x2), (y1,y2)) = (M.add (x1,y1), N.add (x2,y2))
end

(* module Const (M : ADD) (N : ADD) = struct *)
(*   let rec const x _ = DR (x, co_const M.zero) *)
(*       and co_const x _ = DR (x, const N.zero) *)
(* end *)

module HADA (M : ADD) = struct
  open HAD
  let rec elim _    = DR ((), intro M.zero)
      and intro x _ = DR (x, elim)

  let rec dup x     = DR ((x,x), add)
      and add xy    = DR (M.add xy, dup)

  let ( && ) f g x = (dup >> (f ** g)) x
  let snd x = (first elim >> unite) x
  let fst x = (swap >> snd) x
end

module ADF = HADA (Float)
module ADP = HADA (Product (Float) (Float))
module ADU = HADA (Unit)
open HAD

let mul (x,y)  = DR HAD.(x *. y, ADF.(scale y && scale x))
