(* Automatic differentiation 
 *
 * Type is now recursive. Trying for higher order derivatives.
 * It will be a miracle if I can get this work.
 *
 * This is hopeless.
 *)


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

type ('a,'b) darr = DA of (('a -> 'b) * ('a*'b,'a) darr)

module AD (M : ADD) = struct
  (* Cartesian category of differentiable functions *)
  let rec id  = DA (Fun.id, snd)
      and (>>) (DA (f, df) as ff) (DA (g, dg)) = DA (Fun.(f >> g), fst)

      (* and intro x = DA (Fun.intro x, fst) *)
      (* and elim = DA (Fun.elim, fst) *)
      (* and dup = DA (Fun.dup, fst) *)
      (* and add = DA (M.add, fst) *)
      and fst : 'a 'b.('a*'b, 'a) darr = DA (Fun.fst, fst)
      and snd : 'a 'b.('a*'b, 'b) darr = DA (Fun.snd, fst)

      (* and uniti = DA (Fun.uniti, snd >> snd) *)
      (* and unite = DA (Fun.uniti, snd >> uniti) *)
      and swap : 'a 'b.('a*'b, 'b*'a) darr = DA (Fun.swap, snd >> swap)
      (* and first (DA (f,df)) = DA (Fun.first f, fst) *)
      (* and second (DA (f,df)) = DA (Fun.second f, fst) *)
      (* and assocr = DA (Fun.assocr, snd >> assocl) *) 
      (* and assocl = DA (Fun.assocl, snd >> assocr) *)

      (* and (>>) (DA (f, df) as ff) (DA (g, dg)) = DA (Fun.(f >> g), first (dup >> second ff) >> assocr >> second dg >> df) *) 
      (* and intro x = DA (Fun.intro x, fst) *)
      (* and elim = DA (Fun.elim, elim >> intro M.zero) *)
      (* and dup = DA (Fun.dup, snd >> add) *)
      (* and add = DA (M.add, snd >> dup) *)
      (* and fst = DA (Fun.fst, snd >> uniti >> first (intro M.zero) >> swap) *)
      (* and snd = DA (Fun.snd, snd >> uniti >> first (intro M.zero)) *)

      (* and uniti = DA (Fun.uniti, snd >> snd) *)
      (* and unite = DA (Fun.uniti, snd >> uniti) *)
      (* and swap = DA (Fun.swap, snd >> swap) *)
      (* and first (DA (f,df)) = DA (Fun.first f, first fst >> assocl >> first df) *)
      (* and second (DA (f,df)) = DA (Fun.second f, first snd >> swap >> assocr >> second (swap df)) *)
      (* and assocr = DA (Fun.assocr, snd >> assocl) *) 
      (* and assocl = DA (Fun.assocl, snd >> assocr) *)


(*   let rec swap : 'a 'b. (('a * 'b), ('b * 'a)) darr = fun (x,y) -> DR (Utils.Pair.swap (x,y), swap) *)
(*       and unite ((),x) = DR (x, uniti) *)
(*   let rec assocl (x,(y,z))  = DR (((x,y),z), assocr) *)
(*       and assocr ((x,y),z)  = DR ((x,(y,z)), assocl) *)

(*   let rec inl x = DR (Left x, merge) *)
(*       and inr x = DR (Right x, merge) *)
(*       and merge = function *) 
(*                   | Left x -> DR (x, inl) *) 
(*                   | Right y -> DR (y, inr) *)

(*   let rec mirror x = DR (Utils.mirror x, mirror) *)
(*   let rec dup x     = DR ((x,x), add) *)
(*       and add xy    = DR (M.add xy, dup) *)

(*   let ( && ) f g x = (dup >> (f ** g)) x *)
(*   let snd x = (first elim >> unite) x *)
(*   let fst x = (swap >> snd) x *)

(*   let rec ( >> ) : 'a 'b 'c. ('a -> ('a,'b) dres) -> ('b -> ('b,'c) dres) -> 'a -> ('a,'c) dres = fun f g x -> *)
(*     let DR (y,df) = f x in *)
(*     let DR (z,dg) = g y in *)
(*     DR (z, fun dz -> (dg >> df) dz) *)

(*   let rec left f = function *) 
(*     | Left x -> let DR (z,df) = f x in DR (Left z, left df) *)
(*     | Right y -> DR (Right y, id) *)

(*   let rec first : 'a 'b 'c. ('a,'b) darr -> ('a * 'c, 'b * 'c) darr = fun f (x,y) -> *)
(*     let DR (z, df) = f x in *)
(*     DR ((z,y), first df) *)

(*   let second f x = (swap >> first f >> swap) x *)
(*   let right f x = (mirror >> left f >> mirror) x *)
(*   let ( ** ) f g x = (first f >> second g) x *)
(*   let ( ++ ) f g x = (left f >> right g) x *)
(*   (1* let ( || ) f g x = ((f ++ g) >> merge) x *1) *)

(*   let rec neg x     = DR (~-. x, neg) *)
(*   let rec scale k x = DR (k *. x, scale k) *)
(*   let log x = DR (Pervasives.log x, scale (1.0 /. x)) *)
(*   let exp x = let y = Pervasives.exp x in DR (y, scale y) *)

(*   let grad f x = *) 
(*     let DR (y,df) = f x in *) 
(*     let DR (dy_dx,_) = df 1.0 in *)
(*     (y, dy_dx) *)
  
   (* let grad (DA (f,df)) = uniti >> first (intro 1.0) >> swap >> df *)
   let run (DA (f,_)) x = f x
   let ( |>> ) x f = run f x 
end

module ADF = AD (Float)
(* module ADP = HAD (Product (Float) (Float)) *)
(* module ADU = HAD (Unit) *)
open ADF

(* let mul (x,y)  = DR HAD.(x *. y, ADF.(scale y && scale x)) *)
