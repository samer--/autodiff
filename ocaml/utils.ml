(* utitilities *)
type 'a thunk = unit -> 'a

let id x = x
let dup x = (x,x)
let const x _ = x
let flip p x y = p y x
(* let compose : 'a 'b 'c. ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c = fun f g x -> f (g x) *)
let compose f g x = f (g x)
let ( ** ) = compose
let ( $ ) f x = f x
let ( |> ) x f = f x
let curry f x y = f (x,y)
let uncurry f (x,y) = f x y
let cons x y = x::y
let rec ( -- ) n m = if n>m then [] else n::(n+1 -- m)
let rec iterate n f x = if n=0 then x else iterate (pred n) f (f x)
let apply_to x f = f x
let sum = List.fold_left ( + ) 0
let nub xs = BatList.sort_unique compare xs

let rec drop n l = match (n,l) with
  | (0,l) -> l
  | (n,_::l) -> drop (pred n) l


let string_of_list = String.concat " "

(* Some useful signatures *)
module type TYPE = sig
  type t
end

module type SHOW = sig
  include TYPE
  val show : t -> string
end

module type EQ = sig
  type t
  val eq : t -> t -> bool
end

(* ----------------------------------------------------------- *)

module Debug = struct
  let log s = print_endline ("--> "^s)
  let plog s v = print_endline ("<-- "^s^" : done"); v
end
module NoDebug = struct
  let log s = ()
  let plog s v = v
end
(* include Debug *)


module Pair = struct
  let ( $> ) f (x,y) = (x, f y)
  let ( <$ ) f (x,y) = (f x, y)
  let ( <$> ) f (x,y) = (f x, f y)
  let ( $$ ) (f,g) (x,y) = (f x, g y)
  let pmap f (x,y) = (f x, f y)
  let pzip f (x1,y1) (x2,y2) = (f x1 x2, f y1 y2)
  let pair x y = (x,y)
  let swap (x,y) = (y,x)
end

let ispos x = x>0.0
let isneg x = x<0.0
let isnonneg x = x>=0.0
let isnonpos x = x<=0.0

type 'a transformer = 'a -> 'a
let option f g = function | None -> f | Some x -> g x
type ('a,'b) either = Left of 'a | Right of 'b
let either f g = function | Left x -> f x | Right y -> g y
let left f = function | Left x -> Left (f x) | Right y -> Right y
let inl x = Left x
let inr x = Right x
let mirror x = either inr inl x
let merge  = function | Left x -> x | Right y -> y

let more f x = 
  f x; 
  print_string "   [more?]"; 
  let x=read_line () in
  if x="" then () else failwith "aborted"


module Dynamic : sig 
  exception Dynamic
  type dyn
  val newdyn : unit -> ('a -> dyn) * (dyn -> 'a)
end= struct
  (* This modules exploits impure computations (mutable references) to implement
   * a sort of universal type. I copied it from Filinski. The trick is to use a
   * mutable reference as a communication channel between a thunk of fixed type 
   * unit -> unit and a polymorphic function to read the value. 
   *)
  exception Dynamic
  type dyn = Dyn of (unit->unit)

  let newdyn () = 
    let r = ref None in
    ( (fun a -> Dyn (fun () -> r:=Some a)),
      (fun (Dyn d) ->
        r := None; d (); 
        match !r with
        | Some a -> a
        | None -> raise Dynamic))
end

module Modulo (M : sig val modulus : int end) = struct
  (* Functor for modulo-N arithmetic. Only additiona and subtraction for now. *)
  let fix x = let z=x mod M.modulus in if z<0 then z+M.modulus else z
  let ( +% ) x y = fix (x + y)
  let ( -% ) x y = fix (x - y) 
end

module type FIELD = sig
  type t

  val one  : t
  val zero : t
  val ( *. ) : t -> t -> t
  val ( /. ) : t -> t -> t
  val ( +. ) : t -> t -> t
  val ( -. ) : t -> t -> t
  val ( %  ) : int -> int -> t

  val to_string : t -> string
  val to_float  : t -> float
  val from_int  : int -> t
  val recip_int : int -> t
end

module Float = struct
  type t = float
  let to_string = Printf.sprintf "%5.3f" 
  let to_float x = x
  let from_int = float_of_int
  let recip_int n = 1.0 /. (float_of_int n)
  let one = 1.0
  let zero = 0.0
  let ( *. ) =  ( *. )
  let ( /. ) =  ( /. )
  let ( +. ) =  ( +. )
  let ( -. ) =  ( -. )

  let ( % ) num den = float_of_int num /. float_of_int den
end

module Rational = struct
  (* more convenient construction of raional numbers *)
  open Num

  type rational = num
  type t = rational

  let ( *. ) = mult_num
  let ( /. ) = div_num
  let ( +. ) = add_num
  let ( -. ) = sub_num

  let one = num_of_int 1
  let zero = num_of_int 0
  let to_string = string_of_num 
  let to_float = float_of_num
  let from_int = num_of_int
  let recip_int n = num_of_int 1 // num_of_int n

  let ( % ) num den = num_of_int num // num_of_int den
end



module Format = struct
  (* some tools for building string_of_xxx functions *)
  let bool = string_of_bool
  let int = string_of_int
  let string = id
  let list sep f = String.concat sep ** List.map f
  let int_list = list "" int
  let float = string_of_float
  let rat = Rational.to_string
  let paren f x = "(" ^ f x ^ ")"
  let bracket f x = "[" ^ f x ^ "]"
  let pair sep f g (x,y) = f x ^ sep ^ g y
  let ignore _ = ""
  let option f x = match x with | None -> "><" | Some x -> "<" ^ f x ^ ">"
  (* let weighted f (w,x) = float w ^ ":" ^ f x *)
  let char = Printf.sprintf "%c"
end

(* TYPE modules *)
module STRING = struct
  type t = string
  let show s = s
end

module CHAR = struct
  type t = char
  let show c = Format.char c
end

  
(* time execution *)
let timeit thunk =
  let time_start = Sys.time () in
  let result = thunk () in
  Printf.printf "Time: %g s" (Sys.time () -. time_start);
  print_endline "";
  result
