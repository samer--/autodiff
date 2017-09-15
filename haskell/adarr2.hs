{-# LANGUAGE TypeOperators, FlexibleInstances #-}
import Prelude hiding ((>>), (**), (&&), id, fst, snd, exp, log)
import qualified Prelude

-- This is an experiment in functional reverse mode automatic differentiation using
-- a sort of arrow-ish system to represent a dataflow graph that includes the computation
-- of derivatives as well as the computation a primal function.
--
-- It doesn't exactly fit the Arrow type class, partly because there is no way to lift
-- an arbitrary function to a differentiable function and thus no way to implement arr,
-- and also because the differentiability requirement means that most of the data types
-- flowing through the graph need to be able to represent their own tangent spaces; that is,
-- we need `add` and `zero` operators for every data type.
--
-- Instead, the space of differentiable functions is a closer fit to a Cartesian or 
-- bi-Cartesian category. Below, I've only implemented a Cartesian category that can
-- compute with product types. The set of primitives is modelled on Roshan and Sabry's
-- work on reversible computing and information effects [1,2]. The back-propagation of
-- derivatives is not completely unlike reverse computation, and also idea of information
-- effects inherent in certain operations that create or destroy information seems to
-- have resonance in here in the way that dup, intro and elim work. Conal Elliot's work
-- on compiling functions to circuits in closed bi-Cartesian categories was also an influence.
--
-- I orignally wanted to define a class CartesianCategory with instances for ordinary
-- functions (->) and for differentiable functions (~>), but this turned out to be rather
-- difficult because of the additional constraints on types imposed by the differentiable
-- version. The type class Plus states that a type forms an additive monoid and is needed
-- to add derivatives or return an appropriate zero. The differentiable category needs a lot
-- of Plus type constraints which seem to disqualify it from being a general cartesian category.
--
-- So instead, I define the functions for the (->) category with primed names (like id')
-- and the functions for the (~>) with unprimed names, hiding the corresponding ones from
-- Prelude where necesssary.
--
-- [1] Roshan P James and Amr Sabry. Information effects. 
--     In ACM SIGPLAN Notices, volume 47, pages 73–84. ACM, 2012.
-- [2] Roshan P James and Amr Sabry. The two dualities of computation: 
--     Negative and fractional types. Technical report, Citeseer, 2012. 

infixr 0 ~>
infixr 7 **
infixr 7 &&
infixr 0 |>

class Plus t where
	plus :: (t,t) -> t 
	zero :: t 

instance (Num t) => Plus t where
	plus (x,y) = x + y
	zero = 0

instance Plus () where
	plus ((),()) = ()
	zero = ()

instance (Plus t, Plus u) => Plus (t,u) where
  plus ((x1,x2), (y1,y2)) = (plus (x1,y1), plus (x2,y2))
  zero = (zero,zero)

-- class CartesianCategory k where
--   id     ::  a `k` a
--   (>>)   :: (a `k` b) -> (b `k` c) -> (a `k` c)

--   -- wiring (type isomorphisms)
--   uniti  :: a `k` ((),a)
--   unite  :: ((),a) `k` a
--   swap   :: (a,b) `k` (b,a)
--   assocl :: (a,(b,c)) `k` ((a,b),c)
--   assocr :: ((a,b),c) `k` (a,(b,c))

--   -- information effects (see Roshan and Sabry)
--   intro  :: a -> () `k` a
--   elim   :: a `k` ()
--   dup    :: a `k` (a,a)
--   fst    :: (a,b) `k` a
--   snd    :: (a,b) `k` b
--
--   -- Arrow combinators
--   first  :: (a `k` b) -> (a,c) `k` (b,c)
--   second :: (a `k` b) -> (c,a) `k` (c,b)
--   (**) :: (a `k` b) -> (c `k` d) -> (a,c) `k` (b,d)
--   (&&) :: (a `k` b) -> (a `k` c) -> a `k` (b,c)


-- instance CartesianCategory (->) where
--    ... mostly obvious - see below ...

id' x = x
intro' x () = x
elim' _ = ()
dup' x = (x,x)
uniti' x = ((),x)
unite' (_,x) = x
swap' (x,y) = (y,x)
fst' (x,_) = x
snd' (_,y) = y
assocl' (x,(y,z)) = ((x,y),z)
assocr' ((x,y),z) = (x,(y,z))
first' f (x,y) = (f x, y)
second' f (x,y) = (x, f y)

data a ~> b = AD (a->b) ((b,a) ~> a)

-- instance CartesianCategory (~>) where
--    ... won't fly due to Plus type constraints everywhere

id ::  Plus a => a ~> a
id = AD id' fst

(>>) :: (Plus a, Plus b, Plus c) => (a ~> b) -> (b ~> c) -> (a ~> c)
ff@(AD f df) >> AD g dg = AD (g . f) (second (dup >> first ff) >> assocl >> first dg >> df)

intro x = AD (intro' x) fst
elim :: Plus t => t ~> ()
elim  = AD elim' (fst >> intro zero)

dup :: Plus t => t ~> (t,t)
dup = AD dup' (fst >> add)

add :: Plus t => (t,t) ~> t
add = AD plus (fst >> dup)

uniti :: Plus t => t ~> ((),t)
uniti = let fs = fst >> swap in AD uniti' (fs >> fs)

unite :: Plus t => ((),t) ~> t
unite = AD unite' (fst >> uniti)

fst :: (Plus a, Plus b) => (a,b) ~> a
fst = AD fst' (fst >> uniti >> swap >> second (intro zero))

snd :: (Plus a, Plus b) => (a,b) ~> b
snd = AD snd' (fst >> uniti >> first (intro zero))

swap :: (Plus a, Plus b) => (a,b) ~> (b,a)
swap = AD swap' (fst >> swap)

assocl :: (Plus a, Plus b, Plus c) => (a,(b,c)) ~> ((a,b),c)
assocl = AD assocl' (fst >> assocr)

assocr :: (Plus a, Plus b, Plus c) => ((a,b),c) ~> (a,(b,c))
assocr = AD assocr' (fst >> assocl)

first :: (Plus a, Plus b, Plus c) => (a ~> b) -> (a,c) ~> (b,c)
first (AD f df) = AD (first' f) (second fst >> swap >> assocl >> first (swap df))

second :: (Plus a, Plus b, Plus c) => (a ~> b) -> (c,a) ~> (c,b)
second (AD f df) = AD (second' f) (second snd >> assocr >> second df)

f ** g = first f >> second g
f && g = dup >> (f ** g)

mul :: (Num t, Plus t) => (t,t) ~> t
mul = AD (uncurry (Prelude.*)) 
         (second dup 
          >> assocl 
          >> first (assocr >> swap >> first mul) 
          >> assocr >> second mul)

scale :: (Num t, Plus t) => t -> t ~> t
scale k = AD (Prelude.* k) (snd >> scale k)

neg :: (Num t, Plus t) => t ~> t
neg = scale (-1)

pow 0 = elim >> intro 1
pow 1 = id
pow k = AD (Prelude.** k) (first (pow (k-1) >> scale k) >> mul)

exp :: (Floating t, Plus t) => t ~> t
exp = AD Prelude.exp (first exp >> mul) 

log :: (Eq t, Floating t, Plus t) => t ~> t
log = AD Prelude.log (first (pow (-1)) >> mul)

grad (AD f df) = uniti >> first (intro 1.0) >> swap >> df

x |> AD f df = f x
