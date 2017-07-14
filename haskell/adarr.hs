{-# LANGUAGE TypeOperators, FlexibleInstances #-}
import Prelude hiding ((>>), (++), (**), (||), (&&), id, fst, snd, exp, log)
import qualified Prelude

{-
  This is an experiment in functional reverse mode automatic differentiation using
  a sort of arrow-ish system to represent a dataflow graph that includes the computation
  of derivatives as well as the computation of a primal function.

  It doesn't exactly fit the Arrow type class, partly because there is no way to lift
  an arbitrary function to a differentiable function and thus no way to implement arr,
  and also because the differentiability requirement means that most of the data types
  flowing through the graph need to be able to represent their own tangent spaces, at
  least as far as being able to add them and get an appropriate zer.

  Instead, the space of differentiable functions is a closer fit to a Cartesian or 
  bi-Cartesian category. Below, I've only implemented a Cartesian category, ie with
  product types. The set of primitives is modelled on Roshan and Sabry's
  work on reversible computing and information effects [1,2]. The back-propagation of
  derivatives is not completely unlike reverse computation, and also idea of information
  effects inherent in certain operations that create or destroy information seems to
  have resonance in here in the way that fork, intro and elim work. Conal Elliot's work [3]
  on compiling functions to circuits in closed bi-Cartesian categories was also an influence.

  I orignally wanted to define a class CartesianCategory with instances for ordinary
  functions (->) and for differentiable functions (~>), but this turned out to be rather
  difficult because of the additional constraints on types imposed by the differentiable
  version. The type class Plus states that a type forms an additive monoid and is needed
  to add derivatives or return an appropriate zero. The differentiable category needs a lot
  of Plus type constraints which seem to disqualify it from being a general cartesian category.

  So instead, I define the functions for the (->) category with primed names (like id')
  and the functions for the (~>) with unprimed names, hiding the corresponding ones from
  Prelude where necesssary.

  Some remaining questions

  0. Can we restore the type class CartesianCategory and make (~>) an instance of 
     of it by somehow lifing the Plus constraints out?
  1. Can we turn this into a closed category by allowing higher-order computations?
     In the Arrow lingo, this means making the type an instance of ArrowApply.
  2. Can we deal with recursive datatypes, especially lists?
  3. Can we turn more normal looking code (lambda calculus) into arrowish circuits
     automatically? (This is what Conal Elliot wants to do.)
  4. Can 3. be done by some sort of magic with delimited continuations?

  [1] Roshan P James and Amr Sabry. Information effects. 
      In ACM SIGPLAN Notices, volume 47, pages 73–84. ACM, 2012.
  [2] Roshan P James and Amr Sabry. The two dualities of computation: 
      Negative and fractional types. Technical report, Citeseer, 2012. 
  [3] Conal Elliot. Overloading lambda. Blog post 
      http://conal.net/blog/posts/overloading-lambda
-}

infixr 0 ~>
infixr 7 **
infixr 7 &&
infixr 0 |>

class Eq t => Plus t where
	plus :: (t,t) -> t 
	zero :: t 

instance (Eq t, Num t) => Plus t where
	plus (x,y) = x + y
	zero = 0

instance Plus () where
	plus ((),()) = ()
	zero = ()

instance (Plus t, Plus u) => Plus (t,u) where
  plus ((x1,x2), (y1,y2)) = (plus (x1,y1), plus (x2,y2))
  zero = (zero,zero)

instance (Plus t, Plus u) => Plus (Either t u) where
  plus (Left x, Left y) = Left (plus (x,y))
  plus (Right x, Right y) = Right (plus (x,y))
  zero = Left zero -- !!!
-- class Category k where
--   id     ::  a `k` a
--   (>>)   :: (a `k` b) -> (b `k` c) -> (a `k` c)
--
--   intro  :: a -> () `k` a
--   elim   :: a `k` ()
--
--
-- class Category k => CartesianCategory k where
--
--   -- wiring (type isomorphisms)
--   uniti  :: a `k` ((),a)
--   unite  :: ((),a) `k` a
--   swap   :: (a,b) `k` (b,a)
--   assocl :: (a,(b,c)) `k` ((a,b),c)
--   assocr :: ((a,b),c) `k` (a,(b,c))
--
--   -- information effects (see Roshan and Sabry)
--   fork   :: a `k` (a,a)
--   fst    :: (a,b) `k` a
--   snd    :: (a,b) `k` b
--
--   -- Arrow combinators
--   first  :: (a `k` b) -> (a,c) `k` (b,c)
--   second :: (a `k` b) -> (c,a) `k` (c,b)
--   (**) :: (a `k` b) -> (c `k` d) -> (a,c) `k` (b,d)
--   (&&) :: (a `k` b) -> (a `k` c) -> a `k` (b,c)
--
-- class Category k => CoCartesianCategory k where
--   mirror  :: Either a b -> Either b a
--   coassocl :: Either a (Either b c) `k` Either (Either a b) c
--   coassocr :: Either (Either a b) c `k` Either a (Either b c)
--
--   inl    :: a `k` Either a b
--   inr    :: b `k` Either a b
--   join   :: Either a a `k` a
--
--   -- ArrowPlus combinators
--   left   :: (a `k` b) -> Either a c `k` Either b c
--   right  :: (a `k` b) -> Either c a `k` Either c b
--   (++) :: (a `k` b) -> (c `k` d) -> Either a c `k` Either b d
--   (|| :: (a `k` c) -> (b `k` c) -> Either a b `k` c
--
-- class (CartesianCategory k, CoCartesianCategory) => BiCartesianCategory k where
--   distrib :: (Either a b, c) `k` Either (a,c) (b,c)
--   factor :: Either (a,c) (b,c) `k` (Either a b, c) 


-- instance BiCartesianCategory (->) where
--    ... mostly obvious - see below ...

id' x = x
intro' x () = x
elim' _ = ()

fork' x = (x,x)
uniti' x = ((),x)
unite' (_,x) = x
swap' (x,y) = (y,x)
fst' (x,_) = x
snd' (_,y) = y
assocl' (x,(y,z)) = ((x,y),z)
assocr' ((x,y),z) = (x,(y,z))
first' f (x,y) = (f x, y)
second' f (x,y) = (x, f y)

join' (Left x) = x
join' (Right x) = x
mirror' (Left x) = Right x
mirror' (Right x) = Left x
inl' x = Left x
inr' x = Right x

coassocl' (Left x)          = Left (Left x)
coassocl' (Right (Left x))  = Left (Right x)
coassocl' (Right (Right x)) = Right x

coassocr' (Left (Left x))  = Left x
coassocr' (Left (Right x)) = Right (Left x)
coassocr' (Right x)        = Right (Right x)

left' f (Left x) = Left (f x)
left' f (Right x) = Right x
right' f (Left x) = Left x
right' f (Right x) = Right (f x)

distrib' (Left x, y) = Left (x,y)
distrib' (Right x, y) = Right (x,y)

factor' (Left (x,y)) = (Left x, y)
factor' (Right (x,y)) = (Right x, y)

-- A differentiable computation is a function to do the forward computation
-- and a differentiable computation that computes the sensitivity at the input
-- given the input and the sensitivity at the output.
data a ~> b = AD (a -> b) ((a,b) ~> a)

-- instance CartesianCategory (~>) where
--    ... won't fly due to Plus type constraints everywhere

id ::  Plus a => a ~> a
id = AD id' snd

(>>) :: (Plus a, Plus b, Plus c) => (a ~> b) -> (b ~> c) -> (a ~> c)
ff@(AD f df) >> AD g dg = AD (g . f) (first (fork >> second ff) >> assocr >> second dg >> df)

intro x = AD (intro' x) fst
elim :: Plus a => a ~> ()
elim  = AD elim' (snd >> intro zero)

fork :: Plus a => a ~> (a,a)
fork = AD fork' (snd >> add)

add :: Plus a => (a,a) ~> a
add = AD plus (snd >> fork)

uniti :: Plus a => a ~> ((),a)
uniti = AD uniti' (snd >> snd)

unite :: Plus a => ((),a) ~> a
unite = AD unite' (snd >> uniti) -- or AD unite (first fst)

fst :: (Plus a, Plus b) => (a,b) ~> a
fst = AD fst' (snd >> uniti >> first (intro zero) >> swap)

snd :: (Plus a, Plus b) => (a,b) ~> b
snd = AD snd' (snd >> uniti >> first (intro zero))

swap :: (Plus a, Plus b) => (a,b) ~> (b,a)
swap = AD swap' (snd >> swap)

assocl :: (Plus a, Plus b, Plus c) => (a,(b,c)) ~> ((a,b),c)
assocl = AD assocl' (snd >> assocr)

assocr :: (Plus a, Plus b, Plus c) => ((a,b),c) ~> (a,(b,c))
assocr = AD assocr' (snd >> assocl)

first :: (Plus a, Plus b, Plus c) => (a ~> b) -> (a,c) ~> (b,c)
first (AD f df) = AD (first' f) (first fst >> assocl >> first df)

second :: (Plus a, Plus b, Plus c) => (a ~> b) -> (c,a) ~> (c,b)
second (AD f df) = AD (second' f) (first snd >> swap >> assocr >> second (swap >> df))

f ** g = first f >> second g
f && g = fork >> (f ** g)

join :: (Plus a) => Either a a ~> a
join = AD join' (distrib >> (snd ++ snd)) 

mirror :: (Plus a, Plus b) => Either a b ~> Either b a
mirror = AD mirror' (snd >> mirror)

inl :: (Plus a, Plus b) => a ~> Either a b
inl = AD inl' (snd >> unleft)

inr :: (Plus a, Plus b) => b ~> Either a b
inr = AD inr' (snd >> unright)

unright :: (Plus a, Plus b) => Either a b ~> b
unright = AD (\(Right x) -> x) (snd >> inr)

unleft :: (Plus a, Plus b) => Either a b ~> a
unleft = AD (\(Left x) -> x) (snd >> inl)

left (AD f df) = AD (left' f) fst
right (AD f df) = AD (right' f) fst

coassocl :: (Plus a, Plus b, Plus c) => Either a (Either b c) ~> Either (Either a b) c
coassocl = AD coassocl' (snd >> coassocr)

coassocr :: (Plus a, Plus b, Plus c) => Either (Either a b) c ~> Either a (Either b c)
coassocr = AD coassocr' (snd >> coassocl)

f ++ g = left f >> right g
f || g = (f ++ g) >> join

distrib :: (Plus a, Plus b, Plus c) => (Either a b, c) ~> Either (a,c) (b,c)
distrib = AD distrib' (snd >> factor)

factor :: (Plus a, Plus b, Plus c) =>  Either (a,c) (b,c) ~> (Either a b, c)
factor  = AD factor' (snd >> distrib)

mul :: (Num a, Plus a) => (a,a) ~> a
mul = AD (uncurry (Prelude.*)) 
         (second fork 
          >> assocl 
          >> first (assocr >> second mul >> swap) 
          >> assocr >> second mul)

scale :: (Num a, Plus a) => a -> a ~> a
scale k = AD (Prelude.* k) (snd >> scale k)

neg :: (Num a, Plus a) => a ~> a
neg = scale (-1)

pow 0 = elim >> intro 1
pow 1 = id
pow k = AD (Prelude.** k) (first (pow (k-1) >> scale k) >> mul)

exp :: (Floating a, Plus a) => a ~> a
exp = AD Prelude.exp (first exp >> mul) 

log :: (Floating a, Plus a) => a ~> a
log = AD Prelude.log (first (pow (-1)) >> mul)

grad (AD f df) = uniti >> first (intro 1.0) >> swap >> df

x |> AD f df = f x
