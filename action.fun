(* the least fixed point of a family of nominal sets *)
functor FixFamily (F : FAMILY) :
sig
  datatype mu = IN of mu F.t
  include ACTION where type t = mu
end =
struct
  structure Perm = F.Perm
  datatype mu = IN of mu F.t
  type t = mu

  fun act pi (IN x) =
    IN (F.act (dict ()) pi x)
  and supp (IN x) =
    F.supp (dict ()) x
  and dict () =
    {act = act, supp = supp}
end

functor ApplyFamily
  (structure F : FAMILY
   structure X : ACTION
   sharing type F.Perm.t = X.Perm.t
   sharing type F.Perm.atom = X.Perm.atom) : ACTION =
struct
  structure Perm = F.Perm
  type t = X.t F.t

  val dict =
    {act = X.act, supp = X.supp}

  val act = F.act dict
  val supp = F.supp dict
end

functor UnitAction (Perm : PERM) : ACTION =
struct
  structure Perm = Perm
  type t = unit

  fun act _ _ = ()
  fun supp _ = []
end

(* the nominal set of atoms *)
functor AtomAction (Perm : PERM) : ACTION =
struct
  structure Perm = Perm
  type t = Perm.atom

  fun act pi = Perm.app pi

  fun supp a =
    [a]
end

(* the constant family of nominal sets *)
functor ConstActionF (X : ACTION) : FAMILY =
struct
  structure Perm = X.Perm
  type 'a t = X.t
  type 'a action =
    {act : Perm.t -> 'a -> 'a,
     supp : 'a -> Perm.atom list}

  fun act _ = X.act
  fun supp _ = X.supp
end

(* the pointwise product of two families of nominal sets *)
functor ProductActionF
  (structure F : FAMILY
   structure G : FAMILY
   sharing type F.Perm.t = G.Perm.t
   sharing type F.Perm.atom = G.Perm.atom) : FAMILY =
struct
  structure Perm = F.Perm

  type 'a t = 'a F.t * 'a G.t
  type 'a action = 'a F.action

  fun act dict pi (x, y)=
    (F.act dict pi x, G.act dict pi y)

  fun supp dict (x, y) =
    F.supp dict x @ G.supp dict y
end

structure Sum =
struct
  datatype ('a, 'b) t =
      INL of 'a
    | INR of 'b
end

(* the pointwise sum of two families of nominal sets *)
functor SumActionF
  (structure F : FAMILY
   structure G : FAMILY
   sharing type F.Perm.t = G.Perm.t
   sharing type F.Perm.atom = G.Perm.atom) : FAMILY =
struct
  structure Perm = F.Perm
  type 'a t = ('a F.t, 'a G.t) Sum.t
  type 'a action = 'a F.action

  fun act dict pi (Sum.INL x) = Sum.INL (F.act dict pi x)
    | act dict pi (Sum.INR y) = Sum.INR (G.act dict pi y)

  fun supp dict (Sum.INL x) = F.supp dict x
    | supp dict (Sum.INR y) = G.supp dict y
end

functor AbstractionF (P : PERM) : FAMILY =
struct
  structure Perm = P
  type 'a t = P.atom * 'a
  type 'a action =
    {act : P.t -> 'a -> 'a,
     supp : 'a -> P.atom list}

  fun act (dict : 'a action) pi (a, x) =
   (P.app pi a, #act dict pi x)

  fun supp (dict : 'a action) (a, x) =
    List.filter (fn b => not (P.eq (a, b))) (#supp dict x)
end

functor IdActionF (P : PERM) : FAMILY =
struct
  structure Perm = P
  type 'a t = 'a
  type 'a action =
    {act : P.t -> 'a -> 'a,
     supp : 'a -> P.atom list}

  fun act (dict : 'a action) pi x =
   #act dict pi x

  fun supp (dict : 'a action) x =
    #supp dict x
end
