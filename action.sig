(* a nominal set *)
signature ACTION =
sig
  structure Perm : PERM

  type t
  val act : Perm.t -> t -> t
  val supp : t -> Perm.atom list
end

(* a family of nominal sets *)
signature FAMILY =
sig
  structure Perm : PERM

  type 'a action =
    {act : Perm.t -> 'a -> 'a,
     supp : 'a -> Perm.atom list}

  type 'a t
  val act : 'a action -> Perm.t -> 'a t -> 'a t
  val supp : 'a action -> 'a t -> Perm.atom list
end
