signature GROUPOID =
sig
  include CATEGORY
  val inv : ('a, 'b) hom -> ('b, 'a) hom
end

signature GROUP =
sig
  type t
  include GROUPOID where type ('a, 'b) hom = t
end
