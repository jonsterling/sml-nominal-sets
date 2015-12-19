signature CATEGORY =
sig
  type ('a, 'b) hom
  val idn : ('a, 'a) hom
  val cmp : ('b, 'c) hom -> ('a, 'b) hom -> ('a, 'c) hom
end
