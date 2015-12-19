signature PERM =
sig
  include GROUP

  type atom
  val eq : atom * atom -> bool
  val swap : atom * atom -> t
  val app : t -> atom -> atom
end
