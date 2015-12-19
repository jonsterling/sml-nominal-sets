functor VarF (P : PERM) : FAMILY =
  ConstActionF (AtomAction (P))

functor LamOpF (P : PERM) : FAMILY =
  AbstractionF (P)

functor AppOpF (P : PERM) : FAMILY =
  ProductActionF
    (structure F = IdActionF (P)
     structure G = IdActionF (P))

functor LambdaSigF (P : PERM) : FAMILY =
  SumActionF
    (structure F = VarF (P)
     structure G =
       SumActionF
         (structure F = LamOpF (P)
          structure G = AppOpF (P)
         ))

functor LambdaSig (P : PERM) : ACTION =
  FixFamily (LambdaSigF (P))
