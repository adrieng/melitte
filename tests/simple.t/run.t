  $ melitte simple.melitte
  [(Raw.Val ("x", Raw.Type, Raw.Nat));
    (Raw.Val ("three", (Raw.Var "x"),
       (Raw.App (Raw.Succ, (Raw.App (Raw.Succ, (Raw.App (Raw.Succ, Raw.Zero))))
          ))
       ));
    (Raw.Val ("const", (Raw.Forall (Raw.Nat, (Raw.PWildcard, Raw.Nat))),
       (Raw.Lam ((Raw.PVar "x"), Raw.Zero))));
    (Raw.Val ("id",
       (Raw.Forall (Raw.Type,
          ((Raw.PVar "A"),
           (Raw.Forall ((Raw.Var "A"), (Raw.PWildcard, (Raw.Var "A")))))
          )),
       (Raw.Lam ((Raw.PVar "A"), (Raw.Lam ((Raw.PVar "x"), (Raw.Var "x")))))));
    (Raw.Val ("iter",
       (Raw.Forall (Raw.Type,
          ((Raw.PVar "A"),
           (Raw.Forall (
              (Raw.Forall ((Raw.Var "A"), (Raw.PWildcard, (Raw.Var "A")))),
              ((Raw.PVar "f"),
               (Raw.Forall (Raw.Nat,
                  (Raw.PWildcard,
                   (Raw.Forall ((Raw.Var "A"), (Raw.PWildcard, (Raw.Var "A")))))
                  )))
              )))
          )),
       (Raw.Lam
          ((Raw.PVar "A"),
           (Raw.Lam
              ((Raw.PVar "f"),
               (Raw.Lam
                  ((Raw.PVar "n"),
                   (Raw.Lam
                      ((Raw.PVar "z"),
                       Raw.Natelim {discr = (Raw.Var "n"); motive = None;
                         case_zero = (Raw.Var "z");
                         case_succ =
                         ((Raw.PVar "r"),
                          (Raw.App ((Raw.Var "f"), (Raw.Var "r"))))}))))))))
       ));
    (Raw.Val ("add",
       (Raw.Forall (Raw.Nat,
          (Raw.PWildcard, (Raw.Forall (Raw.Nat, (Raw.PWildcard, Raw.Nat)))))),
       (Raw.App ((Raw.Var "iter"), (Raw.App (Raw.Nat, Raw.Succ))))));
    (Raw.Val ("mul",
       (Raw.Forall (Raw.Nat,
          (Raw.PWildcard, (Raw.Forall (Raw.Nat, (Raw.PWildcard, Raw.Nat)))))),
       (Raw.App ((Raw.Var "iter"), (Raw.App (Raw.Nat, (Raw.Var "add")))))));
    (Raw.Val ("exp",
       (Raw.Forall (Raw.Nat,
          (Raw.PWildcard, (Raw.Forall (Raw.Nat, (Raw.PWildcard, Raw.Nat)))))),
       (Raw.App ((Raw.Var "iter"), (Raw.App (Raw.Nat, (Raw.Var "mul")))))))
    ]
