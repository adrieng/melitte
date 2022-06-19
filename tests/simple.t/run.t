  $ melitte simple.melitte
  [(Raw.Val ("x", Raw.Type, Raw.Nat));
    (Raw.Val ("three", (Raw.Var "x"),
       (Raw.App (Raw.Succ, (Raw.App (Raw.Succ, (Raw.App (Raw.Succ, Raw.Zero))))
          ))
       ));
    (Raw.Val ("const", (Raw.Forall (Raw.Nat, (None, Raw.Nat))),
       (Raw.Lam ((Some "x"), Raw.Zero))));
    (Raw.Val ("id",
       (Raw.Forall (Raw.Type,
          ((Some "A"), (Raw.Forall ((Raw.Var "A"), (None, (Raw.Var "A"))))))),
       (Raw.Lam ((Some "A"), (Raw.Lam ((Some "x"), (Raw.Var "x")))))));
    (Raw.Val ("iter",
       (Raw.Forall (Raw.Type,
          ((Some "A"),
           (Raw.Forall ((Raw.Forall ((Raw.Var "A"), (None, (Raw.Var "A")))),
              ((Some "f"),
               (Raw.Forall (Raw.Nat,
                  (None, (Raw.Forall ((Raw.Var "A"), (None, (Raw.Var "A"))))))))
              )))
          )),
       (Raw.Lam
          ((Some "A"),
           (Raw.Lam
              ((Some "f"),
               (Raw.Lam
                  ((Some "n"),
                   (Raw.Lam
                      ((Some "z"),
                       Raw.Natelim {discr = (Raw.Var "n"); motive = None;
                         case_zero = (Raw.Var "z");
                         case_succ =
                         ((Some "r"), (Raw.App ((Raw.Var "f"), (Raw.Var "r"))))}))))))))
       ));
    (Raw.Val ("add",
       (Raw.Forall (Raw.Nat, (None, (Raw.Forall (Raw.Nat, (None, Raw.Nat)))))),
       (Raw.App ((Raw.Var "iter"), (Raw.App (Raw.Nat, Raw.Succ))))));
    (Raw.Val ("mul",
       (Raw.Forall (Raw.Nat, (None, (Raw.Forall (Raw.Nat, (None, Raw.Nat)))))),
       (Raw.App ((Raw.Var "iter"), (Raw.App (Raw.Nat, (Raw.Var "add")))))));
    (Raw.Val ("exp",
       (Raw.Forall (Raw.Nat, (None, (Raw.Forall (Raw.Nat, (None, Raw.Nat)))))),
       (Raw.App ((Raw.Var "iter"), (Raw.App (Raw.Nat, (Raw.Var "mul")))))))
    ]
