  $ melitte -v t1.melitte
  {- Raw code -}
  val id : ∀ (A : 𝕌 0) → A → A = λ A x ⇒ x
  {- Elaborated code -}
  val id : ∀ (A : 𝕌 0) (_1 : A) → A = λ A x ⇒ x
  $ melitte -v t2.melitte
  {- Raw code -}
  val c : ℕ → ℕ = λ n ⇒ n
  eval suc c 0
  val add : ℕ → ℕ → ℕ = λ n m ⇒ elim n with _ ⇒ ℕ { zero ⇒ m | suc _, r ⇒ suc r }
  eval add 2 5
  val mul : ℕ → ℕ → ℕ =
    λ n m ⇒ elim n with _ ⇒ ℕ { zero ⇒ 0 | suc _, r ⇒ add m r }
  eval mul 3 10
  {- Elaborated code -}
  val c : ∀ (_0 : ℕ) → ℕ = λ n ⇒ n
  eval (1 : ℕ)
  val add : ∀ (_1 : ℕ) (_2 : ℕ) → ℕ =
    λ n m ⇒ elim n with _3 ⇒ ℕ { zero ⇒ m | suc _3, r ⇒ suc r }
  eval (7 : ℕ)
  val mul : ∀ (_2 : ℕ) (_3 : ℕ) → ℕ =
    λ n m ⇒ elim n with _4 ⇒ ℕ { zero ⇒ 0 | suc _4, r ⇒ add m r }
  eval (30 : ℕ)
  $ melitte -v t3.melitte
  File "t3.melitte", line 1, characters 19-20: syntax error
  [1]
  $ melitte -v simple.melitte
  {- Raw code -}
  val x : 𝕌 0 = ℕ
  val three : x = 3
  val const : ℕ → ℕ = λ x ⇒ 0
  val id : ∀ (A : 𝕌 0) → A → A = λ A x ⇒ x
  val iter : ∀ (A : 𝕌 0) (f : A → A) → A → ℕ → A =
    λ A f z n ⇒ elim n with _ ⇒ A { zero ⇒ z | suc _, r ⇒ f r }
  val add : ℕ → ℕ → ℕ = iter ℕ (λ n ⇒ suc n)
  val mul : ℕ → ℕ → ℕ = λ n ⇒ iter ℕ (add n) 0
  val exp : ℕ → ℕ → ℕ = λ n ⇒ iter ℕ (mul n) 1
  {- Elaborated code -}
  val x : 𝕌 0 = ℕ
  val three : x = 3
  val const : ∀ (_2 : ℕ) → ℕ = λ x ⇒ 0
  val id : ∀ (A : 𝕌 0) (_4 : A) → A = λ A x ⇒ x
  val iter : ∀ (A : 𝕌 0) (f : ∀ (_5 : A) → A) (_6 : A) (_7 : ℕ) → A =
    λ A f z n ⇒ elim n with _8 ⇒ A { zero ⇒ z | suc _8, r ⇒ f r }
  val add : ∀ (_5 : ℕ) (_6 : ℕ) → ℕ = iter ℕ (λ n ⇒ suc n)
  val mul : ∀ (_6 : ℕ) (_7 : ℕ) → ℕ = λ n ⇒ iter ℕ (add n) 0
  val exp : ∀ (_7 : ℕ) (_8 : ℕ) → ℕ = λ n ⇒ iter ℕ (mul n) 1

Melitte contains a hierarchy of universes.

  $ melitte t4.melitte
  File "t4.melitte", line 1, characters 23-26: universe inconsistency
  [1]

Melitte contains Σ types.

  $ melitte t5.melitte
