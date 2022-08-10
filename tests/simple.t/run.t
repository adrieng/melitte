  $ melitte -v t1.melitte
  {- Raw code -}
  val c1 : ℕ → ℕ = λ n ⇒ n
  val id : ∀ (A : 𝕌) → A → A = λ A x ⇒ x
  val abs : 𝕌 = ∀ (A : 𝕌) → A
  val c2 : ℕ → ℕ = id (ℕ → ℕ) (λ n ⇒ n)
  {- Elaborated code -}
  val c1 : ∀ (_x0 : ℕ) → ℕ = λ n ⇒ n
  val id : ∀ (A : 𝕌) (_x2 : A) → A = λ A x ⇒ x
  val abs : 𝕌 = ∀ (A : 𝕌) → A
  val c2 : ∀ (_x3 : ℕ) → ℕ = id (∀ (_x3 : ℕ) → ℕ) (λ n ⇒ n)
  $ melitte -v t2.melitte
  {- Raw code -}
  val c : ℕ → ℕ = λ n ⇒ n
  eval suc c 0 : ℕ
  val add : ℕ → ℕ → ℕ = λ n m ⇒ elim n with _ ⇒ ℕ { zero ⇒ m | suc _, r ⇒ suc r }
  eval add 2 5 : ℕ
  val mul : ℕ → ℕ → ℕ =
    λ n m ⇒ elim n with _ ⇒ ℕ { zero ⇒ 0 | suc _, r ⇒ add m r }
  eval mul 3 10 : ℕ
  {- Elaborated code -}
  val c : ∀ (_x0 : ℕ) → ℕ = λ n ⇒ n
  eval 1 : ℕ
  val add : ∀ (_x1 : ℕ) (_x2 : ℕ) → ℕ =
    λ n m ⇒ elim n with _x3 ⇒ ℕ { zero ⇒ m | suc _x3, r ⇒ suc r }
  eval 7 : ℕ
  val mul : ∀ (_x2 : ℕ) (_x3 : ℕ) → ℕ =
    λ n m ⇒ elim n with _x4 ⇒ ℕ { zero ⇒ 0 | suc _x4, r ⇒ add m r }
  eval 30 : ℕ
  $ melitte -v t3.melitte
  {- Raw code -}
  val iter : ∀ (A : 𝕌) (f : A → A) → ℕ → A → A = λ A f n z ⇒ z
  val c : ℕ → ℕ → ℕ = iter ℕ (λ m ⇒ m)
  {- Elaborated code -}
  val iter : ∀ (A : 𝕌) (f : ∀ (_x1 : A) → A) (_x2 : ℕ) (_x3 : A) → A =
    λ A f n z ⇒ z
  val c : ∀ (_x1 : ℕ) (_x2 : ℕ) → ℕ = iter ℕ (λ m ⇒ m)
  $ melitte -v simple.melitte
  {- Raw code -}
  val x : 𝕌 = ℕ
  val three : x = 3
  val const : ℕ → ℕ = λ x ⇒ 0
  val id : ∀ (A : 𝕌) → A → A = λ A x ⇒ x
  val iter : ∀ (A : 𝕌) (f : A → A) → A → ℕ → A =
    λ A f z n ⇒ elim n with _ ⇒ A { zero ⇒ z | suc _, r ⇒ f r }
  val add : ℕ → ℕ → ℕ = iter ℕ (λ n ⇒ suc n)
  val mul : ℕ → ℕ → ℕ = λ n ⇒ iter ℕ (add n) 0
  val exp : ℕ → ℕ → ℕ = λ n ⇒ iter ℕ (mul n) 1
  {- Elaborated code -}
  val x : 𝕌 = ℕ
  val three : x = 3
  val const : ∀ (_x2 : ℕ) → ℕ = λ x ⇒ 0
  val id : ∀ (A : 𝕌) (_x4 : A) → A = λ A x ⇒ x
  val iter : ∀ (A : 𝕌) (f : ∀ (_x5 : A) → A) (_x6 : A) (_x7 : ℕ) → A =
    λ A f z n ⇒ elim n with _x8 ⇒ A { zero ⇒ z | suc _x8, r ⇒ f r }
  val add : ∀ (_x5 : ℕ) (_x6 : ℕ) → ℕ = iter ℕ (λ n ⇒ suc n)
  val mul : ∀ (_x6 : ℕ) (_x7 : ℕ) → ℕ = λ n ⇒ iter ℕ (add n) 0
  val exp : ∀ (_x7 : ℕ) (_x8 : ℕ) → ℕ = λ n ⇒ iter ℕ (mul n) 1
