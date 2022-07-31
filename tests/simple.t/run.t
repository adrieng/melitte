  $ melitte simple.melitte
  {- Raw source code -}
  val x : 𝕌 = ℕ
  val three : x = succ succ succ zero
  val const : ℕ → ℕ = λ x ⇒ zero
  val id : ∀ (A : 𝕌) → A → A = λ A x ⇒ x
  val iter : ∀ (A : 𝕌) (f : A → A) → ℕ → A → A =
    λ A f n z ⇒ elim n { zero ⇒ z | r ⇒ f r}
  val add : ℕ → ℕ → ℕ = iter ℕ (λ x ⇒ succ x)
  val mul : ℕ → ℕ → ℕ = iter ℕ add
  val exp : ℕ → ℕ → ℕ = iter ℕ mul
  val exp_n : ℕ → 𝕌 =
    let exp_once : 𝕌 → 𝕌  = λ t ⇒ ℕ → t in iter (ℕ → 𝕌) exp_once n ℕ
  {- Elaborated source code -}
