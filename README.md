# NaturalDeduction
A natural deduction prover in Gentzen's system NI. Note that this means that the rule `¬¬X → X` is not included.
The output can be rendered in LaTeX using the [lkproof](https://ctan.org/pkg/lkproof) package.

## Example usage
Suppose you want a proof for `A0, A1, A2 ⊢ A0 ∧ A1 ∧ A3 → (A2 ∨ ⊥)`. You can then run the following:
```haskell
$ ghci Main.hs
> sequence_ $ printDeductionTree <$> proof ((Atom 0 /\ Atom 1) /\ (Atom 3 --> (Atom 2 \/ Falsum))) [Atom 0, Atom 1, Atom 2]
\infer
    {A_0 \land A_1 \land A_3 \rightarrow (A_2 \lor \bot)}
    {
        \infer
            {A_0 \land A_1}
            {
                A_0
                &
                A_1
            }
        &
        \infer[^0]
            {A_3 \rightarrow (A_2 \lor \bot)}
            {
                \infer
                    {A_2 \lor \bot}
                    {
                        \infer
                            {A_2}
                            {
                                [A_3]^0
                            }
                    }
            }
    }
```
The rendered output is:

![Rendered output](https://i.gyazo.com/2403a017c5c0368eca887b825cd6c323.png)

## TODO
 - [x] Introduction rules
 - [x] Elimination rules
 - [ ] Extend to first order propositional logic