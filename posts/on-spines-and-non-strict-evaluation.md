---
author: Jordi Aranda
date: 2015-12-28 17:30:00
title: On spines and non-strict evaluation
tags: haskell, lazyness
---
# On spines and non-strict evaluation

Understanding lists is a very important topic in Haskell. Let's review the `[]` datatype definition first (defined in `GHC.Types`):

```haskell
data [] a = [] | a : [a]
```

The list type is defined as a sum type (please notice the `|` within the definition). `[]` is the type constructor for lists as well as the data constructor for the empty list (i.e. it is a nullary constructor because it takes no arguments). The second data constructor has two arguments and uses the `cons` (`:`) infix operator: a value of type `a` and a list of type `[a]`, which evaluates to `[a]`. So we can actually see lists as a recursive series of cons cells `a : [a]` terminated by the empty list `[]`. 

<!--more-->
Haskell provides syntactic sugar when building new lists. Given the following expression: `[1,2,3,4]`, it gets translated into:

```haskell
1 : 2 : 3 : 4 : []
```

The `cons` infix operator associates to the right. You can check this, and more generally, any operator associativity with the corresponding precedence level, by using the `:info` (or `:i`) command within the `ghc` REPL:

<pre>
Prelude> :i (:)
data [] a = ... | a : [a]       -- Defined in ‘GHC.Types’
infixr 5 :
</pre>

When talking about data structures, we talk about them having a `spine`. We can see the `spine` as the glue which ties the values together. In the case of lists, it is the `cons` operator. Because of this and the way non-strict evaluation works, cons cells can be evaluated independently of what value they contain. This means we can evaluate the spine of a list no matter what individual values are present. It is actually possible to evaluate only part of the spine and not the rest of it.

It is important to differentiate between *evaluation* and *construction*. Evaluation of the list proceeds down the spine, while construction proceeds up the spine. Let's represent the list `[1,2,3,4]` graphically:

<pre>
  :
 / \
1   :
   / \
  2   :
     / \
    3   :
       / \
      4  []
</pre>

If we use the above representation, when we construct such list, we first put the 4 into the empty list, then we add 3 to the front of that list, and so on, while the evaluation starts the other way around, from 1 downward to 4 and the empty list.

Why is all this important? Because many functions are only strict in the spine, meaning they only force evaluation of the spine but not the values themselves. This is easy to check with the following example:

<pre>
Prelude> let x = [1, 2, 3, undefined]
</pre>

`undefined` can be seen as a [*bottom*](https://wiki.haskell.org/Bottom) value. The term *bottom* refers to a computation which never completes successfully. That includes any kind of failure or an infinite loop. How come we can define such expression in Haskell? Well, this is how [*lazy evaluation*](https://wiki.haskell.org/Lazy_evaluation) works :) Expressions are not evaluated when they are bound to variables, but their evaluation is deferred until their results are needed by other computations. In consequence, arguments are not evaluated before they are passed to a function, but only when their values are actually used.

We could be interested in the length of the list `x`. For this we might use the `length` function:

<pre>
Prelude> length x
4
</pre>

As we can see, computing the length of the list caused no error, although `undefined` is a value present in the list. Let's take the first two list values instead:

<pre>
Prelude> take 2 x
[1, 2]
</pre>

Again, no error popped up. As we may infer from the proceeding examples, taking the last element of the list (which forces evaluation of all list elements) will, in this case, throw an error.

<pre>
Prelude> last x
*** Exception: Prelude.undefined
</pre>

So as we have seen, spines are evaluated independently of values. This relates to [*weak head normal form*](https://wiki.haskell.org/Weak_head_normal_form), which I might talk about in another blog entry.
