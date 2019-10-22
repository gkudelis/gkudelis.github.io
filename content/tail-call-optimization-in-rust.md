+++
title = "Tail Call Optimisation in Rust"
draft = true
[taxonomies]
tags = ["rust", "recursion"]
+++

When developing a recursive algorithm regardless of the language one of the
issues you as a programmer have to consider is not causing a stack overflow.
In some cases you might know that the recursion won't be that deep, but there
are perfectly good algorithms that can be expressed in recursive form that
can and will consume all of the stack memory and then cause a crash.

There is a way to solve this - by rewriting the algorithm so that the recursive
call is the last instruction in the function and the result of it is then
returned unchanged you can execute this without adding a new stack frame. This
is called tail call optimisation and if the language/compiler you're using
supports it - you're in luck. Unfortunately, the Rust compiler doesn't support
tail call optimisation in the general case. In this post I will be looking at
how you can restructure your recursive functions using loops to avoid causing a
stack overflow and why this is effectively the same as performing tail call
optimisation.

### Simple recursive algorithm

First we'll need a simple and common recursive algorithm to use as an example.
One of the easiest things that make sense to implement recursively is the `all`
function that takes an iterator and a predicate function and checks that all
the items in the iterator match the predicate.

```rust
fn all<I, P>(mut items: I, predicate: P) -> bool
where
    I: Iterator,
    P: Fn(I::Item) -> bool,
{
    unimplemented!()
}
```

The recursive implementation of `all` is simple - if the predicate is false for
the next item of the iterator then `all` should return false. If it is true
then we need to repeat the same action on the rest of the iterator until the
iterator is empty, at which point we can say that the predicate is true for
all the items.

```rust
fn all<I, P>(mut items: I, predicate: P) -> bool
where
    I: Iterator,
    P: Fn(I::Item) -> bool,
{
    match items.next() {
        Some(item) => {
            if predicate(item) {
                all(items, predicate)
            } else {
                false
            }
        }
        None => true,
    }
}
```

It is clear that in this case `all` could take advantage of tail-call
optimisation as the result of the recursive call is returned as-is.

So what's the issue here? Let's say we want to test a very long (potentially
infinite) sequence. Every time `all` calls itself it creates a stack frame that
consumes memory. Eventually, that leads to a stack overflow. A quick test to
show it failing:


```rust
#[test]
fn long_sequence() {
    assert!(!all(0.., |x| x < 1_000_000));
}
```

This fails on my machine, but if it runs fine on yours then it's always
possible to make it crash just by increasing the cut-off number (or removing it
altogether and making the closure always return true).

### 

Write:
- intro
- what is TCO?
- describe and implement all() - show how it fails
- re-implement using loop - show how it doesn't fail
- explain how to use the short-circuiting && operator to get rid of the if
- this can also be useful when creating iterators using from_fn or repeat_with
  since you can't use recursion inside a closure
