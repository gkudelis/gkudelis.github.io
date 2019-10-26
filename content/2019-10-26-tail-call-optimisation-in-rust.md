+++
title = "Tail Call Optimisation in Rust"
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

### A simple recursive algorithm

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

### Tail call optimisation using `loop`

Let's see what we can do to fix this. In case of `all` the solution is quite
obvious - we can rewrite it using a loop. We can simply loop until the iterator
is exhausted or we find an item where the predicate is false. To make it clear
how this maps to the recursive style let's change the `match` in the first
version to a `break match` and wrap it inside a `loop`. That way if the selected
`match` branch returns a value we break out of the loop and return from
our function. If on the other hand it would make a recursive call we can
instead simply modify the state and use `continue` to start a new iteration
of the loop. When rewriting a recursive function to use a loop this is key -
instead of passing modified arguments to a recursive call we change the state
and start a new iteration of the loop.

In the case of our `all` function we don't need to modify the state explicitly,
this is done for us by the `next` method. If the predicate is true we move
on to the next iteration using `continue` and if it's false we simply return
`false`, which breaks the loop and is used as the return value of the function.
```rust
pub fn all<I, P>(mut items: I, predicate: P) -> bool
where
    I: Iterator,
    P: Fn(I::Item) -> bool,
{
    loop {
        break match items.next() {
            Some(item) => {
                if predicate(item) {
                    continue;
                } else {
                    false
                }
            }
            None => true,
        };
    }
}
```
If you try running the `long_sequence` test again you'll see that it no longer
fails. However, `continue` is not often used and I feel that many people are
more used to using `break` statements. We can rewrite the same tail-call
optimised function by switching back from `break match` to a regular `match` -
this way simply returning a value in a `match` branch does nothing, the loop
continues. Instead we need to use a `break` statement to return a value from
the loop and from our functions. The `continue` statement is no longer needed
as the default behavior is to continue with the loop.
```rust
pub fn all<I, P>(mut items: I, predicate: P) -> bool
where
    I: Iterator,
    P: Fn(I::Item) -> bool,
{
    loop {
        match items.next() {
            Some(item) => {
                if !predicate(item) {
                    break false;
                }
            }
            None => break true,
        };
    }
}
```
Note that since the `None` branch of the `match` is of unit type we also don't
need the if to return any value either. This way we can get rid of one of the
branches and only keep the one with the `break` statement.

### Short-circuiting logic operators

While this works, we can do away with the `if` statement - in Rust both `||`
and `&&` operators are short-circuiting, so we can use them as simple forms of
flow control. In this case we want to `break false` when the predicate is false
for the current item, so we can instead write:
```rust
pub fn all<I, P>(mut items: I, predicate: P) -> bool
where
    I: Iterator,
    P: Fn(I::Item) -> bool,
{
    loop {
        match items.next() {
            Some(item) => predicate(item) || break false,
            None => break true,
        };
    }
}
```
This looks much nicer and also more compact. But remember that we started by
using `continue` instead of `break`? We can use it in a similar fashion, but
since we only want to `continue` if the predicate is true we have to use `&&`
instead.
```rust
pub fn all<I, P>(mut items: I, predicate: P) -> bool
where
    I: Iterator,
    P: Fn(I::Item) -> bool,
{
    loop {
        break match items.next() {
            Some(item) => predicate(item) && continue,
            None => true,
        };
    }
}
```
This works just fine and passes the test, but I find the `&& continue` part
a lot less obvious than the correspondig `|| break true`. Overall I think that
while the `continue` style is helpful for understanding as the `continue`
maps directly to a recursive call in code, the `break` style should normally be
preferred.

Hopefully I've convinced you that while having in-built tail call optimisation
is nice it's really not that difficult to rewrite your recursive code using
loops thus making it a little more robust. There is also a performance benefit
and I'm planning to write another post about that, complete with some
benchmarks!
