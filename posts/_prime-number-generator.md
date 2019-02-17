---
title: Generating prime numbers in Python
tags: python, generators
---

A while ago I read [this fantastic paper](https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf) by [Melissa E. O'Neill](https://www.cs.hmc.edu/~oneill/) where the author
explains how the algorithm that is often portrayed in introductory texts on
functional programming as the lazy functional implementation of Sieve of
Eratosthenes is actually not the sieve and is actually even worse than trial
division (testing numbers by checking if any prime already found divides them).
She then outlines and implements an algorithm that is actually a lazy and
functional sieve and uses that to show how important the choice of a suitable
datastructure can be for performance.

After reading the paper I wrote a quick implementation of the algorithm in
Clojure and forgot about it. I recently had to do some data processing at work.
I had to load some data, do some transformations and save it for later
analysis (classic ETL). I wasn't sure how much data there would be and so I
decided it's best to write my script in a lazy manner, so it wouldn't have to
fetch all the data before starting to process it. I was doing this in Python
and so had a chance to explore Python generators a little more in-depth (I've
somehow managed to ignore this really neat part of Python up until then) and
as I learned more about generator expressions and functions I realised that the
lazy algorithm for generating prime numbers can be expressed really easily and
clearly using generator functions.

In this post I want to quickly run through the basics of generator functions in
Python and explain how they can be used to create lazy sequences. Then I'll
explain how the trial division algorithm works and use it to create a generator
function for prime numbers. And lastly I'll go through the prime number
algorithm described by Prof. O'Neill, show how to implement it in Python and
make a quick performance comparison with the trial division algorithm.

### Generator functions

In Python generator functions are one of the ways to create iterators, which
is to say objects that have a `__next__` method. When you call the Python
inbuilt `next` function and pass it an iterator it in turn calls the `__next__`
method, which returs the next item in the iterator. This is done implicitly in
`for` loops and many other places in Python, so you don't often have to use
`next` in your own code. Mind you this is a bit of a simplification and if you'd
like a full view of the differences between iterables, iterators, and other
related interfaces you might want to check out [Iterables vs. Iterators vs.
Generators](https://nvie.com/posts/iterators-vs-generators/) by Vincent Driessen.

Generator functions offer a nice and flexible way for creating iterators
without the boilerplate of making one by creating a class. Instead, a generator
looks exactly like a function, except it has one or more `yield` statements.
When you call this function a generator object is returned (generators are also
iterators), but the function is not executed. Instead, once `next` is called on
the generator object the function actually runs. Once the execution reaches a
`yield` statement the argument is returned as the first item of the iterator
and execution is paused. The next time `next` is called the execution resumes
from the same point until the next `yield` statement is reached. When the
generator is finished it can either use an empty `return` statement or the
function can simply finish without one.

Let's write a generator that describes the Fibonacci sequence as a a simple
example. We first set up two variables to contain the two previous items of the
sequence and set them both to 1 as we know those are the first two items.
We can emit those immediately after and then enter an infinite loop where we
calculate the next item by adding the two previous items, emit the new item and
then update our values for the two previous items of the sequence.

    def fibonacci():
        a = 1
        b = 1
        yield a
        yield b
        while True:
            c = a + b
            yield c
            a = b
            b = c

### Trial division

### Lazy sieve

### Performance comparison
