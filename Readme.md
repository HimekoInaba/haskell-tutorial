# Haskell tutorial for beginners
## Prerequisites
 * You can code on one of the imperative programming languages (C, C++, Python, Java, ...)
 * You have installed Haskell Toolchain (ghc, ghci, caabal)

## Content
1. [Introduction](#intro)
2. [Basics](https://github.com/HimekoInaba/haskell-tutorial/blob/master/basics.hs)
3. [Lists](https://github.com/HimekoInaba/haskell-tutorial/blob/master/lists.hs)

<h2 id="intro">Introduction</h2>

### What is Functional Programming?
 - Pure functions - aka mathematical functions, they can not have side effects.
 - Immutable data - values cannot be reassigned.
 - Declarative    - express the logic instead of control flow (for example, SQL is declarative). 
 - Easy to verify - you can mathematically prove the correctnes of an algorithm.

### Declarative vs Imperative
***Imperative***   
Describe an algorithm how to sum an array. You give an instructions to machiene.  
***Java***  
```java
int sum (int[] arr) {
    int sum = 0;
    for (int i = 0; i < arr.length; i++) {
        sum += arr[i];
    }
    return sum;
}
```

***Declarative***   
Define what it means to have a sum.  
***Haskell***  
```haskell
sum [] = 0
sum (x:xs) = x + sum xs
```
### Lazy vs Strict
Consider scenario: we have 3 functions: func1, func2, func3. Every functions needs 1 hour to run.

Haskell is lazily evaluated.
When variables are defined, they are not evaluated. They evaluates only when they are used.
In the code below, only x or y is evaluated, depending on z, but never both.
It means that our code will run in 2 hours.  
***Haskell***  
```haskell
evaluate arg =
    let x = func1 arg
        y = func2 arg
        z = func3 arg
    in
    if z then x else y
```

***Strict evaluation***  
Variables are evaluated immediately. It means when we call func on an assignment, will hold the evaluated value of the func. In our scenario, our code will run in 3 hours. Even though, we will use only x or y as a return value.  
***Java***  
```java
int evaluate(int arg) {
    int x = func1(arg);
    int y = func2(arg);
    int z = func3(arg);

    if (z) {
        return x;
    } else {
        return y;
    }
}
```
