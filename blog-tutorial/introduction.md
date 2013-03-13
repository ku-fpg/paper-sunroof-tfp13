
# Introduction to Sunroof

**RESOLVE TODOS!**

This is supposed to be a quick and dirty tutorial to 
give you a feeling for what can be done with Sunroof and where
to look to get started. If you need more detailed information 
look at [the sources][SunroofSource] and generate the documentation from them.

## Installation and Setup

To install Sunroof you have to check out [a few packages from GitHub][SunroofReadme].
Please, look into the provided readme to find further instruction.
It will be up to date.

Lets look at expressions in Sunroof first and then move on to monadic statements.

## Expressions: Basic building blocks

All types in Sunroof that represent a Javascript value and therefore
can form expressions in Javascript implement the class `Sunroof`.
Everything in this section works when importing the following packages:

    import Data.Default     -- Provides defaults for value
    import Data.Monoid      -- Monoids are important for strings
    import Data.Boolean     -- Overloaded booleans
    import Language.Sunroof -- All Sunroof functionality

If you want to play around with the compiler you can use this 
function to compile your expressions directly:

    test :: (Sunroof a) => a -> IO ()
    test o = sunroofCompileJS def "main" (return o) >>= putStrLn

Lets look at the basic types that are provided. First of all there
is unit. It can be thought of as an equivalent of void or `null`
in Javascript, because it indicates nothing of interest.

Of course, there is a boolean type `JSBool`. Sunroof uses the 
[`Data.Boolean`][BooleanPackage] package to overload booleans.
There are constants `true` and `false` as well as the 
usual operators `&&*`, `||*` or `notB`. Branches can be expressed 
using the `ifB` function. If you need a standard operator involving booleans 
usually you can just append a star (`*`) for the overloaded version. 
The overloaded version of a function can be found by appending `B`
to its name.

    ifB (true &&* false) (notB false) (false ||* true) :: JSBool

The type `JSNumber` represents Javascript numbers. They can be thought 
of as `Double` values. Thanks to the generality of the `Num` class 
numeric expressions can be written as usual.

    (3 + 4) * 5 :: JSNumber

Following the naming scheme mentioned above there are 
also overloadings of the operators `==`, `/=`, `<` and `>` 
(provided by the classes `EqB` and `OrdB`). Just append
a star to use them.

Lets look at an example for all this in Haskell:

    (x - 5 >= -10 && x * 2 <= 10) && (x /= 0 || not (y + 3 == x))

How would this look as a Javascript expression in Sunroof?

    (x - 5 >=* -10 &&* x * 2 <=* 10) &&* (x /=* 0 ||* notB (y + 3 ==* x))

What other types are there? `JSString` represents strings.
We did not overload operators like `++`, but `JSString` is a
instance of `Monoid`, therefore you can just use `<>` instead.

    "Your name is " <> name

In case you are wondering how we can use a Haskell string literal here:
To do that you need to activate the GHC language extension `OverloadedStrings`.
In case you do not want to do that, you can just use the `string`
conbintor to convert a Haskell string into a Sunroof string.

There also is `JSArray a` which can roughly be thought of an equivalent 
to `[a]`. You can create
your own array instances from lists using the `array` combinator.

    array [0 .. 5 :: Int]

This seems nice and type safe, does it not? 
But Javascript does not hava static typing you might say. 
Of course, you are right. In case you really need to convert types 
into each other Sunroof provides the `cast` function which can 
convert any Sunroof type into another one.

You will also encounter `JSObject`. There is no direct equivalent
to this type in Haskell. The closest you can get is `Sunroof a => Map String a`.
`JSObject` is important, because it represents everything that Sunroof
cannot represent. When using the Javascript APIs provided by Sunroof
you will often encounter `JSObject`.

To access the attributes of an object or entries of an array you
can use the `!` operator together with the combinators `index` or `label`.

    arr ! index 0
    obj ! label "name"

Now lets move on to statements!

## Statements: Sequential code

Sunroof provides a deep embedding of Javascript into Haskell.
All code written is structured in a monad to capture its sequential 
nature. Also a major difference between monadic statements and expression,
like we have seen in the previous section, is that expressions provided 
by Sunroof can be assumed to be free of side effects. Everything
inside a monadic statement may have a side effect in Javascript.
In fact binding itself is the most basic side effect, an assignment
to a variable.

The central monadic type in Sunroof is `JS t a`. The `t` type parameter 
represents the threading model. In the
scope of this introduction you can ignore the `t` type. 
For what we want to look into here, you can just use the short-hand `JSA a` 
instead of `JS t a`.

To get started with the `JS` monad lets look at a small example:

    example :: JSA ()
    example = do
      canvas <- document # getElementById "canvas"
      context <- canvas # getContext "2d"
      context # drawRect (10, 10) (100, 100)

This look pretty close to what you would write in Javascript, right?
It actually translates into what you would expect:

    var v0 = document.getElementById("canvas");
    var v1 = v0.getContext("2d");
    v1.drawRect(10,10,100,100);

The monadic binding (`<-`) can be thought of as assignment to a 
new variable (it is literally translated to that). Statements that 
produce unit as return value are not assigned to a variable, because
assigning `void` to a variable is not a useful thing to do.

The `#`-operator is analog to the `.`-operator, because we do not want to 
get in confict with function composition.

The `document` object is a `JSObject` provided by the `Language.Sunroof.JS.Browser`
module for convenience. This module also provides the `getElementById` function.

`getContext` and `fillRect` are part of the HTML5 canvas API which is
provided by `Language.Sunroof.JS.Canvas`.

To compile your `JS` monad you can use the `sunroofCompileJS` 
function.

    compileSunroofJS def "main" example

The first parameter contains the compiler options. Just
use the options provided by `def` (from `Data.Default`) to get started.
The second argument is the name of the variable the result
of your code is assigned to. The third and last parameter is the
`JS` monad you want to compile.
The example will produce the following result:

    var main = (function() {
      var v0 = (document).getElementById("canvas");
      var v1 = (v0).getContext("2d");
      (v1).fillRect(10,10,100,100);
    })();

All statements are wrapped into the local scope of a function. This
protects the global Javascript namespace from being polluted with all the 
new variables produced by Sunroof. Also it prevents us from getting in
conflict with global bindings. At the same time effects can escape the 
scope. If the compiled Sunroof has a interesting result it is returned
by the function. Looking back at the `test` function you can see how this
works.

This is all you need to know to get started writing your own Javascript
with Sunroof.

If you are interested in writing more complex application that require 
communication between client and server you should look into the 
`Language.Sunroof.KansasComet` package. It provides ready to use 
infrastructure for setting up a Kansas Comet server and communicating
with the browser. This makes it possible to interleave Haskell and Javascript
computations as needed.

[The Sunroof examples][SunroofExamples] are also a good starting point
to see how Sunroof can be used.
The examples provided here are collected in [this file][ExampleFile].

The next part of the tutorial will show how the clock example
works and it will show you how to create Javascript functions!

**RESOLVE TODOS!**

[SunroofBlogIntro]: http://www.ittc.ku.edu/csdlblog/?p=88 "Monad Reification in Haskell and the Sunroof Javascript compiler"
[BooleanPackage]: http://hackage.haskell.org/package/Boolean-0.1.2 "Boolean package on Hackage"
[SunroofSource]: https://github.com/ku-fpg/sunroof "Sunroof sources on GitHub"
[SunroofExamples]: https://github.com/ku-fpg/sunroof/tree/master/examples "Sunroof examples on GitHub"
[ExampleFile]: TODO/tutorial.hs "Introduction examples"
[SunroofReadme]: https://github.com/ku-fpg/sunroof/blob/master/README.md "Sunroof README on GitHub"