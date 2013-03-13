 
# Part 2: Clock Example

The clock example demonstrates how Sunroof can produce self-contained 
Javascript that renders a clock using the HTML5 canvas element.

![The Clock Example](clock.png)

The Javascript API for HTML5 canvas element is already provided by 
Sunroof in the module `Language.Sunroof.JS.Canvas`. 
Lets look how we can render one line of the clock face using Sunroof:

    c # save
    -- Draw one of the indicator lines
    c # beginPath
    c # moveTo (0, -u * 1.0)
    ifB (n `mod` 5 ==* (0 :: JSNumber))
        (c # lineTo (0, -u * 0.8)) -- Minute line
        (c # lineTo (0, -u * 0.9)) -- Hour line
    ifB (n `mod` 15 ==* (0 :: JSNumber))
        (c # setLineWidth 8) -- Quarter line
        (c # setLineWidth 3) -- Non-Quarter line
    c # stroke
    c # closePath
    -- Draw of the hour numbers
    ifB (n `mod` 5 ==* (0 :: JSNumber))
        (do
          c # translate (-u * 0.75, 0)
          c # rotate (-2 * pi / 4)
          c # fillText (cast $ n `div` 5) (0, 0)
        ) (return ())
    c # restore

The monadic `do`-notation is used for sequencing Javascript statements
in a neat fashion.

The first few lines probably look familiar to people how have written 
Javascript before.

    c # save
    -- Draw one of the indicator lines
    c # beginPath
    c # moveTo (0, -u * 1.0)

Javascripts `.`-operator is used for function composition in Haskell.
We use the `#`-operator for that reason. Knowing this you can see 
that we are calling methods on the Javascript object `c` (Our canvas context).
The methods without parameters do not require empty paranthesis, as
a Hasekell programmer would expect. The tuple used in the call of `moveTo`
is only their to indicate that this parameter is a coordinate, not 
to single numbers. You can also see that Javascript numbers are neatly
embedded using the `Num`-class and can be used naturally.

The next few lines show a branch.

    ifB (n `mod` 5 ==* (0 :: JSNumber))
        (c # lineTo (0, -u * 0.8)) -- Minute line
        (c # lineTo (0, -u * 0.9)) -- Hour line

Haskells lack the possibilities to deep embed branches and
boolean expressions. For that reason we are using the 
[`Data.Boolean`][BooleanPackage] package. Instead of `if-then-else`
you are required to use `ifB` when writing Javascript. 
Operations that produce booleans
like `==`, `<` and `&&` are provided with an appended star.

The thrid branch also shows how nested blocks of code can be used.

    ifB (n `mod` 5 ==* (0 :: JSNumber))
        (do
          c # translate (-u * 0.75, 0)
          c # rotate (-2 * pi / 4)
          c # fillText (cast $ n `div` 5) (0, 0)
        ) (return ())

Note the cast operation in the fifth line. As Haskells type
system is more restrictive then the one used in Javascript, we sometimes
have to `cast` one Javascript value to another. This may seem more
complicated then writing the Javascript by hand, but when using 
the API correctly (by not working around it) compile time
errors can show mistakes in the Javascript code.

Getting back to the initial code block: How do we render the other 
59 lines of the clock face? We just frap this code into a function.
Of course, we do this at Javascript level.

    renderClockFaceLine <- function $ \(c, u, n) -> do
      ...

We have just created a Javascript function `renderClockFaceLine` with the 
three needed parameters. So lets render the complete clock face using
the `forEach`-method provided by arrays.

    c # save
    c # rotate (2 * pi / 4) -- 0 degrees is at the top
    -- Draw all hour lines.
    array [1..60::Int] # forEach $ \n -> do
      c # save
      c # rotate ((2 * pi / 60) * n)
      renderClockFaceLine `apply` (c, u, n)
      c # restore
    c # restore -- Undo all the rotation.

The `array` combinator converts the list into a Javascript array. The supplied 
function for the loop body takes the current element as a parameter.
In the loop body you can see how the `apply` combinator is used to 
apply a Javascript function to arguments. As the usefulness of partial 
application is questionable in the context of deep embedded Javascript, 
we only allow tuples as parameters to functions.

Using these techniques we can render the clock with about 50 
lines of Haskell.

    clockJS = do
      -- Renders a single line (with number) of the clock face.
      renderClockFaceLine <- function $ \(c, u, n) -> do
        c # save
        -- Draw one of the indicator lines
        c # beginPath
        c # moveTo (0, -u * 1.0)
        ifB (n `mod` 5 ==* (0 :: JSNumber))
            (c # lineTo (0, -u * 0.8)) -- Minute line
            (c # lineTo (0, -u * 0.9)) -- Hour line
        ifB (n `mod` 15 ==* (0 :: JSNumber))
            (c # setLineWidth 8) -- Quarter line
            (c # setLineWidth 3) -- Non-Quarter line
        c # stroke
        c # closePath
        -- Draw of the hour numbers
        ifB (n `mod` 5 ==* (0 :: JSNumber))
            (do
              c # translate (-u * 0.75, 0)
              c # rotate (-2 * pi / 4)
              c # fillText (cast $ n `div` 5) (0, 0)
            ) (return ())
        c # restore
      -- Renders a single clock pointer.
      renderClockPointer <- function $ \(c, u, angle, width, len) -> do
        c # save
        c # setLineCap "round"
        c # rotate angle
        c # setLineWidth width
        c # beginPath
        c # moveTo (0, u * 0.1)
        c # lineTo (0, -u * len)
        c # stroke
        c # closePath
        c # restore
      -- Renders the clocks pointers for hours, minutes and seconds.
      renderClockPointers <- function $ \(c, u) -> do
        (h, m, s) <- currentTime
        c # save
        c # setLineCap "round"
        -- Hour pointer
        apply renderClockPointer
          (c, u, (2 * pi / 12) * ((h `mod` 12) + (m `mod` 60) / 60), 15, 0.4)
        -- Minute pointer
        apply renderClockPointer
          ( c, u, (2 * pi / 60) * ((m `mod` 60) + (s `mod` 60) / 60), 10, 0.7)
        -- Second pointer
        c # setStrokeStyle "red"
        apply renderClockPointer
          ( c, u, (2 * pi / 60) * (s `mod` 60), 4, 0.9)
        -- Restore everything
        c # restore
      -- Renders the complete face of the clock, without pointers.
      renderClockFace <- function $ \(c, u) -> do
        c # save
        c # rotate (2 * pi / 4) -- 0 degrees is at the top
        -- Draw all hour lines.
        array [1..60::Int] # forEach $ \n -> do
          c # save
          c # rotate ((2 * pi / 60) * n)
          renderClockFaceLine `apply` (c, u, n)
          c # restore
        c # restore -- Undo all the rotation.
      -- Renders the complete clock.
      renderClock <- function $ \() -> do
        u <- clockUnit
        (w,h) <- canvasSize
        c <- context
        -- Basic setup
        c # save
        c # setFillStyle "black"
        c # setStrokeStyle "black"
        c # setLineCap "round"
        c # setTextAlign "center"
        c # setFont ((cast $ u * 0.1) <> "px serif")
        c # setTextBaseline "top"
        c # clearRect (0,0) (w,h)
        c # translate (w / 2, h / 2)
        -- Draw all hour lines.
        renderClockFace `apply` (c, u)
        -- Draw the clock pointers
        renderClockPointers `apply` (c, u)
        c # restore
        return ()
      renderClock `apply` ()
      window # setInterval renderClock 1000
      return ()

Using the `staticCompiler` we can compile 
the deep embedded Javascript into a string of actual Javascript.

    main = staticCompiler def "main" clockJS >>= writeFile "main.js"

The compiled string will contain a function `main` 
that executes the our Javascript.

You can see [the example in action here][ExampleRunning]. 
The [produced Javascript looks like this][ProducedJS]. The 
[Haskell sources can be found here][HaskellSource].

If you want to see the rest of Sunroof you can find it on [github][SunroofClockExample].


[SunroofBlogIntro]: http://www.ittc.ku.edu/csdlblog/?p=88 "Monad Reification in Haskell and the Sunroof Javascript compiler"
[KansasComet]: https://github.com/ku-fpg/kansas-comet "Kansas Comet repository"
[SunroofClockExample]: https://github.com/ku-fpg/sunroof "Sunroof clock example"
[BooleanPackage]: http://hackage.haskell.org/package/Boolean-0.1.2 "Boolean package on Hackage"
[ExampleRunning]: TODO "Sunroof clock example"
[ProducedJS]: TODO "Sunroof clock example"
[HaskellSource]: TODO "Sunroof clock example"


