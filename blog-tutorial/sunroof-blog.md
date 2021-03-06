 
# Sunroof: Clockwork Progress

In this article, we are going to generate a JavaScript application. Last year, we [wrote a blog post][BlogSunroofIntro] about using monad reification to implement a JavaScript compiler. The compiler, called Sunroof, is now in state that we can make the [first public release][HackageSunroofCompiler]. By way of a non-trivial example, this blog entry illustrates how to construct an analog clock as a self-contained JavaScript application that renders the clock using the HTML5 canvas element.

![The Clock Example](clock-small.png)

The JavaScript API for HTML5 canvas element is already provided by Sunroof in the module `Language.Sunroof.JS.Canvas`. Lets look how we can render one line of the clock face using Sunroof:

    c # save
    -- Draw one of the indicator lines
    c # beginPath
    c # moveTo (0, -u * 1.0)
    ifB (n `mod` 5 ==* 0)
        (c # lineTo (0, -u * 0.8)) -- Minute line
        (c # lineTo (0, -u * 0.9)) -- Hour line
    ifB (n `mod` 15 ==* 0)
        (c # setLineWidth 8) -- Quarter line
        (c # setLineWidth 3) -- Non-Quarter line
    c # stroke
    c # closePath
    -- Draw of the hour numbers
    ifB (n `mod` 5 ==* 0)
        (do
          c # translate (-u * 0.75, 0)
          c # rotate (-2 * pi / 4)
          c # fillText (cast $ n `div` 5) (0, 0)
        ) (return ())
    c # restore

The monadic `do`-notation is used for sequencing JavaScript statements in a neat fashion.

The first few lines probably look familiar to people who have written JavaScript before.

    c # save
    -- Draw one of the indicator lines
    c # beginPath
    c # moveTo (0, -u * 1.0)

The `#`-operator is used instead of the `.`-operator in JavaScript. `u` represents the radius of the clock. Knowing this you can see that we are calling methods on the JavaScript object `c` (Our canvas context). The methods without parameters do not require empty parenthesis, as a Haskell programmer would expect. The tuple used in the call of `moveTo` is only there to indicate that this parameter is a coordinate, not two single numbers. You can also see that JavaScript numbers are neatly embedded using the `Num`-class and can be used naturally.

The next few lines show a branch.

    ifB (n `mod` 5 ==* 0)
        (c # lineTo (0, -u * 0.8)) -- Minute line
        (c # lineTo (0, -u * 0.9)) -- Hour line

Haskell lacks the possibilities to deep embed branches and boolean expressions. For that reason we use the [`Data.Boolean`][HackageBoolean] package. Instead of `if-then-else` you are required to use `ifB` when writing JavaScript.

    ifB (n `mod` 5 ==* 0)
        (do
          c # translate (-u * 0.75, 0)
          c # rotate (-2 * pi / 4)
          c # fillText (cast $ n `div` 5) (0, 0)
        ) (return ())

Note the `cast` operation in line five. As Haskell's type system is more restrictive then the one used in JavaScript, we sometimes have to `cast` one value to another. This may seem more complicated then writing JavaScript by hand, but when using the API correctly (by not working around it) compile time errors can show mistakes in the code early.

Getting back to the initial code block: How do we render the other 59 lines of the clock face? We just wrap this code into a function. Of course, we do this at JavaScript level.

    renderClockFaceLine <- function $ \(c :: JSCanvas, u :: JSNumber, n :: JSNumber) -> do
      ...

We have just created the JavaScript function `renderClockFaceLine` with three parameters. So lets render the complete clock face using the `forEach`-method provided by arrays.

    c # save
    c # rotate (2 * pi / 4) -- 0 degrees is at the top
    -- Draw all hour lines.
    lines <- array [1..60::Int]
    lines # forEach $ \n -> do
      c # save
      c # rotate ((2 * pi / 60) * n)
      renderClockFaceLine $$ (c, u, n)
      c # restore
    c # restore -- Undo all the rotation.

The `array` combinator converts the list into a JavaScript array. The supplied function for the loop body takes the current element as a parameter. In the loop body you can see how the `$$`-operator is used just as the `$`-operator in Haskell to apply a JavaScript function to arguments. As the usefulness of partial application is questionable in the context of deep embedded JavaScript, we only allow uncurried functions.

Using these techniques we can render the clock with about 90 lines of Haskell.

    clockJS :: JS A (JSFunction () ())
    clockJS = function $ \() -> do
      -- Renders a single line (with number) of the clock face.
      renderClockFaceLine <- function $ \(c :: JSCanvas, u :: JSNumber, n :: JSNumber) -> do
        c # save
        -- Draw one of the indicator lines
        c # beginPath
        c # moveTo (0, -u * 1.0)
        ifB (n `mod` 5 ==* 0)
            (c # lineTo (0, -u * 0.8)) -- Minute line
            (c # lineTo (0, -u * 0.9)) -- Hour line
        ifB (n `mod` 15 ==* 0)
            (c # setLineWidth 8) -- Quarter line
            (c # setLineWidth 3) -- Non-Quarter line
        c # stroke
        c # closePath
        -- Draw of the hour numbers
        ifB (n `mod` 5 ==* 0)
            (do
              c # translate (-u * 0.75, 0)
              c # rotate (-2 * pi / 4)
              c # fillText (cast $ n `div` 5) (0, 0)
            ) (return ())
        c # restore
      -- Renders a single clock pointer.
      renderClockPointer <- function $ \(c :: JSCanvas, u :: JSNumber, angle :: JSNumber, width :: JSNumber, len :: JSNumber) -> do
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
      renderClockPointers <- function $ \(c :: JSCanvas, u :: JSNumber) -> do
        (h, m, s) <- currentTime
        c # save
        c # setLineCap "round"
        -- Hour pointer
        renderClockPointer $$
          (c, u, (2 * pi / 12) * ((h `mod` 12) + (m `mod` 60) / 60), 15, 0.4)
        -- Minute pointer
        renderClockPointer $$
          ( c, u, (2 * pi / 60) * ((m `mod` 60) + (s `mod` 60) / 60), 10, 0.7)
        -- Second pointer
        c # setStrokeStyle "red"
        renderClockPointer $$ ( c, u, (2 * pi / 60) * (s `mod` 60), 4, 0.9)
        -- Restore everything
        c # restore
      -- Renders the complete face of the clock, without pointers.
      renderClockFace <- function $ \(c :: JSCanvas, u :: JSNumber) -> do
        c # save
        c # rotate (2 * pi / 4) -- 0 degrees is at the top
        -- Draw all hour lines.
        lines <- array [1..60::Int]
        lines # forEach $ \n -> do
          c # save
          c # rotate ((2 * pi / 60) * n)
          renderClockFaceLine $$ (c, u, n)
          c # restore
        c # restore -- Undo all the rotation.
      -- Renders the complete clock.
      renderClock <- continuation $ \() -> do
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
        renderClockFace $$ (c, u)
        -- Draw the clock pointers
        renderClockPointers $$ (c, u)
        c # restore
        return ()
      window # setInterval (goto renderClock) 1000
      -- and draw one now, rather than wait till later
      goto renderClock ()

      return ()

Using the `sunroofCompileJSA` function we can compile the deep embedded JavaScript into a string of actual JavaScript.

    sunroofCompileJSA def "main" clockJS >>= writeFile "main.js"

The compiled string will contain a function `main` that executes our JavaScript. This is then called in the HTML file to execute.

There are a few small utilities used in the code. The current time is perceived by `currentTime` which uses the JavaScript date API provided by the module `Language.Sunroof.JS.Date`.

    currentTime :: JS A (JSNumber, JSNumber, JSNumber)
    currentTime = do
      date <- newDate ()
      h <- date # getHours
      m <- date # getMinutes
      s <- date # getSeconds
      return (h, m, s)

Note that this will literally copy the JavaScript produced by `currentTime` to where it is used, because it is not abstracted to a function in JavaScript. Every time you write Sunroof code that is not wrapped in a function, the Haskell binding will work like a macro.

The other helpers are just shortcuts to get certain values:

    canvas :: JS A JSObject
    canvas = document # getElementById "canvas"

    context :: JS A JSCanvas
    context = canvas >>= getContext "2d"

    clockUnit :: JS A JSNumber
    clockUnit = do
      (w, h) <- canvasSize
      return $ (maxB w h) / 2

    canvasSize :: JS A (JSNumber, JSNumber)
    canvasSize = do
      c <- jQuery "#canvas"
      w <- c # invoke "innerWidth" ()
      h <- c # invoke "innerHeight" ()
      return (w, h)

You can see [the clock in action here][ExampleRunning].

As you can see Sunroof mirrors JavaScript closely, and allows access to the capabilities a browser provides. But is this Haskell for Haskell's sake? We do not think so:

 * Sunroof is a deeply embedded DSL, so it is easy to write functions that generate custom code.
 * Sunroof provides some level of type safely on top of JavaScript, including typed arrays, finite maps, functions and continuations.
 * Sunroof also offers an abstraction over the JavaScript threading model, by providing two types of threads, atomic and (cooperatively) blocking. On top of this, Sunroof provides some Haskell concurrency patterns
   like `MVar` or `Chan` (`JSMVar` and `JSChan`).
 * Furthermore, the [`sunroof-server` package][HackageSunroofServer] offers a ready to use web-server to deploy generated JavaScript on the fly. It enables you to interleave Haskell and JavaScript computations as needed, through synchronous or asynchronous remote procedure calls.

[A number of examples][GitHubSunroofWikiExamples] and [a tutorial][GitHubSunroofWikiTutorial] is provided on [GitHub][GitHubSunroofExamples]. Their [Haskell sources can be found on github][GitHubSunroofClock], they are part of the [`sunroof-examples` package][HackageSunroofExamples].

[BlogSunroofIntro]: http://www.ittc.ku.edu/csdlblog/?p=88 "Monad Reification in Haskell and the Sunroof JavaScript compiler"

[ExampleRunning]: http://www.ittc.ku.edu/csdl/fpg/software/sunroof/clock-example/clock.html "The clock example"

[HackageBoolean]: http://hackage.haskell.org/package/Boolean-0.1.2 "Hackage: Boolean"
[HackageSunroofCompiler]: http://hackage.haskell.org/package/sunroof-compiler "Hackage: sunroof-compiler"
[HackageSunroofServer]: http://hackage.haskell.org/package/sunroof-server "Hackage: sunroof-server"
[HackageSunroofExamples]: http://hackage.haskell.org/package/sunroof-examples "Hackage: sunroof-examples"

[GitHubKansasComet]: https://github.com/ku-fpg/kansas-comet "kansas-comet repository"
[GitHubSunroofCompiler]: http://hackage.haskell.org/package/sunroof-compiler "sunroof-compiler repository"
[GitHubSunroofServer]: http://hackage.haskell.org/package/sunroof-server "sunroof-server repository"
[GitHubSunroofExamples]: http://hackage.haskell.org/package/sunroof-examples "sunroof-examples repository"

[GitHubSunroofClock]: https://github.com/ku-fpg/sunroof-examples/blob/master/examples/clock/Main.hs "Clock examples Haskell sources"
[GitHubSunroofWikiExamples]: https://github.com/ku-fpg/sunroof-compiler/wiki/Examples "Example descriptions"
[GitHubSunroofWikiTutorial]: https://github.com/ku-fpg/sunroof-compiler/wiki/Tutorial "Sunroof Tutorial"











