## Infrastructure

As client server infrastructure we utilize [kansas-comet][KansasComet].
It sets up a Javascript push mechanism that sends
events from the browser to the server and allows us to
push arbitrary Javascript code fragments to the broswer for execution.

Sunroof provides the `sync` and `async` functions for this purpose.

    async :: Document -> JS () -> IO ()
    sync :: (Sunroof a) => Document -> JS a -> IO (Maybe a)

**TODO: Finalize the type of sync.**

`async` compiles the given Javascript and sends it of to the browser
for execution. `sync` does the same but it waits for the result
of the sent Javascript code and returns that result to work with. 
