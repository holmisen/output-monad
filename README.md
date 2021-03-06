The `Output` monad is an output subset of the `IO` monad.

# Motivation

`IO` functions may perform both input and output. Output causes side
effects while input causes non determinism. Given

	foo :: S -> IO T

we can only know that `foo` takes an `S`, returns a `T` and
possibly has side effects. We cannot know if it is non
deterministic.

But given

	bar :: S -> Output T

we know that while `bar` may have side effects, it is
deterministic (in the absence of exceptions).

# What about the `Writer` monad

The `Writer` monad can be used to collect output from a pure function
to be output to file(s) etc. But then the whole computation must be
completed before any data is output. In a long running computation it
may be desirable to interleave output with processing.

# When is this useful

Whenever you want deterministic computations with side effects. It
is ofcourse possible to make specialised monads for those, but that
may be overkill (just like it may be overkill to make you own
specialised IO monad with a restricted subset of IO functions).

Logging is a special case provided for by dedicated logging
frameworks (such as
[hslogger](http://hackage.haskell.org/package/hslogger). If you only need
logging you should probably use one of those instead.
