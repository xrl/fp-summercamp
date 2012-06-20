all: NumberGame BoundedChan

NumberGame : NumberGame.hs Async.hs
	ghc NumberGame.hs

BoundedChan : BoundedChan.hs 
	ghc BoundedChan.hs


clean:
	rm NumberGame BoundedChan *.hi *.o
