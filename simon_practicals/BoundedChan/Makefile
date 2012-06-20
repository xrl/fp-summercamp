all: BoundedChanSeq BoundedChan

BoundedChan: BoundedChan.hs
	ghc BoundedChan.hs -o BoundedChan

BoundedChanSeq: BoundedChanSeq.hs
	ghc BoundedChanSeq.hs -o BoundedChanSeq

clean:
	rm -f *.hi *.o BoundedChan BoundedChanSeq