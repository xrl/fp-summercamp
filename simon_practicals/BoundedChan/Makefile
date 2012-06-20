all: BoundedChanSeq TChan

TChan: TChan.hs
	ghc TChan.hs -o TChan

BoundedChanSeq: BoundedChanSeq.hs
	ghc BoundedChanSeq.hs -o BoundedChanSeq

clean:
	rm -f *.hi *.o BoundedChan BoundedChanSeq