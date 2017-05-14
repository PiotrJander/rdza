all:
	happy -gca ParRdza.y
	alex -g LexRdza.x
	ghc --make TestRdza.hs -o interpreter

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocRdza.* LexRdza.* ParRdza.* LayoutRdza.* SkelRdza.* PrintRdza.* TestRdza.* AbsRdza.* TestRdza ErrM.* SharedString.* ComposOp.* Rdza.dtd XMLRdza.* Makefile*
	

