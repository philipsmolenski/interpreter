# Makefile generated by BNFC.

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : interpreter Big/TestBig

# Rules for building the parser.

Big/ErrM.hs Big/LexBig.x Big/PrintBig.hs Big/ParBig.y Big/TestBig.hs : Big.cf
	bnfc --haskell -p Big Big.cf

%.hs : %.y
	happy --ghc --coerce --array --info $<

%.hs : %.x
	alex --ghc $<

Big/TestBig : Big/TestBig.hs Big/ErrM.hs Big/LexBig.hs Big/ParBig.hs Big/PrintBig.hs
	ghc --make $< -o $@

interpreter : Main.hs Big/TestBig.hs Big/ErrM.hs Big/LexBig.hs Big/ParBig.hs Big/PrintBig.hs Interpreter.hs EvalExpr.hs DataStructures.hs TypeCheck.hs
	ghc --make $< -o interpreter

# Rules for cleaning generated files.

clean :
	-rm -f Big/*.hi Big/*.o Big/*.log Big/*.aux Big/*.dvi *.o *.hi interpreter
	-rm -rf Big/

distclean : clean
	-rm -f Big/AbsBig.hs Big/AbsBig.hs.bak Big/ComposOp.hs Big/ComposOp.hs.bak Big/DocBig.txt Big/DocBig.txt.bak Big/ErrM.hs Big/ErrM.hs.bak Big/LayoutBig.hs Big/LayoutBig.hs.bak Big/LexBig.x Big/LexBig.x.bak Big/ParBig.y Big/ParBig.y.bak Big/PrintBig.hs Big/PrintBig.hs.bak Big/SharedString.hs Big/SharedString.hs.bak Big/SkelBig.hs Big/SkelBig.hs.bak Big/TestBig.hs Big/TestBig.hs.bak Big/XMLBig.hs Big/XMLBig.hs.bak Big/ASTBig.agda Big/ASTBig.agda.bak Big/ParserBig.agda Big/ParserBig.agda.bak Big/IOLib.agda Big/IOLib.agda.bak Big/Main.agda Big/Main.agda.bak Big/Big.dtd Big/Big.dtd.bak Big/TestBig Big/LexBig.hs Big/ParBig.hs Big/ParBig.info Big/ParDataBig.hs Makefile
	-rmdir -p Big/

# EOF