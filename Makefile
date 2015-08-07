SRCS=Board.hs Pos.hs Problem.hs Unit.hs

showboard: $(SRCS) ShowBoard.hs
	ghc -o showboard --make ShowBoard

main: $(SRCS) Main.hs
	ghc -o main --make Main
