SRCS=Board.hs Game.hs Pos.hs Problem.hs Source.hs Unit.hs

showboard: $(SRCS) ShowBoard.hs
	ghc -o showboard --make ShowBoard

main: $(SRCS) Main.hs
	ghc -o main --make Main
