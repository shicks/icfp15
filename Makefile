SRCS=Board.hs Debug.hs Game.hs Pos.hs Problem.hs Source.hs Unit.hs

main: $(SRCS) Main.hs
	ghc -o main --make Main

showboard: $(SRCS) ShowBoard.hs
	ghc -o showboard --make ShowBoard

score: $(SRCS) Score.hs
	ghc -o score --make Score
