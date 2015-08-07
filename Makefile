showboard: Board.hs ShowBoard.hs
	ghc -o showboard --make ShowBoard

main: Board.hs Main.hs
	ghc -o main --make Main
