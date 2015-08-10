SRCS=Board.hs Debug.hs Game.hs Pos.hs Problem.hs Source.hs Unit.hs

play_icfp2015: $(SRCS) Main.hs
	ghc -O3 -o play_icfp2015 --make Main

main: $(SRCS) Main.hs
	ghc -O3 -o main --make Main

showboard: $(SRCS) ShowBoard.hs
	ghc -o showboard --make ShowBoard

score: $(SRCS) Score.hs
	ghc -rtsopts -o score --make Score

decode: $(SRCS) Decode.hs
	ghc -o decode --make Decode
