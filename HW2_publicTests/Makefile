# When compiling, ensure HW1.lhs is in the same directory as this file!

.PHONY: all zip clean

all:
	ghc -o HW2_publicTests.test -main-is HW2_publicTests -i ./tic-tac-toe/classic/src/TicTacToe.hs -i -i ./HW2.lhs -i ./tic-tac-toe/classic/src/Types.hs -i ./tic-tac-toe/classic/src/Checks.hs -i ./tic-tac-toe/classic/src/Misc.hs -i ./tic-tac-toe/classic/src/Player/Human.hs -i ./tic-tac-toe/classic/src/Player/Computer.hs -i ./tic-tac-toe/classic/src/Player/MinMax.lhs -i ./tic-tac-toe/classic/src/Player/BestNext.hs ./HW2_publicTests.lhs   


zip: 
	zip -FS HW2_publicTests.zip Makefile HW2_publicTests.lhs run_tests.sh README

clean:
	rm -f *.o *.hi *~ *.test *.zip
