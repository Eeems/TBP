all: bin/TBP.8xp

bin/TBP.8xp:
	spasm -I src/ "src/The Blue Platform.asm" bin/TBP.8xp
