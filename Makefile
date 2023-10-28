maelstrom = maelstrom/maelstrom

compile:
	cabal build

echo: compile
	$(maelstrom) test -w echo --bin $$(cabal list-bin echo) --node-count 1 --time-limit 10

web:
	$(maelstrom) serve