maelstrom = maelstrom/maelstrom

compile:
	cabal build

echo: compile
	$(maelstrom) test -w echo --bin $$(cabal list-bin echo) --node-count 1 --time-limit 10

unique-ids: compile
	$(maelstrom) test -w unique-ids --bin $$(cabal list-bin unique-ids) --time-limit 30 --rate 1000 --node-count 3 --availability total --nemesis partition

web:
	$(maelstrom) serve