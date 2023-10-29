maelstrom = maelstrom/maelstrom

compile:
	cabal build

echo: compile
	$(maelstrom) test -w echo --bin $$(cabal list-bin echo) --node-count 1 --time-limit 10

unique-ids: compile
	$(maelstrom) test -w unique-ids --bin $$(cabal list-bin unique-ids) --time-limit 30 --rate 1000 --node-count 3 --availability total --nemesis partition

broadcast-single: compile
	$(maelstrom) test -w broadcast --bin $$(cabal list-bin broadcast) --node-count 1 --time-limit 20 --rate 10

broadcast-multi: compile
	$(maelstrom) test -w broadcast --bin $$(cabal list-bin broadcast) --node-count 5 --time-limit 20 --rate 10

broadcast-fault-tolerant: compile
	$(maelstrom) test -w broadcast --bin $$(cabal list-bin broadcast) --node-count 5 --time-limit 20 --rate 10 --nemesis partition

web:
	$(maelstrom) serve