
SIGNATURES = utils.sig stream.sig parscomb.sig parser.sig typecheck.sig
STRUCTURES = $(patsubst %.sig, %.sml, $(SIGNATURES))

i:
	sml $(SIGNATURES) $(STRUCTURES)

