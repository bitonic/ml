
SIGNATURES = utils.sig stream.sig parscomb.sig parser.sig typecheck.sig
STRUCTURES = $(patsubst %.sig, %.sml, $(SIGNATURES))

i:
	sml utils.sig stream.sig parscomb.sig parser.sig utils.sml stringstream.sml parscomb.sml parser.sml typecheck.sig typecheck.sml

