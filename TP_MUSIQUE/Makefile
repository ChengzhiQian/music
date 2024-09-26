all: a.out

MIDI.cmo MIDI.cmi: MIDI.ml
	ocamlc -w -26 -c $<

SimpleMIDI.cmi: SimpleMIDI.mli MIDI.cmi Morceau.cmi
	ocamlc -c $<

SimpleMIDI.cmo:  SimpleMIDI.ml MIDI.cmi SimpleMIDI.cmi Morceau.cmi
	ocamlc -c $<

Morceau.cmo Morceau.cmi: Morceau.ml
	ocamlc -c $<

Exemple.cmo: Exemple.ml MIDI.cmi SimpleMIDI.cmi
	ocamlc -i $<
	ocamlc -c $<

ExNoMidi.cmo: ExNoMidi.ml Morceau.cmo
	ocamlc -c $<

a.nomidi.out: Morceau.cmo ExNoMidi.cmo 
	ocamlc -o $@ $<

a.out: MIDI.cmo Morceau.cmo SimpleMIDI.cmo Exemple.cmo 
	ocamlc $^



%.wav: %.mid
	timidity -Ow $<


clean:
	rm -f *.cmo *.cmi

.SUFFIXES:
