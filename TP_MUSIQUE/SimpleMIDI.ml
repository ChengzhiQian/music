(* Julien Cohen - Polytech Nantes *)

open List
open Morceau

type piste =
    {
      midi_partition : (hauteur * int) list ; 
      midi_tempo :  int ; 
      midi_instrument : int 
    }
      


let nombre_de_tics_par_noire = 4 ;;

let int_of_instrument i = match i with (* table GM1 *)
    Piano -> 1
  | Violon -> 42
  | GuitareSat -> 30
  | Basse -> 38


let piste_of_morceau m = 

  let dur_doublecroche  = nombre_de_tics_par_noire / 4
  and dur_croche =  nombre_de_tics_par_noire / 2
  and dur_noire =  nombre_de_tics_par_noire
  in let duree n = match n with 
      Noire -> dur_noire
    | Croche -> dur_croche
    | DCroche -> dur_doublecroche 
     in let evenements = map (fun (a,b)->(a, duree b)) m.partition
	in {midi_partition = evenements;
	    midi_instrument = int_of_instrument m.instrument;
	    midi_tempo = m.tempo}


let volume_moyen = 100 ;; (* Volume entre 0 et 127 *)

let rec convert_events l = match l with
    [] -> []
  | (hauteur,duree) :: reste -> 
    (0,1, MIDI.MIDI.NoteON(hauteur,volume_moyen)) :: 
      (duree, 1, MIDI.MIDI.NoteOFF(hauteur,volume_moyen)) :: convert_events reste ;;


let tempo_to_microseconds_par_noire tempo = 
  (* les changements de tempo dans un fichier MIDI s'expriment en micro-secondes par noire. *)
  let microsecondesparminutes = 1000000 * 60
  in  microsecondesparminutes / tempo
      ;;




let write_piste {midi_partition = ev; midi_instrument = son; midi_tempo = tempo } fic =
  let change_tempo_ev = ((0,0,MIDI.MIDI.Tempo (tempo_to_microseconds_par_noire tempo ) ))
  and change_instr_ev = (0,1,MIDI.MIDI.ProgramChange son ) in 
  let evlist2 = change_tempo_ev :: change_instr_ev :: convert_events ev in
  let mid = (nombre_de_tics_par_noire, [evlist2]) in
    MIDI.MIDI.write mid fic ;;


let write_morceau m = write_piste (piste_of_morceau m)
