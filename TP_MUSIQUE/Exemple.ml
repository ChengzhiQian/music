(* Fichier à compléter. *)

(* Fichier à compléter. *)

open Morceau
open SimpleMIDI


let monmorceaubase = {
  partition = [(70,Noire);(70,Noire);(80,Noire);(120,Noire)] ;
  tempo = 2000 ;
  instrument = Piano
}

let montest = {
  partition = [(70,Noire);(70,Noire);(80,DCroche);(90,Noire);(70,DCroche);(70,Noire);(80,Croche);(90,Noire);(70,Noire);(70,DCroche);(80,Noire);(90,Noire);(70,Noire);(70,Noire);(80,Noire);(90,Noire)];
  tempo = 80;
  instrument = Basse
}


let accord_maj hauteur = [hauteur; hauteur+4; hauteur+7];;

type theme = {
  intro : hauteur list;
  couplet : hauteur list;
  refrain : hauteur list;
  solo : hauteur list;
  outro : hauteur list;
}

let monsupertheme = {
  intro = accord_maj 50;
  couplet = accord_maj 60;
  refrain = accord_maj 70;
  solo = accord_maj 80;
  outro = accord_maj 90;
}

let decompose list = List.mapi (fun pos x -> if pos = 0 then (x, Noire) else (x, Croche)) list

let chansonette theme = 
  let _intro = decompose theme.intro in
  let _couplet = decompose theme.couplet in
  let _refrain = decompose theme.refrain in
  let _solo = decompose theme.solo in
  let _outro = decompose theme.outro in
  let _partition = _intro @ _couplet @ _refrain @ _solo @ _outro in
  {
  partition = _partition;
  tempo = 80 ;
  instrument = Piano
}

let dedouble hauteurs = List.fold_left (fun a x -> a@[x-24; x-12]) [] hauteurs

let decompose_disco list = List.map (fun x -> (x, Croche)) (dedouble list)

let disco theme = 
  let _partition = decompose_disco theme.intro @
  decompose_disco theme.couplet @
  decompose_disco theme.refrain @
  decompose_disco theme.solo @
  decompose_disco theme.couplet @
  decompose_disco theme.refrain @
  decompose_disco theme.outro in
  {
  partition = _partition;
  tempo = 90 ;
  instrument = Basse
}

let transpose dt morceau = {
  partition = List.map (fun (x, y) -> (x+dt, y)) morceau.partition;
  tempo = morceau.tempo;
  instrument = morceau.instrument;
} 

let octave morceau = transpose 12 morceau

let rec rev_aux l acc = 
  match l with
  | [] -> acc
  | e::r -> (rev_aux r (e::acc));;

let rev liste = rev_aux liste [];;

let envers morceau = {
  partition = rev morceau.partition;
  tempo = morceau.tempo;
  instrument = morceau.instrument;
}

let _ = write_morceau (envers (disco monsupertheme)) "test.mid";;
