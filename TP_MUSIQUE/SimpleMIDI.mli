(* Julien Cohen - Polytech Nantes *)
open Morceau

(** Produit un fichier Midi pour le morceau donné avec le nom donné. *)
val write_morceau : morceau -> string -> unit
