(* Julien Cohen - Polytech Nantes *)

type hauteur = int

type valeur = Noire | Croche  | DCroche

type instru = Piano | Violon | GuitareSat | Basse


type  morceau = { 
  partition : (hauteur * valeur) list ; 
  tempo :  int ; 
  instrument : instru 
}
