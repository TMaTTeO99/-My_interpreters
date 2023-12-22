(*
  Esercitazione 27/11/20   

  Progettare e realizzare un interprete per un linguaggio didattico:
    1) Funzionale
    2) Con funzioni dichiarate globalmente
    3) Le funzioni non possono essere dichiarate localmente in altre funzioni
    4) Un programma in questo linguaggio ha la forma: P = [fun_dcl list] main_exp
*)

(*
  Definisco albero di sintassi astratta   
*)
type ide = string;;

type exp =
  | Cint of int
  | Add of exp * exp
  | Sub of exp * exp
  | Mul of exp * exp
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
  | Eq of exp * exp
  | Ifthenelse of exp * exp * exp
  | Den of ide
  | Let of ide * exp * exp
  | Apply of ide * exp;;

(*definisco il tipo delle funzioni in modo da non poter essere definite all interno del cotrutto let*)
type def = Fun of ide * ide * exp;; 

(*definisco il tipo del programma*)
type prog = Prog of (def list) * exp

exception Emptyenv;;

(*defeinisco l'ambiente come una funzione*)
type 'v env = ide -> 'v;;
let bind env i v = function x -> if x = i then v else env x;;
let env0 = function x -> raise Emptyenv;;

(*
  Non è richiesto di fare il controllo dei tipi   
*)

(*usero due ambienti uno per la valutazione delle espressioni uno per la valutazione delle funzioni definite globali*)

let rec eval e enve envf = match e with
  | Cint(n) -> n
  | Den(n) -> enve n 
  | Add(e1,e2) -> (eval e1 enve envf) + (eval e2 enve envf)
  | Sub(e1,e2) -> (eval e1 enve envf) - (eval e2 enve envf)
  | Mul(e1,e2) -> (eval e1 enve envf) * (eval e2 enve envf)
  | And(e1,e2) -> if ((eval e1 enve envf) = 1) then (eval e2 enve envf) else 0 
  | Or(e1,e2) -> if ((eval e1 enve envf) = 1) then 1 else if ((eval e2 enve envf) = 1) then 1 else 0
  | Not(e1) -> if ((eval e1 enve envf) = 1) then 0 else 1
  | Eq(e1,e2) -> if ((eval e1 enve envf) = (eval e2 enve envf)) then 1 else 0
  | Ifthenelse(cond,e1,e2) -> if ((eval cond enve envf) = 1) then (eval e1 enve envf) else (eval e2 enve envf) 
  | Let(x,e1,e2) -> eval e2 (bind enve x (eval e1 enve envf )) envf
  | Apply(nm,e1) -> match (envf nm) with
                      | (pf,body) -> let ve1 = eval e1 enve envf in
                                      let nenv = bind enve pf ve1 in
                                        eval body nenv envf;;

let rec insenvf lst =  match lst with
  | [] -> env0
  | Fun(nm,pf,body)::t -> bind (insenvf t) nm (pf,body);; 

let evalprog p = match p with
  | Prog(de,e) -> let envf = insenvf de in eval e env0 envf;;

  let p1 = Prog([ ], Add(Cint 4, Cint 5));;
  let p2 = Prog([Fun("succ", "x", Add(Den "x", Cint 1))], Add(Apply("succ", Cint 4), Cint 5));;
  let p3 = Prog([Fun("tria", "x", Ifthenelse(Eq(Den "x", Cint 0), Cint 5, Add(Den "x", Apply("tria", Sub(Den "x", Cint 1)))))],Apply("tria", Cint 4));;
  
  (*
    funzionaaa ma è come quello del prof nell altri file fun_global ho fatto la mia soluzione che è migliore perche 
    fa anche il controllo dei tipi dinamicamente
  *)

(*
  Ora faccio a penna il secondo esercizioooooo
  
*)
