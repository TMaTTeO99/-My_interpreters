(*estendere il linguaggio funzionale per operare con stack contenente valori interi*)
(*attenzione a non esporre la rappresentazione*)

type exp = ... | Stack of Valist | Push of exp * exp | Pop of exp 
and Valist = | Empty | Val of exp * Valist;;

type evT = .... | ValStack of evT list;;

let rec eval (e : exp) (en : evT env) : evT = match e with
  .
  .
  .
  | Stack(l) -> let lV = evallist l en in ValStack(lv)
  | Push(s,v) -> let vs = eval s en in
                  (match vs with
                  | ValStack(ls) -> let vv = eval v en in
                                      if typecheck "int" vv then ValStack(vv::ls)
                                      else failwith("error");; 
                  | _ -> failwith("errore tipo"))
  | Pop(s) -> let vs = eval s en in
                (match vs with
                | ValStack(l) -> match l with
                                 | [] -> failwith("Empty")
                                 | h::t -> ValStack(t)
                | _ -> failwith("error"));;



let rec evallist (l : Valist) (en : evT env) : env = match l with
    | Empty -> []
    | h::t -> let v = eval h en in if (typecheck "int" (eval h en)) then v::(evallist t en)
                                      else failwith("error");; 


(*anche questo è corretto domani provo a farne qualcuno totalmente solo*)