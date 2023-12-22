(*
  Una funzione a dominio finito è definita solo per un numero finito di argomenti
  Estendere il linguaggio funzionale in modo da poter eseguire funzioni di questo tipo
*)

type exp = .... | Dfun of (*parametro f*)ide * (*corpo*)exp * exp list;;(*lista di espressioni che dovranno 
                                                                          essere valutate per ottenere i valore
                                                                          del dominio della funzione*) 

(*ora bisogna estendere gli evT*)

type evT = ...... |DVfun of ide * exp * evT list * evT env;;


rec eval (e : exp) (env : evT env) : evT = match e with
  .
  .
  .
  .
  | Dfun(i,e,l) -> let vl = evalList l env in DVfun(i,e,vl,env)
  | Apply(e,arg) -> let ve = eval e env in 
                      (match ve with
                      | DVfun(i,body,lsV,envf) -> let vpa = eval arg env in
                                                    if check vpa lsV then eval body (bind envf i vpa) 
                                                    else Unbound
                      | _ -> failwith("ERROR") );;


let rec evalList l env = match l with
  | [] -> []
  | h::t -> let vh = eval h env in vh::evalList t env;;

let rec check x lst = match lst with
| [] -> false
| h::t -> if evtEQ x h then true else check x t;;

evtEQ (x : evT) (y : evT) = (*opportuna operazione di uguaglianza in base al tipo evT*) 

(*esercizio giusto *) 