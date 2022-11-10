(*definisco il tipo ide per gli identificatori*)
type ide = string;;
(*
  Definisco l ambiente non piu come una funzione ma come una lista di coppie (stringa ,valore) 
  (Si puo definire come una funzione)   
  L ambiente è un tipo parametrico rispetto al tipo degli elemnti che ha
*)
type 'v env = (string * 'v) list;;

(*definisco l'albero di sintassi astratta*)
type exp = 
  | Cint of int
  | Ctrue
  | Cfalse
  | Den of ide
  | Eq of exp * exp
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
  | Subb of exp * exp
  | Add of exp * exp
  | Mull of exp * exp
  | Is_zero of exp
  | Ifthenelse of exp * exp * exp
  | Let of ide * exp * exp
  | Fun of ide * exp
  | Applyy of exp * exp
  | LetRec of ide * ide * exp * exp;;

(*definisco i tipi di dato esprimibili (dati primitivi)*)


type evT = 
| Int of int
| Bool of bool
| Unbound
| Closure of ide * exp * evT env
| RecClousure of ide * ide * exp * evT env;;


(*definisco le operazioni di lookup e bind*)
let rec loockup env x = 
  match env with
  | [] -> Unbound
  | (ide,v)::t -> if x = ide then v else loockup t x;;

(*definisco l operazione di binding*)
let bind env i v = (i,v)::env;;

(*definisco ambiente vuoto*)
let emptyenv = [("", Unbound)];;


(*ora definisco il typechecker per fare il controllo dei tipi dinamicamente*)

let typecheck t des = match t with
  | "int" -> (match des with
              | Int(n) -> true
              | _ -> false)
  | "bool" -> (match des with
              | Bool(b) -> true
              | _ -> false)
  | _ -> failwith("Type not found");;


(*ora allora posso definire alcune funzioni primitive*)
let add x y = match (typecheck "int" x, typecheck "int" y, x, y) with
  | (true, true, Int(a), Int(b)) -> Int(a+b)
  | (_,_,_,_) -> failwith("Run Time Err");;

let sub x y = match (typecheck "int" x, typecheck "int" y, x, y) with
  | (true, true, Int(a), Int(b)) -> Int(a-b)
  | (_,_,_,_) -> failwith("Run Time Err");;

let mul x y = match (typecheck "int" x, typecheck "int" y, x, y) with
| (true, true, Int(a), Int(b)) -> Int(a*b)
| (_,_,_,_) -> failwith("Run Time Err");;

let anD x y = match (typecheck "bool" x, typecheck "bool" y, x, y) with 
| (true, true, Bool(a), Bool(b)) -> Bool(a && b)
| (_,_,_,_) -> failwith("Run Time Err");;

let oR x y = match (typecheck "bool" x, typecheck "bool" y, x, y) with
| (true, true, Bool(a), Bool(b)) -> Bool(a || b)
| (_,_,_,_) -> failwith("Run Time Err");;

let noT x = match (typecheck "bool" x, x) with
| (true, Bool(a)) -> Bool(not a)
| (_,_) -> failwith("Run Time Err");;

let is_zero x = match (typecheck "int" x, x) with
| (true, Int(a)) -> Bool(a=0)
| (_,_) -> failwith("Run Time Err");;

let eq_int x y = match (typecheck "int" x, typecheck "int" y, x, y) with
| (true,true,Int(a),Int(b)) -> Bool(a=b)
| (_,_,_,_) -> failwith("ERROR TYPE");;


(*ciclo dell' interprete*)

let rec eval exp env = match exp with
  | Cint(n) -> Int(n)
  | Cfalse -> Bool(false)
  | Ctrue -> Bool(true)
  | Den(x) -> loockup env x
  | And(e1,e2) -> anD (eval e1 env) (eval e2 env)
  | Or(e1,e2) -> oR (eval e1 env) (eval e2 env)
  | Not(x) -> noT (eval x env)
  | Eq(e1,e2) -> eq_int (eval e1 env) (eval e2 env)
  | Subb(e1,e2) -> sub (eval e1 env) (eval e2 env)
  | Add(e1,e2) -> add (eval e1 env) (eval e2 env)
  | Mull(e1,e2) -> mul (eval e1 env) (eval e2 env)
  | Is_zero(x) -> is_zero (eval x env)
  | Ifthenelse(cond,e1,e2) -> let g = eval cond env in
                                (match (typecheck "bool" g, g) with
                                  | (true, Bool(true)) -> eval e1 env
                                  | (true, Bool(false)) -> eval e2 env
                                  | (_,_) -> failwith("Run Time Err"))
  | Let(x,e1,e2) -> eval e2 (bind env x (eval e1 env))
  | Fun(x,e) -> Closure(x, e, env)
  | LetRec(name,x,e,body) -> let en = bind (env) (name) (RecClousure(name,x,e,env)) in
                                      eval body en
  | Applyy(ex,arg) -> let vex = eval ex env in
                          (match vex with
                            | Closure(pf,bd,envc) -> let vpa = eval arg env in
                                                      let nenv = bind envc pf vpa in
                                                        eval bd nenv 
                            | RecClousure(nm,pf,bd,envr) -> let vpf = eval arg env in
                                                              let recenv = bind envr nm vex in 
                                                                let fenv = bind recenv pf vpf in
                                                                  eval bd fenv
                            | _ -> failwith("Val fun err"));;

(*test*)
let myfat = LetRec("fat", "x", Ifthenelse(Eq(Den "x", Cint 0), Cint 1, 
                                Mull(Den "x", Applyy(Den "fat", Subb(Den "x", Cint 1)))), Applyy(Den "fat", Cint 3));;

eval myfat emptyenv;;