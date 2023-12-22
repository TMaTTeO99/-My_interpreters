(*
  Esercitazione 27/11/20   

  Progettare e realizzare un interprete per un linguaggio didattico:
    1) Funzionale
    2) Con funzioni dichiarate globalmente
    3) Le funzioni non possono essere dichiarate localmente in altre funzioni
    4) Un programma in questo linguaggio ha la forma: P = [fun_dcl list] main_exp
*)

(*
  1) Funzionale:  significa che le funzioni possono essere di ordine superiore, 
  (Possono essere passate come parametri ad altre funzioni)   
  2) Con funzioni dichiarate globalmente : significa che dobbiamo definire due ambienti, un ambiente per le espressioni
     del main, e un ambiente in cui andare a inserire le valutazioni delle funzioni globali dichiarate 
  3) Le funzioni non possono essere dichiarate localmente in altre funzioni: significa che bisogna definire un espressione
     diversa per le funzioni da quelle dell albero di sintassi astratta in modo da non poter dedfinire funzioni 
     all interno di un let
  4) Forma del programma 
*)

(*
  comincio con la definizione dell'albero di sintassi astratta
*)

(*definisco un identificatore*)
type ide = string;;


type exp = 
  | CInt of int
  | Booltrue
  | Boolfalse
  | Den of ide
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
  | Add of exp * exp
  | Sub of exp * exp
  | Mull of exp * exp
  | EQ of exp * exp
  | Ifthenelse of exp * exp * exp
  | Let of ide * exp * exp
  | MyApply of ide * exp;;

(*
  Il tipo funzione (riguarda pero la sintassi della funzione, serve per far si che le funzioni non possano essere
  definite all'interno di un costrutto let ad esempio) 
  prende un ide che è il nome della funzione, prende un ide che è il parametro formale e prende 
  un exp che è il corpo della funzione
*)
type def = 
  | Fun of ide * ide * exp;;

(*
  Definisco ra gli evaluetion type   
*)
type evT = 
  | Int of int
  | Bool of bool
  | Unbound;;

(*
  Qui ora definisco l'ambiente, abbiamo detto che servono due ambienti uno per inserire le info riguardanti le fun 
  definite globalmente, e un ambiente per tenere traccia delle info riguardanti le espressioni all interno del
  programma main, prima però definisco il tipo del programma   
*)

(*
  Tipo dell'programma: il programma è una coppia: <lista di funzioni definite globalmente, exp del main>    
*)
type prog = 
  | Prog of (def list) * exp;;

(*
  definisco l ambiente come una lista paramentrica rispetto al tipo degli elementi, Nota: potrei anche 
  definirlo come una funzione esempio: 
    
    exception envempty
    type 'v env = ide -> 'v
    let rec bind env i v = function x -> if x = i then v else env x;;
    let env0 = function x -> raise envempty;;
*)
(*
  Nel mio caso lo voglio definire come una lista e non come una funzione   
*)
type 'v env = (ide * 'v) list;;

let rec loockup en x  = match en with
  | [] -> failwith("Run time err not var found")
  | (i,v)::y -> if x = i then v else loockup y x;;

let bind env x v = (x,v)::env;;

let emptyen= [];;

(*
  Definisco il typecheker   
*)
let typecheck tipo descrit = 
  match tipo with
  | "int" -> (match descrit with
              | Int(n) -> true
              | _ -> false)
  
  | "bool" -> (match descrit with
              | Bool(b) -> true
              | _ -> false)
  | _ -> failwith("ERROR TYPE");;

(*
  Definisco qualche funzione primitiva ad esempio l' eq   
*)

let eq x y = match (typecheck "int" x, typecheck "int" y, x, y) with
  | (true,true,Int(n),Int(m)) -> Bool(n=m)
  | (_,_,_,_) -> failwith("Run time ERROR type");;

let and_ x y = match (typecheck "bool" x, typecheck "bool" y, x, y) with
  | (true,true,Bool(n),Bool(m)) -> Bool(n && m)
  | (_,_,_,_) -> failwith("Run time ERROR type");;

let or_ x y = match (typecheck "bool" x, typecheck "bool" y, x, y) with
  | (true,true,Bool(n),Bool(m)) -> Bool(n || m)
  | (_,_,_,_) -> failwith("Run time ERROR type");;

let not_ x = match (typecheck "bool" x, x) with
  | (true, Bool(n)) -> Bool(not n)
  | (_,_) -> failwith("Run time ERROR type");;

let sub_ x y = match (typecheck "int" x, typecheck "int" y, x, y) with
  | (true,true,Int(n),Int(m)) -> Int(n-m)
  | (_,_,_,_) -> failwith("Run time ERROR type");;


let add_ x y = match (typecheck "int" x, typecheck "int" y, x, y) with
  | (true,true,Int(n),Int(m)) -> Int(n+m)
  | (_,_,_,_) -> failwith("Run time ERROR type");;

let mull x y = match (typecheck "int" x, typecheck "int" y, x, y) with
  | (true,true,Int(n),Int(m)) -> Int(n*m)
  | (_,_,_,_) -> failwith("Run time ERROR type");;

(*
  Qui ora devo implementare il ciclo dell interprete, Nota: in questo caso nel ciclo dell interprete devo
  inserire due ambienti, quello in cui sono inserite le funzioni globali e quello  in cui sono inserite le valutazioni
  delle espressioni che si possono incotreare nell exp main     
*)

(*
  Note riguardanti il costrutto let e l if then else:
  In questo caso non possono essere definite funzioni annidate, possono però essere richiamate ad esempio nel let 
  quindi in sostanza non cambia nulla rispetto al linguaggio visto a lezione
  
*)

let rec eval e enve envf = match e with
  | CInt(n) -> Int(n)
  | Booltrue -> Bool(true)
  | Boolfalse -> Bool(false)
  | Den(n) -> loockup enve n
  | Add(e1,e2) -> add_ (eval e1 enve envf) (eval e2 enve envf)
  | Sub(e1,e2) -> sub_ (eval e1 enve envf) (eval e2 enve envf)
  | And(e1,e2) -> and_ (eval e1 enve envf) (eval e2 enve envf)
  | Or(e1,e2) -> or_ (eval e1 enve envf) (eval e2 enve envf)   
  | Not(e1) -> not_ (eval e1 enve envf)
  | Mull(e1,e2) -> mull (eval e1 enve envf) (eval e2 enve envf)
  | EQ(e1,e2) -> eq (eval e1 enve envf) (eval e2 enve envf)
  | Let(x,e1,e2) -> eval e2 (bind enve x (eval e1 enve envf)) envf
  | Ifthenelse(cond,e1,e2) -> let g = eval cond enve envf in
                                  (match g with
                                  | Bool(true) -> eval e1 enve envf
                                  | Bool(false) -> eval e2 enve envf
                                  | _ -> failwith("Not Bool Condition"))
  | MyApply(e1,e2) ->  let vfun = loockup envf e1 in
                          (match vfun with
                          | Fun(nm,pf,cp) -> let vpt = eval e2 enve envf in
                                                let nenv = bind emptyen pf vpt in (*qui inserisco in emptyenv perche è quello l ambiente in cui la funzione è definita SCOPING STATICO*)
                                                  eval cp nenv envf  
                          | _ -> failwith("Run time err fo function"));;

(*
  A questo punto devo definire le funzioni per la lettura del programma e per l inserimento dello funzioni all interno
  dell ambiente globale   
*)

(*
  Definisco l'inserimento nell ambiente globale   
*)


let rec insenvf lst = 
  match lst with 
  | [] -> emptyen
  | Fun(nm,pf,e)::t -> bind (insenvf t) (nm) (Fun(nm,pf,e));; 

(*
  Definisco l'operazione di lettura del programma, il tipo del programma è il seguente
  type prog = Prog of (def list) * exp
  end type def = Fun of ide * ide * exp
*)

let evalprog p = 
  match p with
  | Prog(de,e) -> let genv = (insenvf de) in eval e emptyen genv;;


  (*test*)
  let p1 = Prog([ ], Add(CInt 4, CInt 5));;
  let p2 = Prog([Fun("succ", "x", Add(Den "x", CInt 1))], Add(MyApply("succ", CInt 4), CInt 5));;
  let p3 = Prog([Fun("tria", "x", Ifthenelse(EQ(Den "x", CInt 0), CInt 5, Add(Den "x", MyApply("tria", Sub(Den "x", CInt 1)))))],MyApply("tria", CInt 4));;

  evalprog p1;;
  evalprog p2;;
  evalprog p3;;
