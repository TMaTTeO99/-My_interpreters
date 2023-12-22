type ide = string;;
type 'v env = ide -> 'v;;
let bind r i v = function x -> if x = i then v else r x;;

exception EmptyException;;
let env0 = function x -> raise EmptyException;;


type exp = 
  | Cint of int
  | Cbool of bool
  | Add of exp * exp
  | Den of ide
  | Let of ide * exp * exp
  | Fun of ide * exp
  | Apply of exp * exp
  | Iterator of exp * exp * exp;;

type evT = 
  | Bool of bool
  | Int of int
  | VFun of ide * exp * evT env;;

let rec eval (e : exp) (n : evT env) : evT = match e with
  | Cint(n) -> Int(n)
  | Cbool(b) -> Bool(b)
  | Add(e1,e2) -> let ve1 = eval e1 n in
                    let ve2 = eval e2 n in
                      (match (ve1, ve2) with
                      | (Int(g), Int(h)) -> Int(g+h)
                      | _ -> failwith("EEEE"))
  | Den(x) -> n x
  | Let(i,e1,e2) -> let ve1 = eval e1 n in
                      let nex = bind n i ve1 in
                        eval e2 nex
  | Fun(i,ex) -> VFun(i,ex,n)
  | Apply(e1,arg) -> let ve1 = eval e1 n in
                        (match ve1 with
                          VFun(i,body,en) -> let varg = eval arg n in 
                                              let exn = bind n i varg in
                                                eval body exn
                          | _ -> failwith("ERRORFUN"))
  | Iterator(e1,e2,e3) -> let clo = eval e1 n in
                              (match clo with 
                              | VFun(i,body,en) -> let ve3 = eval e3 n in
                                                      let ve2 = eval e2 n in
                                                        let rec auxf i bd nn rip v = match rip with
                                                            | Int(0) -> v
                                                            | Int(valore) -> let nenv = bind nn i v in
                                                                    let t = eval bd nenv in
                                                                      (match t with
                                                                      | Int(x) -> auxf (i) (bd) (nn) (Int(valore-1)) (Int(x)) 
                                                                      | Bool(y) -> auxf (i )(bd) (nn) (Int(valore-1)) (Bool(y)) 
                                                                      | _ -> failwith("ERRORE"))
                                                        in auxf (i )(body )(en )(ve3 )(ve2 ));;
                              (*| _ -> failwith("null");;*)

let ff = Iterator(Fun("x", Add(Den "x", Cint 2)), Cint 5, Cint 10);;