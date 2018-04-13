open Base_types

type t = challenge list ref

let create () : t = ref []

    let list_to_string elt_to_string l =
      let rec aux = function
          [x] -> elt_to_string x
        | x :: xs -> elt_to_string x ^ "; " ^ (aux xs)
        | [] -> assert false
      in
      "[" ^
        (match l with
           [] -> ""
         | _ -> aux l)
        ^ " ]"

let challenges_to_string challenges =
  list_to_string (fun i -> string_of_int (i#get_id))
  @@ List.map Ttt_server_lib_challenge.id challenges

let log_debug_challenges challenges =
  Logs.debug (fun m ->
      m "challenges: %s"
        (challenges_to_string challenges)
    )

let add l (x : challenge) =
  l := !l @ [x];
  Logs.debug (fun m -> m "adding challenge");
  log_debug_challenges !l

let get_all l =
  !l

let remove_id challenges id =
  let rec aux id =
    let open Base_types in
    function
      [] -> [], Id_not_present
    | challenge :: xs ->
       let id2 = (Ttt_server_lib_challenge.id challenge)#get_id in
       if id#get_id = id2 then
         xs, Deleted(challenge)
       else
         let a,b = aux id xs in
         challenge :: a, b
  in
  match aux id !challenges with
  | _ ,Id_not_present -> Logs.debug (fun m -> m "too late delete");Id_not_present
  | l, del -> challenges := l; Logs.debug (fun m -> m "deleted challenge"); log_debug_challenges !challenges; del

let remove_user_challenges (challenges : t) user =
  let open Ttt_server_lib_challenge in
  let rec aux = function
    | [] -> []
    | c :: cs ->
       if challenger c = user then
         begin
           Logs.debug (fun m ->
               m "Deleted challenge %d for user %s"
                 (id c)#get_id
                 user
             );
           aux cs
         end
       else
         c :: aux cs
  in
  Logs.debug (fun m -> m "deleted all challenges for user %s" user);
  log_debug_challenges !challenges;
  challenges := aux !challenges
