(** Computer opponent. Owner: Jah. Backup: Shehroze. *)

type strategy =
  | Random
  | Greedy_capture
  | Avoid_threats

let choose ?strategy (_g : Game.t) : Move.t option =
  ignore strategy;
  failwith "Bot.choose: not implemented yet"

let evaluate (_pos : Position.t) : int =
  failwith "Bot.evaluate: not implemented yet"
