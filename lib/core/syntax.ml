(** Binding operators for Result-based error handling.

    [let*] is [Result.bind]: on [Ok v] the continuation runs with [v];
    on [Error e] the whole expression short-circuits to [Error e]. *)

let ( let* ) = Result.bind
