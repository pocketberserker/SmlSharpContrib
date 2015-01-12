structure ParseResult = struct

  datatype 'a t =
    Fail of string * string list * string
    | Partial of (string -> 'a t)
    | Done of string * 'a

  fun map (Fail(input, stack, message)) _ =
    Fail(input, stack, message)
    | map (Partial k) f =
      Partial(fn s => map (k s) f)
    | map (Done(input, result)) f =
      Done(input, f result)

  fun feed (f as Fail _) s = f
    | feed (Partial k) s = k s
    | feed (Done(input, result)) s =
      Done(input ^ s, result)

  fun done (Fail _ ) = NONE
    | done (Partial _) = NONE
    | done (Done(_, result)) = SOME result
end

