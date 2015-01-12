structure InternalResult = struct
  datatype 'a t =
    Fail of string * string list * string
    | Partial of (string -> 'a t)
    | Done of string * 'a
  fun translate (Fail(input, stack, message)) =
    ParseResult.Fail(input, stack, message)
    | translate (Partial k) =
      ParseResult.Partial (fn a => translate (k a))
    | translate (Done(input, result)) =
      ParseResult.Done(input, result)
end

