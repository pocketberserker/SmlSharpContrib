structure Parser = struct

  type s =
    {
      Input : string,
      Pos : int,
      Complete : bool
    }

  fun bind p f =
    let
      fun inner s lose succ =
        let
          fun succ ss a = (f a) ss lose succ
        in
          p s lose succ
        end
    in
      inner
    end
end

