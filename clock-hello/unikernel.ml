open Lwt

module Main (C: V1_LWT.CONSOLE) (CLK: V1.CLOCK) = struct

  let start c clock =
    for_lwt i = 0 to 4 do
      lwt () = OS.Time.sleep 1.0 in
      C.log c (Printf.sprintf "time is %f" (CLK.time ()));
      return ()
    done

end
