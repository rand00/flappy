
open Tyxml
module H = Html

let content = [
  H.h1 [ H.txt "flap flap" ];
  H.div ~a:[H.a_id Constants.html_id] [];
  H.noscript [H.txt "Sorry, you need to enable JavaScript to see this page."]
]

let page =
  H.html
    (H.head
       (H.title (H.txt "Typesafe html"))
       [ H.meta ~a:[H.a_charset "utf-8"] ();
         (* meta ~a:[a_name "viewport";
          *          a_content "width=device-width,initial-scale=1.0"] (); *)
         H.script (H.txt "")
           ~a:[H.a_mime_type "text/javascript";
               H.a_defer ();
               H.a_src "flap.js"];
       ])
    (H.body content)

let () = page |> H.pp () Format.std_formatter 

