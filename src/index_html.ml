
open Tyxml
module H = Html

let js_div_id = "gaxel" 

let page =
  H.html
    (H.head
       (H.title (H.pcdata "Tyxml generated!"))
       [ H.meta ~a:[H.a_charset "utf-8"] ();
         (* meta ~a:[a_name "viewport";
          *          a_content "width=device-width,initial-scale=1.0"] (); *)
         H.script (H.pcdata "")
           ~a:[H.a_mime_type "text/javascript";
               H.a_defer ();
               H.a_src "gaxel.js"];
       ])
    (H.body
       [ H.div ~a:[H.a_id js_div_id] [];
         H.noscript [H.pcdata "Sorry, you need to enable JavaScript \
                           to see this page."]
       ])

let () = page |> H.pp () Format.std_formatter 

