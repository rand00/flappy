
let sp = Printf.sprintf

type t = string

let make l = String.concat "; " l

let position = function
  | `Fixed -> "position: fixed"
  | `Absolute -> "position: absolute"
  | `Relative -> "position: relative"

let background_image url = sp "background-image: url(%s)" url
let background_color color = sp "background-color: %s" color
let background_size = function
  | `Cover -> "background-size: cover"
  | `Contain -> "background-size: contain"
  | `Auto -> "background-size: auto"

let dist = function
  | `Px px -> sp "%dpx" px

let width d = sp "width: %s" (dist d)
let heigth d = sp "height: %s" (dist d)

let left d = sp "left: %s" (dist d)
let top d = sp "top: %s" (dist d)
let bottom d = sp "bottom: %s" (dist d)
let right d = sp "right: %s" (dist d)

let rotate = function
  | `Deg deg -> sp "transform: rotate(%ddeg)" deg

let z_index i = sp "z-index: %d" i

let font_family = function
  | `Courier_new -> sp "font-family: \"Courier New\", monospace"

let font_size d = sp "font-size: %s" (dist d)

let text_shadow d0 d1 d2 color =
  sp "text-shadow: %s %s %s %s" (dist d0) (dist d1) (dist d2) color

let color color = sp "color: %s" color 
