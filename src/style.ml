
let sp = Printf.sprintf

type t = string

let make l = String.concat "; " l

let position = function
  | `Fixed -> "position: fixed"
  | `Absolute -> "position: absolute"
  | `Relative -> "position: relative"

let background_image url = sp "background-image: url(%s)" url
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
