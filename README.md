# Flappy
Implementation of some kind of flappy-bird game.
Primary point of this is to showcase how to use OCaml and its existing 
libraries, to write powerful but simple code with an Elm-like structure. 
See `flap.ml` for the game-code.

### Updates
* Other birds can now revive a fallen bird by flying into feathers
* Begun drawing own assets
* Added local multiplayer
* Added smoot multi-directional vector-based movement
* Added homing dogs 
* ... 

### Goals:
- [x] Elm-like code using frp (`react`) + typesafe html (`tyxml`) + 
virtual DOM (`reactiveData` + `tyxml_js`). 
  * Simple
  * Typesafe + functional - at least concerning the body of the application
  * Minimal direct usage of js-bindings.
  * Visualization code separated from game-model update.
- [x] Avoid frameworks, use libraries only.
- [x] Coming up with an elegant solution to *identity of object*-related 
problems. E.g. homing missiles.
  * Having some way to calculate who the referenced entity is from the data is fine for homing missiles. Depends on what one wants to do. 
- [ ] Composability of 'components' 
  * Is the Virtual DOM part of the code effectively composeable, when having 
    nested reactive nodes?
    * If not, maybe use Janestreets `Incr_dom` - not compatible with `react`?

#### Non-goals:
* Effective visualization 
* Interesting gameplay 

### How to fly
* `git clone` the repository.
* See the needed dependencies in the `_tags` file - use `opam` to install them.
* Run `./compile.sh` which generates an `webout/index.html` and `webout/flap.js`.
* Open `webout/index.html` in your favorite browser
* Use w/a/s/d + i/j/k/l + arrow-keys to fly (depending on the value of `players` in `flap.ml`)

