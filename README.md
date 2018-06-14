# bluebell-nilsson

Control flow when some values are absent.

Why not exceptions?
 
  * Hard to understand the origin of an exception
  * Every function gets a potentially unrestricted set of possible outcomes
  
Use exceptions locally within a function, or as a way of reporting bugs.
But probably not a good idea to use it as a general error reporting mechanism.

## Usage

Use the ```nlet``` form that works almost like a let, except that

  * If a symbol is nil, any subsequent bindings depending on that symbol will not be evaluated.
  * There are directives to prefix each binding, to specify special behaviour:
    - ```:req-one``` will provoke an error if all symbols are nil
    - ```:req-all``` will provoke an error if at least one symbol is nil
    - ```:exclude``` will not track the symbol, so that bindings that contain it will be evaluated even if it is nil.

## License

Copyright © 2018 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
