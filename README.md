# bluebell-nilsson

Control flow when some values are absent.

Provides a special ```nlet``` macro that analyzes the code and sees what depends on which symbols. And if at runtime, if a symbol is nil, then the depending code will not be executed. This way, we avoid a lot of ugly if-let-forms.

## Usage

Use the ```nlet``` form that works almost like a let, except that

  * If a symbol is nil, any subsequent bindings depending on that symbol will not be evaluated.
  * There are directives to prefix each binding, to specify special behaviour:
    - ```:req-one``` will provoke an error if all symbols are nil
    - ```:req``` will provoke an error if at least one symbol is nil
    - ```:exclude``` will not track the symbol, so that bindings that contain it will be evaluated even if it is nil.

## Why not exceptions?
 
  * Hard to understand the origin of an exception
  * Every function gets a potentially unrestricted set of possible outcomes, 
    hard to reason about code.
  
But exceptions are still good, sometimes. We can't check for every possible thing that could go wrong. For instance, we might fail to allocate a value dynamically, and it would be unreasonable to check every allocated value with a return code.

Use exceptions locally within a function, or as a way of reporting bugs or very serious errors. But think carefully about what is best, a return value or an exception?


## License

Copyright © 2018 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
