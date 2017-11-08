# lambda-interpreter

Very primitive lambda interpreter. 

Parser by [dmringo](https://github.com/dmringo).

With [stack](https://docs.haskellstack.org/en/stable/README/) installed:

``` shell
> git clone https://github.com/masonwr/Lambda-Interpreter.git
> cd Lambda-Interpreter
> make
> make run
stack exec lambda-interpreter-exe
Welcome to a basic lambda interpreter.
\x.y
(λx. y)
(\x.x x)(\y.y)
(λy. y)
```
