# simpl

This is a stack project, meaning you should have stack (currently better alternative to cabal) installed on your machine for everything to run properly. First try building the project by using `stack build` command from project folder. That should pull down all the necessary files (it might take a while when you try it for the first time). After build has finished try running `stack repl`. That will load all of your project files in haskell repl (a.k.a. GHCi) so that you can test individual functions.

If you stumble upon any problems post your questions on our google group.

https://groups.google.com/forum/#!forum/haskell-fer

For fifth part of this project you will have to add some non standard packages your self, like `parsec` for example. You can add new packages by listing them in `simpl.cabal` file under `build-depends:` section, like this :

build-depends:       base >= 4.7 && < 5, mypackage01, mypackage02, ...

Feel free to add what ever you think you need.