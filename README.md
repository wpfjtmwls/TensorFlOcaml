# TensorFlOcaml

Final Project for CS3110 (Functional Programming) at Cornell

Dependencies:
Ocaml >= 4.06.0
Opam 
Owl Numerical Library
OpenBlas

Setup:
Install OpenBlas onto your system. If you're using OSX and have homebrew then you can do this with 
```bash
brew install openblas
```
You'll then want to get the latest version of the owl repo. 
```bash 
git clone https://github.com/owlbarn/owl.git
```
does the trick.
I put the repo in '~/.opam/packages' but you can put it wherever you want really.
You'll then want to follow the instructions on the owl github page. 
First we want to see if we have any outside dependencies missing. Run the following command while inside the owl repo to see 
what you're missing.
```bash 
jbuilder external-lib-deps --missing @install
```
Most likely you'll be missing something called Plplot. Confusion may stem from trying to install Plplot via opam. Turns out, ocamls plplot
depends on the plplot software, which you'll need to install from http://plplot.sourceforge.net/downloads.php or if you're on mac
```bash 
brew install plplot
```
 . If you install via brew, you'll most likely need to run the following commands 
```bash
echo 'export PATH="/usr/local/opt/icu4c/bin:$PATH"' >> ~/.zshrc
echo 'export PATH="/usr/local/opt/icu4c/sbin:$PATH"' >> ~/.zshrc
```
Replace ".zshrc" with ".bashrc" if you use bash instead of zshrc. 
Once you've done that 
```bash
opam install plplot
```
should work. Finally 
```bash
make && make install
```
. If this works you should be able to run 
```bash
#require "owl-top"
```
in utop succsessfully. 