sudo apt install opam
opam init
eval $(opam env)
opam install ounit2 -y
eval $(opam env)
opam install ocamlfind
eval $(opam env)
opam install ocamlbuild
eval $(opam env)
make build
make game
