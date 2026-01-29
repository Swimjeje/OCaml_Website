Pour lancer le site (dans le dossier OcamlApp):
dune build
dune exec ./main.exe

Pour démarrer la proxy:
openvpn Downloads/pfSense-user-config.ovpn
ssh xubuntu@10.1.0.24

Pour installer:
yay -S opam
opam init
ocamlfind ocamlopt -linkpkg -package dream main.ml -o app 
./app
opam install dream
ocamlfind ocamlopt -linkpkg -package dream main.ml -o app
