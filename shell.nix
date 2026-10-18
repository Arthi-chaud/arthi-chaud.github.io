{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  packages = with pkgs; [
    ruby
    jekyll
    bibtex-tidy
    ltex-ls
  ];
}
