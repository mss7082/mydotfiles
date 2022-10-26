/*
This is a nix expression to build Emacs and some Emacs packages I like
from source on any distribution where Nix is installed. This will install
all the dependencies from the nixpkgs repository and build the binary files
without interfering with the host distribution.
To build the project, type the following from the current directory:
$ nix-build emacs.nix
To run the newly compiled executable:
$ ./result/bin/emacs
*/
{ pkgs ? import <nixpkgs> {} }: 

let
  myEmacs = pkgs.emacs; 
  emacsWithPackages = (pkgs.emacsPackagesFor myEmacs).emacsWithPackages; 
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [ 
    magit          # ; Integrate git <C-x g>
    zerodark-theme # ; Nicolas' theme
    lsp-mode
    lsp-ui
  ]) ++ (with epkgs.melpaPackages; [ 
   # undo-tree      # ; <C-x u> to show the undo tree
   # zoom-frm       # ; increase/decrease font size for all buffers %lt;C-x C-+>
   lsp-haskell
   neotree
   haskell-mode
   evil
   evil-dvorak
   doom-modeline
   all-the-icons
   doom-themes
   which-key
   ivy
   counsel
   swiper
   direnv
   org-bullets
   company
   projectile
   rainbow-delimiters
   ormolu
   reformatter
   helm-lsp
   pdf-tools
   yaml-mode
   nix-mode
   undo-fu-session
   undo-fu
   dashboard
   org-mime
   telega
   super-save
   perspective
   hydra
   elfeed
   org-roam
   company-prescient
   ivy-prescient
   evil-nerd-commenter
   company-tabnine
   nix-haskell-mode
   
  ]) ++ (with epkgs.elpaPackages; [ 
    auctex         # ; LaTeX mode
    beacon         # ; highlight my cursor when scrolling
    nameless       # ; hide current package name everywhere in elisp code
  ]) ++ [
    pkgs.notmuch   # From main packages set 
  ])
