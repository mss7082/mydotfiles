# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./ledger.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # networking.hostName = "nixos"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
 # networking.networkmanager.enable = true; #Activate wifi with "nmcli device wifi connect <SSID> password <password>
 networking.wireless.networks = {
  "Searching..." = {
    pskRaw = "b8ea9095abc9bce69d3d786b06d0defcac891247df7e1f584ca1b9c02bf37780";
  };
};

  # Set your time zone.
  time.timeZone = "Europe/Dublin";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp4s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "dvorak-programmer";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable Emacs Service
  services.emacs.enable = true;
  services.emacs.package = import ./emacs.nix { pkgs = pkgs; }; 

  # Enable Docker
  virtualisation.docker.enable = true;


  # Enable the GNOME Desktop Environment.
  #services.xserver.displayManager.gdm.enable = true;
  #services.xserver.desktopManager.gnome.enable = true;
  #services.xserver.windowManager = {
  #  xmonad = {
  #    enable = true;
  #    enableContribAndExtras = true;
  #    };
  #  };
  services.xserver.windowManager.exwm.enable = true;
  

  # Configure keymap in X11
  services.xserver.layout = "us";
  services.xserver.xkbVariant = "dvp";
  # services.xserver.xkbOptions = "eurosign:e";


  # Enable sound.
  sound.enable = true;
  #sound.mediaKeys = {
  #  enable = true;
  #  volumeStep = "5%";
  #};

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  # New group for Ledger
  users.groups.plugdev = {};

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.moses = {
    isNormalUser = true;
    shell = pkgs.fish;
    extraGroups = [ "wheel" "plugdev" "networkmanager" "docker"]; # Enable ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    (import ./emacs.nix { inherit pkgs; })
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    picom
    wget
    firefox
    libreoffice
    gnome3.gnome-system-monitor
    authy
    dig
    brave
    git
    alacritty
    fish
    vscode-fhs
    bitwarden
    htop
    cointop
    discord
    qutebrowser
    vlc
    ledger-live-desktop
    isync
    mu
    exercism
    haskell-language-server
    ghc
    cabal-install
    stack
    ormolu
    slock
    pasystray
    dunst
    feh
    brightnessctl
    scrot
    upower
    tlp
    playerctl
    logseq
    #tdlib #using the unstable version in user env
    filezilla
    gnumake
    pkg-config
    gcc
  ];

  nixpkgs.config.allowUnfree = true;

  nixpkgs.config.permittedInsecurePackages = [
    "electron-9.4.4"
    "electron-13.6.9"
  ];


  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  programs.fish.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
  # Backlight for display
  programs.light.enable = true;
  #services.actkbd = {
  #  enable = true;
  #  bindings = [
  #    { keys = [ 225 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -A 3"; }
  #    { keys = [ 224 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -U 3"; }
  #    { keys = [ 113 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/runuser -l moses -c 'amixer -q set Master toggle'"; }
  #    { keys = [ 114 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/runuser -l moses -c 'amixer -q set Master 5%- unmute'"; }
  #    { keys = [ 115 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/runuser -l moses -c 'amixer -q set Master 5%+ unmute'"; }
  #  ];
  #};

  # List services that you want to enable:

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplip ]; 

  # Enable Bluetooth Services
  services.blueman.enable = true;
  hardware.bluetooth.enable = true;
  hardware.bluetooth.settings = {
  General = {
    Enable = "Source,Sink,Media,Socket";
  };
};

  #Enable extra audio codecs
  hardware.pulseaudio = {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  #Enable Flatpak
  services.flatpak.enable = true;

  #Enable gnome-keyring
  services.gnome.gnome-keyring.enable = true;

  #Enable xdg desktop Integration for flatpak
  xdg = {
  portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-wlr
      xdg-desktop-portal-gtk
    ];
    gtkUsePortal = true;
           };
        };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Automatic Upgrades
  system.autoUpgrade.enable = true;
  system.autoUpgrade.allowReboot = true;

  nix.gc.automatic = true;
  nix.gc.dates = "03:15";

  # Plutus config
  nix = {
    binaryCaches          = [ "https://hydra.iohk.io" "https://iohk.cachix.org" ];
    binaryCachePublicKeys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=" ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

}

