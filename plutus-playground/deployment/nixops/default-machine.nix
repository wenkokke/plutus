{ machines, stdOverlays, ... }: node: pkgs:
{
      nixpkgs.overlays = stdOverlays;
      nix = {
        nixPath = [ "nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz"
                  ];
        binaryCaches = [ https://hydra.iohk.io https://cache.nixos.org https://mantis-hydra.aws.iohkdev.io ];
        requireSignedBinaryCaches = false;
        extraOptions = ''
          build-cores = 8
          auto-optimise-store = true
        '';
        trustedBinaryCaches = [ https://hydra.iohk.io https://mantis-hydra.aws.iohkdev.io ];
        binaryCachePublicKeys = [
          "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        ];
        gc.automatic = true;
		    gc.options = "--delete-older-than 7d";
      };
      imports = [ <nixpkgs/nixos/modules/virtualisation/amazon-image.nix> ];

      systemd.services.amazon-init.wantedBy = pkgs.lib.mkForce [ ];

      ec2.hvm = true;

      networking.timeServers = [ "1.amazon.pool.ntp.org" "2.amazon.pool.ntp.org" "3.amazon.pool.ntp.org" ];

      ## Disable journald ratelimiting.
      services.journald.rateLimitBurst = 0;

      ## This makes our networking stack ignore the AWS MTU advertisement of 9001,
      ## that silently breaks intra-VPC, for some reason.
      ## The intent of this is to reduce the MTU to 1500.
      networking.dhcpcd.extraConfig = ''
        nooption interface_mtu
      '';

      users.extraUsers.root.openssh.authorizedKeys.keys = machines.rootSshKeys;

      # services.journalbeat = {
      #   enable = true;
      #   extraConfig = ''
      #     journalbeat.cursor_state_file: "/var/lib/journalbeat/data/.cursor-state"
      #     journalbeat.pending_queue.file: "/var/lib/journalbeat/data/.pending-queue"
      #     journalbeat.clean_field_names: true
      #     output.elasticsearch:
      #       enabled: true
      #       hosts: ${builtins.toJSON esNodes}
      #       template.enabled: false
      #   '';
      # };

      services.fail2ban.enable = true;

      # services.riemann-tools = {
      #   enableHealth = true;
      #   riemannHost = "${machines.riemannA.dns}";
      # };

      # services.nginx.recommendedGzipSettings = true;
      # services.nginx.recommendedProxySettings = true;
      # services.nginx.recommendedOptimisation = true;
      # services.nginx.appendHttpConfig = ''
      #   server_names_hash_bucket_size 128;
      #   log_format compression '$remote_addr - $remote_user [$time_local] '
      #                    '"$request" $status $body_bytes_sent '
      #                    '"$http_referer" "$http_user_agent" "$gzip_ratio"';
      # '';
}
