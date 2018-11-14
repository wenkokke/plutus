{ mkInstance = { hydraPlayground, defaultMachine, ... }: node: { config, pkgs, lib, ... }:
  {
      imports = [ (defaultMachine node pkgs)
                ];

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 80 8080 ];
  };

  services.nginx = {
    enable = true;
    user = "plutus";

    recommendedGzipSettings = true;
    recommendedProxySettings = true;
    recommendedOptimisation = true;

    appendHttpConfig = ''
      server_names_hash_bucket_size 128;
      log_format compression '$remote_addr - $remote_user [$time_local] '
                       '"$request" $status $body_bytes_sent '
                       '"$http_referer" "$http_user_agent" "$gzip_ratio"';
    '';

    virtualHosts = {
      "~." = {
        listen = [{ addr = "0.0.0.0"; port = 80; }];
        extraConfig = ''
          return 301 https://$host$request_uri;
          '';
      };
      "${node.dns}" = {
        locations = {
          "/" = {
            proxyPass = "http://127.0.0.1:4000/";
            proxyWebsockets = true;
          };
        };
      };
    };
  };

  systemd.services.plutus-playground = {
    wantedBy = [ "nginx.service" ];
    before = [ "nginx.service" ];
    enable = true;
    path = [
      "${hydraPlayground.plutus-server-invoker}/bin"
    ];

    serviceConfig = {
      TimeoutStartSec = "0";
      Restart = "always";
      User = "plutus";
      PrivateTmp = true;
    };

    script = "plutus-playground-server webserver -b 127.0.0.1 -p 4000 ${hydraPlayground.plutus-playground-client}";
  };

};
}
