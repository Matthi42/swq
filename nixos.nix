swq:
{ lib, config, pkgs, ... }:
let
  swqServer = swq."${pkgs.system}";
  cfg = config.services.swq;
in
with lib; {
  options.services.swq = {
    enable = mkEnableOption "Enable the SWQ Server";
    domainName = mkOption { type = types.str; };
  };
  config = mkIf cfg.enable {
    users = {
      users.swq = {
        isSystemUser = true;
        description = "swq server user";
        group = "swq";
      };
      groups.swq = { };
    };

    systemd.services.swq = {
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      # path = [ ];
      serviceConfig = {
        User = "swq";
        Group = "swq";
        Type = "simple";
        ExecStart = "${swqServer}/bin/swq ${./style.css}";
      };
    };

    services.nginx = {
      enable = true;
      virtualHosts."${cfg.domainName}" = {
        forceSSL = true;
        enableACME = true;
        locations."/".proxyPass = "http://127.0.0.1:3003";
      };
    };
  };
}
