{
  config,
  lib,
  ...
}: {
  age.secrets.github-token = {
    file = ./github-token.age;
    owner = "dengo123"; # oder dein späterer Benutzername
  };
}
