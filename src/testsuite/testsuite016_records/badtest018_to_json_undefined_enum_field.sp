js: json_string;

type enum is (ape, bat);
type rt is record
  a : enum;
  b : enum;
end rt;
r : rt;

r.a := ape;
r.b := bat;
records.to_json( js, r );
env js;

-- vim: set ft=ada
