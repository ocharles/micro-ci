with (import <nixpkgs> {});
with lib;

let go = x: path:
     if builtins.isAttrs x then
       if x ? type && x.type == "derivation" then
         [ path ]
       else
         concatMap (name: go x."${name}" (path ++ [ name ]))
           (builtins.attrNames x)
     else 
       throw "Not an attribute set";
in x: go x []
