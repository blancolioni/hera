with Ada.Containers.Hashed_Maps;

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Hera.Identifiers.Maps is

   package Identifier_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Object_Identifier,
        Element_Type    => Element_Type,
        Hash            => Hash,
        Equivalent_Keys => "=",
        "="             => "=");

   subtype Map is Identifier_Maps.Map;

end Hera.Identifiers.Maps;
