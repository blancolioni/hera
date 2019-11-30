package Hera.Identifiers is

   type Object_Identifier is new String (1 .. 8);

   function Next_Identifier return Object_Identifier;

end Hera.Identifiers;
