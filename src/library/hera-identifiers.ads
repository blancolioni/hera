with Ada.Containers;
private with Ada.Strings.Fixed.Hash;

package Hera.Identifiers is

   type Object_Identifier is new String (1 .. 8);

   function Next_Identifier return Object_Identifier;

   function Hash
     (Identifier : Object_Identifier)
      return Ada.Containers.Hash_Type;

private

   function Hash
     (Identifier : Object_Identifier)
      return Ada.Containers.Hash_Type
   is (Ada.Strings.Fixed.Hash (String (Identifier)));

end Hera.Identifiers;
