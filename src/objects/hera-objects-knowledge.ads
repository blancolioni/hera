private with Ada.Containers.Hashed_Maps;
private with Hera.Identifiers;

with Hera.Knowledge;

package Hera.Objects.Knowledge is

   type Object_Knowledge_Type is tagged private;

   procedure Update_Knowledge
     (Object_Knowledge : in out Object_Knowledge_Type;
      Object           : not null access constant Root_Hera_Object'Class;
      Knowledge_Level  : Hera.Knowledge.Knowledge_Level_Type);

   function Knowledge_Level
     (Object_Knowledge : Object_Knowledge_Type;
      Object           : Root_Hera_Object'Class)
      return Hera.Knowledge.Knowledge_Level_Type;

private

   package Knowledge_Maps is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Hera.Identifiers.Object_Identifier,
        Element_Type    => Hera.Knowledge.Knowledge_Level_Type,
        Hash            => Hash,
        Equivalent_Keys => Hera.Identifiers."=",
        "="             => Hera.Knowledge."=");

   type Object_Knowledge_Type is
     new Knowledge_Maps.Map with null record;

end Hera.Objects.Knowledge;
