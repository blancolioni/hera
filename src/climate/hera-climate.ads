private with Ada.Containers.Doubly_Linked_Lists;
private with Tropos;

with Hera.Objects;

with Hera.Terrain;

package Hera.Climate is

   type Root_Climate_Type is
     new Hera.Objects.Root_Localised_Object with private;

   function Default_Terrain
     (Climate : Root_Climate_Type'Class)
      return Hera.Terrain.Terrain_Type;

   function Terrain_Chance
     (Climate : Root_Climate_Type'Class;
      Terrain : Hera.Terrain.Terrain_Type)
      return Unit_Real;

   type Climate_Type is access constant Root_Climate_Type'Class;

   function Exists (Tag : String) return Boolean;
   function Get (Tag : String) return Climate_Type
     with Pre => Exists (Tag);

   type Climate_Array is array (Positive range <>) of Climate_Type;

private

   type Terrain_Chance_Record is
      record
         Terrain : Hera.Terrain.Terrain_Type;
         Chance   : Unit_Real;
      end record;

   package Terrain_Chance_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Terrain_Chance_Record);

   type Root_Climate_Type is
     new Hera.Objects.Root_Localised_Object with
      record
         Hazard_Level    : Unit_Real;
         Default_Terrain : Hera.Terrain.Terrain_Type;
         Terrain_Chance  : Terrain_Chance_Lists.List;
      end record;

   overriding procedure Save
     (Climate : Root_Climate_Type;
      To        : in out Tropos.Configuration);

   overriding procedure Load
     (Climate : in out Root_Climate_Type;
      From      : Tropos.Configuration);

   function Default_Terrain
     (Climate : Root_Climate_Type'Class)
      return Hera.Terrain.Terrain_Type
   is (Climate.Default_Terrain);

   procedure New_Climate
     (Tag             : String;
      Hazard_Level    : Unit_Real;
      Default_Terrain : Hera.Terrain.Terrain_Type;
      Terrain_Chance  : Terrain_Chance_Lists.List);

end Hera.Climate;
