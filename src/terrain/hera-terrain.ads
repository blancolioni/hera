private with Ada.Containers.Doubly_Linked_Lists;
private with Tropos;

with Hera.Objects;

with Hera.Commodities;

package Hera.Terrain is

   type Root_Terrain_Type is
     new Hera.Objects.Root_Localised_Object with private;

   function Hazard_Level (Terrain : Root_Terrain_Type'Class) return Unit_Real;
   function Resource_Chance
     (Terrain  : Root_Terrain_Type'Class;
      Resource : Hera.Commodities.Resource_Type)
      return Unit_Real;

   type Terrain_Type is access constant Root_Terrain_Type'Class;

   function Exists (Tag : String) return Boolean;
   function Get (Tag : String) return Terrain_Type
     with Pre => Exists (Tag);

   type Terrain_Array is array (Positive range <>) of Terrain_Type;

   function All_Terrain return Terrain_Array;

private

   type Resource_Chance_Record is
      record
         Resource : Hera.Commodities.Commodity_Type;
         Chance   : Unit_Real;
      end record;

   package Resource_Chance_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Resource_Chance_Record);

   type Root_Terrain_Type is
     new Hera.Objects.Root_Localised_Object with
      record
         Priority        : Positive;
         Hazard_Level    : Unit_Real;
         Resource_Chance : Resource_Chance_Lists.List;
      end record;

   overriding procedure Save
     (Terrain : Root_Terrain_Type;
      To        : in out Tropos.Configuration);

   overriding procedure Load
     (Terrain : in out Root_Terrain_Type;
      From      : Tropos.Configuration);

   function Hazard_Level (Terrain : Root_Terrain_Type'Class) return Unit_Real
   is (Terrain.Hazard_Level);

   procedure New_Terrain
     (Terrain : Terrain_Type);

end Hera.Terrain;
