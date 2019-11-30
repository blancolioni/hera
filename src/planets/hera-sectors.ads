private with Ada.Containers.Doubly_Linked_Lists;

with Hera.Objects;

with Hera.Commodities;
with Hera.Terrain;

with Hera.Quantities;

package Hera.Sectors is

   type Root_Sector_Type is
     new Hera.Objects.Root_Hera_Object with private;

   function X
     (Sector : Root_Sector_Type'Class)
      return Positive;

   function Y
     (Sector : Root_Sector_Type'Class)
      return Positive;

   function Terrain
     (Sector : Root_Sector_Type'Class)
      return Hera.Terrain.Terrain_Type;

   type Sector_Type is access constant Root_Sector_Type'Class;

   type Sector_Array is
     array (Positive range <>) of Sector_Type;

   function Resources
     (Sector : Root_Sector_Type'Class)
      return Hera.Commodities.Commodity_Array;

   function Yield
     (Sector   : Root_Sector_Type'Class;
      Resource : Hera.Commodities.Commodity_Type)
      return Hera.Quantities.Quantity_Type;

   function Remaining
     (Sector   : Root_Sector_Type'Class;
      Resource : Hera.Commodities.Commodity_Type)
      return Hera.Quantities.Quantity_Type;

   procedure Mine_Resources
     (Sector   : Root_Sector_Type'Class;
      Resource : Hera.Commodities.Resource_Type;
      Quantity : Hera.Quantities.Quantity_Type);

private

   type Sector_Deposit is
      record
         Resource : Hera.Commodities.Resource_Type;
         Quantity : Hera.Quantities.Quantity_Type;
         Yield    : Hera.Quantities.Quantity_Type;
      end record;

   package Sector_Deposit_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Sector_Deposit);

   type Root_Sector_Type is
     new Hera.Objects.Root_Hera_Object with
      record
         X, Y     : Positive;
         Terrain  : Hera.Terrain.Terrain_Type;
         Deposits : Sector_Deposit_Lists.List;
      end record;

   overriding function Log_Id
     (Sector : Root_Sector_Type)
      return String
   is (Sector.X'Image & Sector.Y'Image);

   function Terrain
     (Sector : Root_Sector_Type'Class)
      return Hera.Terrain.Terrain_Type
   is (Sector.Terrain);

   function X
     (Sector : Root_Sector_Type'Class)
      return Positive
   is (Sector.X);

   function Y
     (Sector : Root_Sector_Type'Class)
      return Positive
   is (Sector.Y);

end Hera.Sectors;
