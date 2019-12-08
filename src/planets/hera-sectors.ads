private with Ada.Containers.Doubly_Linked_Lists;

with Hera.Objects;

with Hera.Commodities;
with Hera.Surfaces;
with Hera.Terrain;

with Hera.Quantities;

package Hera.Sectors is

   subtype Elevation_Range is
     Real range -10_000.0 .. 20_000.0;

   type Root_Sector_Type is
     new Hera.Objects.Root_Hera_Object with private;

   function Elevation
     (Sector : Root_Sector_Type'Class)
      return Elevation_Range;

   function Average_Temperature
     (Sector : Root_Sector_Type'Class)
      return Non_Negative_Real;

   function Habitability
     (Sector : Root_Sector_Type'Class)
      return Unit_Real;

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
         Tile            : Hera.Surfaces.Surface_Tile_Index;
         Elevation       : Elevation_Range;
         Terrain         : Hera.Terrain.Terrain_Type;
         Deposits        : Sector_Deposit_Lists.List;
         Habitability    : Unit_Real;
         Ave_Temperature : Non_Negative_Real;
      end record;

   overriding function Log_Id
     (Sector : Root_Sector_Type)
      return String
   is (Sector.Tile'Image);

   function Elevation
     (Sector : Root_Sector_Type'Class)
      return Elevation_Range
   is (Sector.Elevation);

   function Average_Temperature
     (Sector : Root_Sector_Type'Class)
      return Non_Negative_Real
   is (Sector.Ave_Temperature);

   function Habitability
     (Sector : Root_Sector_Type'Class)
      return Unit_Real
   is (Sector.Habitability);

   function Terrain
     (Sector : Root_Sector_Type'Class)
      return Hera.Terrain.Terrain_Type
   is (Sector.Terrain);

end Hera.Sectors;
