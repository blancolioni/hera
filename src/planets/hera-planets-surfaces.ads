private with Ada.Containers.Vectors;
private with Hera.Surfaces;

with Hera.Objects;

with Hera.Sectors;

private package Hera.Planets.Surfaces is

   type Root_Surface_Type is
     new Hera.Objects.Root_Hera_Object with private;

   function Find
     (Surface : Root_Surface_Type'Class;
      Score   : not null access
        function (Sector : Hera.Sectors.Sector_Type)
      return Non_Negative_Real)
      return Hera.Sectors.Sector_Array;

   procedure Iterate_Tiles
     (Surface : Root_Surface_Type'Class;
      Process : not null access
        procedure (Tile : Hera.Surfaces.Surface_Tile_Index));

   function Serialize
     (Surface : Root_Surface_Type'Class;
      Tile    : Hera.Surfaces.Surface_Tile_Index)
      return Hera.Json.Json_Object'Class;

   type Surface_Type is access constant Root_Surface_Type'Class;

   function Get_Surface
     (Planet : Root_Planet_Type'Class)
      return Surface_Type;

private

   package Sector_Vectors is
     new Ada.Containers.Vectors
       (Hera.Surfaces.Surface_Tile_Index,
        Hera.Sectors.Sector_Type, Hera.Sectors."=");

   type Surface_Access is access constant Hera.Surfaces.Surface_Type;

   type Root_Surface_Type is
     new Hera.Objects.Root_Hera_Object with
      record
         Tiles   : Surface_Access;
         Sectors : Sector_Vectors.Vector;
      end record;

end Hera.Planets.Surfaces;
