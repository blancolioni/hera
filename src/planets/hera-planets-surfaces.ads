private with Ada.Containers.Vectors;

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

   function Width (Surface : Root_Surface_Type'Class) return Natural;
   function Height (Surface : Root_Surface_Type'Class) return Natural;

   type Surface_Type is access constant Root_Surface_Type'Class;

   function Get_Surface
     (Planet : Root_Planet_Type'Class)
      return Surface_Type;

   function Get_Sectors
     (Planet : Root_Planet_Type'Class)
      return Hera.Sectors.Sector_Array;

private

   package Sector_Vectors is
     new Ada.Containers.Vectors
       (Positive, Hera.Sectors.Sector_Type, Hera.Sectors."=");

   type Root_Surface_Type is
     new Hera.Objects.Root_Hera_Object with
      record
         Width  : Natural;
         Height : Natural;
         Vector : Sector_Vectors.Vector;
      end record;

   function Width (Surface : Root_Surface_Type'Class) return Natural
   is (Surface.Width);

   function Height (Surface : Root_Surface_Type'Class) return Natural
   is (Surface.Height);

end Hera.Planets.Surfaces;
