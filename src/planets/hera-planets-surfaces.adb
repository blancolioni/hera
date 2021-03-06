with WL.Heaps;
with WL.String_Maps;

with Hera.Color.Palettes;
with Hera.Solar_System;

with Hera.Colonies;
with Hera.Planets.Surfaces.Generate;

with Hera.Paths;

package body Hera.Planets.Surfaces is

   Surface_Version : constant Hera.Objects.Object_Version := "0.0.1";

   package Surface_Maps is
     new WL.String_Maps (Surface_Type);

   Map : Surface_Maps.Map;

   procedure Create_Surface (Planet : Root_Planet_Type'Class);

   Have_Palettes : Boolean := False;

   function Get_Palette
     (Map : Surface_Map_Type)
      return Hera.Color.Palettes.Palette_Type;

   --------------------
   -- Create_Surface --
   --------------------

   procedure Create_Surface (Planet : Root_Planet_Type'Class) is
      Radius : constant Non_Negative_Real :=
                 Planet.Radius / Hera.Solar_System.Earth_Radius;
      Tile_Count : constant Natural :=
                     (case Planet.Composition is
                         when Ice | Rock | Rock_Ice | Rock_Iron =>
                           Natural (Radius * 400.0),
                         when Hydrogen | Gaseous                =>
                           0);

   begin

      if Tile_Count > 0 then
         declare
            Tiles   : Hera.Surfaces.Root_Surface_Type;
            Surface : Root_Surface_Type;
         begin
            Planet.Log
              ("creating surface with "
               & Tile_Count'Image & " tiles");
            Tiles.Create_Voronoi_Partition (Tile_Count);
            Hera.Planets.Surfaces.Generate.Generate_Surface
              (Planet  => Planet,
               Tiles   => Tiles,
               Surface => Surface);

            Map.Insert
              (String (Planet.Identifier),
               Surface_Type
                 (Surface.New_Object (Surface_Version)));

            Planet.Log ("done");
         end;
      end if;
   end Create_Surface;

   ----------
   -- Find --
   ----------

   function Find
     (Surface : Root_Surface_Type'Class;
      Score   : not null access
        function (Sector : Hera.Sectors.Sector_Type)
      return Non_Negative_Real)
      return Hera.Sectors.Sector_Array
   is
      package Sector_Heaps is
        new WL.Heaps (Non_Negative_Real, Hera.Sectors.Sector_Type, "<",
                      Hera.Sectors."=");

      Queue : Sector_Heaps.Heap;

   begin
      for Sector of Surface.Sectors loop
         declare
            This_Score : constant Non_Negative_Real := Score (Sector);
         begin
            if This_Score > 0.0 then
               Queue.Insert (This_Score, Sector);
            end if;
         end;
      end loop;

      return Result : Hera.Sectors.Sector_Array (1 .. Queue.Length) do
         for I in Result'Range loop
            Result (I) := Queue.First_Element;
            Queue.Delete_First;
         end loop;
      end return;

   end Find;

   -----------------
   -- Get_Palette --
   -----------------

   function Get_Palette
     (Map : Surface_Map_Type)
      return Hera.Color.Palettes.Palette_Type
   is
   begin
      if not Have_Palettes then
         Hera.Color.Palettes.Load_Palette
           (Surface_Map_Type'Image (Temperature_Map),
            260.0, 300.0,
            Hera.Paths.Config_File ("planets/world-temperature.bmp"));
         Hera.Color.Palettes.Load_Palette
           (Surface_Map_Type'Image (Elevation_Map),
            0.0, 2500.0,
            Hera.Paths.Config_File ("planets/world-temperature.bmp"));
         Hera.Color.Palettes.Load_Palette
           (Surface_Map_Type'Image (Habitability_Map),
            0.0, 1.0,
            Hera.Paths.Config_File ("planets/world-temperature.bmp"));
         Have_Palettes := True;
      end if;

      return Hera.Color.Palettes.Get_Palette (Map'Image);
   end Get_Palette;

   -----------------
   -- Get_Surface --
   -----------------

   function Get_Surface
     (Planet : Root_Planet_Type'Class)
      return Surface_Type
   is
   begin
      if not Map.Contains (String (Planet.Identifier)) then
         Create_Surface (Planet);
      end if;

      return Map.Element (String (Planet.Identifier));
   end Get_Surface;

   -------------------
   -- Iterate_Tiles --
   -------------------

   procedure Iterate_Tiles
     (Surface : Root_Surface_Type'Class;
      Process : not null access
        procedure (Tile : Hera.Surfaces.Surface_Tile_Index))
   is
   begin
      for I in 1 .. Surface.Tiles.Tile_Count loop
         Process (I);
      end loop;
   end Iterate_Tiles;

   ---------------
   -- Serialize --
   ---------------

   function Serialize
     (Surface : Root_Surface_Type'Class;
      Tile    : Hera.Surfaces.Surface_Tile_Index;
      Map     : Surface_Map_Type)
      return Hera.Json.Json_Object'Class
   is
      Sector : constant Hera.Sectors.Sector_Type :=
                 Surface.Sectors.Element (Tile);
      Palette : constant Hera.Color.Palettes.Palette_Type :=
                  Get_Palette (Map);

      Result : Hera.Json.Json_Object;

      function To_Json
        (P : Hera.Surfaces.Vector_3)
         return Json.Json_Value'Class;

      -------------
      -- To_Json --
      -------------

      function To_Json
        (P : Hera.Surfaces.Vector_3)
         return Json.Json_Value'Class
      is
         Object : Json.Json_Object;
      begin
         Object.Set_Property ("x", Float (P (1)));
         Object.Set_Property ("y", Float (P (2)));
         Object.Set_Property ("z", Float (P (3)));
         return Object;
      end To_Json;

   begin
      Result.Set_Property ("index", Natural (Tile));
      Result.Set_Property ("normal",
                           To_Json (Surface.Tiles.Tile_Centre (Tile)));

      declare
         Arr : Json.Json_Array;
      begin
         for P of Surface.Tiles.Tile_Boundary (Tile) loop
            Arr.Append (To_Json (P));
         end loop;
         Result.Set_Property ("boundary", Arr);
      end;

      declare
         use Hera.Colonies;
         Colony : constant Colony_Type := Get_By_Sector (Sector);
         Parameter : constant Real :=
                       (case Map is
                           when Elevation_Map    =>
                             Sector.Elevation,
                           when Temperature_Map  =>
                             Sector.Average_Temperature,
                           when Habitability_Map =>
                             Sector.Habitability);
      begin
         if Colony /= null then
            Result.Set_Property ("colony", String (Colony.Identifier));
         end if;

         Result.Set_Property
           ("color",
            Hera.Color.To_Html_String
              (Hera.Color.Palettes.Get_Color
                   (Palette, Parameter)));
      end;

      return Result;
   end Serialize;

end Hera.Planets.Surfaces;
