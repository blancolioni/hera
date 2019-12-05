with Ada.Containers.Doubly_Linked_Lists;

with WL.Noise;
with WL.Random.Height_Maps;

with Hera.Random;
with Hera.Surfaces;
with Hera.Terrain;

with Hera.Sectors.Configure;

package body Hera.Planets.Surfaces.Generate is

   Use_Noise : constant Boolean := True;

   type Terrain_Chance_Record is
      record
         Terrain : Hera.Terrain.Terrain_Type;
         Chance  : Unit_Real;
      end record;

   package Terrain_Chance_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Terrain_Chance_Record);

   type Terrain_Layout_Array is
     array (Positive range <>, Positive range <>)
     of Hera.Terrain.Terrain_Type;

   function Random_Terrain
     (Current : Terrain_Layout_Array;
      X, Y    : Positive;
      Chance  : Terrain_Chance_Lists.List;
      Default : Hera.Terrain.Terrain_Type)
      return Hera.Terrain.Terrain_Type
     with Unreferenced;

   package Heights renames WL.Random.Height_Maps;

   ----------------------
   -- Generate_Surface --
   ----------------------

   procedure Generate_Surface
     (Planet  : Root_Planet_Type'Class;
      Tiles   : Hera.Surfaces.Surface_Type;
      Surface : in out Root_Surface_Type'Class)
   is
      Freqs : constant Heights.Frequency_Map (1 .. Planet.Elevation_Range) :=
                (others => 1);
      Hs    : Heights.Height_Array (1 .. Natural (Tiles.Tile_Count));

      function Base_Temperature
        (Tile : Hera.Surfaces.Surface_Tile_Index)
         return Non_Negative_Real
        with Unreferenced;

      function Get_Neighbours
        (Index : Positive)
         return Heights.Neighbour_Array;

      ----------------------
      -- Base_Temperature --
      ----------------------

      function Base_Temperature
        (Tile : Hera.Surfaces.Surface_Tile_Index)
         return Non_Negative_Real
      is
         Y : constant Real := Tiles.Tile_Centre (Tile) (3);
      begin
         return Planet.Average_Temperature
           + (0.5 - abs Y) * 10.0;
      end Base_Temperature;

      --------------------
      -- Get_Neighbours --
      --------------------

      function Get_Neighbours
        (Index : Positive)
         return Heights.Neighbour_Array
      is
         use Hera.Surfaces;
         Tile : constant Surface_Tile_Index :=
                  Surface_Tile_Index (Index);
      begin
         return Ns : Heights.Neighbour_Array
           (1 .. Natural (Tiles.Neighbour_Count (Tile)))
         do
            for I in Ns'Range loop
               Ns (I) :=
                 Positive
                   (Tiles.Neighbour
                      (Tile,
                       Tile_Neighbour_Index (I)));
            end loop;
         end return;
      end Get_Neighbours;

   begin

      Surface.Tiles := new Hera.Surfaces.Root_Surface_Type'Class'(Tiles);

      if Use_Noise then
         declare
            Noise : WL.Noise.Perlin_Noise (3);
         begin
            Noise.Reset (WL.Random.Random_Number (1, 1_000_000));
            for I in 1 .. Surface.Tiles.Tile_Count loop
               declare
                  Center : constant Hera.Surfaces.Vector_3 :=
                             Surface.Tiles.Tile_Centre (I);
                  Coord  : constant WL.Noise.Noise_Vector :=
                             (1 => Float (Center (1)),
                              2 => Float (Center (2)),
                              3 => Float (Center (3)));
                  Value  : constant WL.Noise.Signed_Unit_Real :=
                             Noise.Get (Coord);
                  Elevation : constant Positive :=
                                Natural
                                  (Real'Floor
                                     ((Real (Value) + 1.0) / 2.0
                                      * Real (Planet.Elevation_Range)))
                                + 1;
               begin
                  Hs (Positive (I)) := Elevation;
               end;
            end loop;
         end;
      else
         Heights.Generate_Height_Map
           (Heights     => Hs,
            Frequencies => Freqs,
            Smoothing   => 3,
            Neighbours  => Get_Neighbours'Access);
      end if;

      for I in 1 .. Tiles.Tile_Count loop
         declare
            Terrain : constant Hera.Terrain.Terrain_Type :=
                        (if Hs (Positive (I)) <= Planet.Sea_Level
                         then Hera.Terrain.Get ("water")
                         else Hera.Terrain.Get ("plain"));
            Elevation : constant Real :=
                          Real (Hs (Positive (I)) - Planet.Sea_Level)
                          * 100.0;

            Sector  : constant Hera.Sectors.Sector_Type :=
                        Hera.Sectors.Configure.New_Sector
                          (Tile      => I,
                           Elevation => Elevation,
                           Terrain   => Terrain);
         begin
            Surface.Sectors.Append (Sector);
         end;
      end loop;

   end Generate_Surface;

   --------------------
   -- Random_Terrain --
   --------------------

   function Random_Terrain
     (Current : Terrain_Layout_Array;
      X, Y    : Positive;
      Chance  : Terrain_Chance_Lists.List;
      Default : Hera.Terrain.Terrain_Type)
      return Hera.Terrain.Terrain_Type
   is
   begin
      for Chance_Record of Chance loop
         if Chance_Record.Chance > 0.0 then
            declare
               This_Chance     : Non_Negative_Real := Chance_Record.Chance;
               Neighbour_Count : Natural := 0;
            begin
               for DX in -1 .. 1 loop
                  for DY in -1 .. 1 loop
                     if Y + DY in Current'Range (2) then
                        declare
                           use Hera.Terrain;
                           New_Y : constant Positive := Y + DY;
                           New_X : constant Positive :=
                                     (X + DX - 1) mod Current'Length (1) + 1;
                           N     : constant Terrain_Type :=
                                     Current (New_X, New_Y);
                        begin
                           if N = Chance_Record.Terrain then
                              Neighbour_Count := Neighbour_Count + 1;
                           end if;
                        end;
                     end if;
                  end loop;
               end loop;

               if Neighbour_Count > 0 then
                  This_Chance := This_Chance
                    * Real (WL.Random.Random_Number (1, Neighbour_Count));
               end if;

               if Hera.Random.Unit_Random < This_Chance then
                  return Chance_Record.Terrain;
               end if;
            end;
         end if;
      end loop;
      return Default;
   end Random_Terrain;

end Hera.Planets.Surfaces.Generate;
