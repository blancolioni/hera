with Ada.Containers.Doubly_Linked_Lists;

with WL.Heaps;
with WL.Random;
with WL.String_Maps;

with Hera.Random;
with Hera.Solar_System;

with Hera.Sectors.Configure;
with Hera.Terrain;

package body Hera.Planets.Surfaces is

   Surface_Version : constant Hera.Objects.Object_Version := "0.0.1";

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
      return Hera.Terrain.Terrain_Type;

   package Surface_Maps is
     new WL.String_Maps (Surface_Type);

   Map : Surface_Maps.Map;

   procedure Create_Surface (Planet : Root_Planet_Type'Class);

   --------------------
   -- Create_Surface --
   --------------------

   procedure Create_Surface (Planet : Root_Planet_Type'Class) is
      Height : constant Natural :=
        Natural
          (10.0 * Planet.Radius / Hera.Solar_System.Earth_Radius);
      Width  : constant Natural := Height * 2;
      Terrain : Terrain_Layout_Array (1 .. Width, 1 .. Height) :=
                  (others => (others => null));
      Chances : Terrain_Chance_Lists.List;
      Default_Terrain : constant Hera.Terrain.Terrain_Type :=
                          Planet.Climate.Default_Terrain;
      Surface         : Root_Surface_Type := Root_Surface_Type'
        (Hera.Objects.Root_Hera_Object with
         Width  => Width,
         Height => Height,
         Vector => <>);
   begin

      for Terrain of Hera.Terrain.All_Terrain loop
         declare
            Chance : constant Unit_Real :=
                       Planet.Climate.Terrain_Chance (Terrain);
         begin
            Chances.Append
              (Terrain_Chance_Record'
                 (Terrain => Terrain,
                  Chance  => Chance));
         end;
      end loop;

      for Y in Terrain'Range (2) loop
         for X in Terrain'Range (1) loop
            Terrain (X, Y) :=
              Random_Terrain (Terrain, X, Y, Chances, Default_Terrain);
         end loop;
      end loop;

      for Y in Terrain'Range (2) loop
         for X in Terrain'Range (1) loop
            Surface.Vector.Append
              (Hera.Sectors.Configure.New_Sector
                 (X => X,
                  Y => Y,
                  Terrain => Terrain (X, Y)));
         end loop;
      end loop;

      Map.Insert (String (Planet.Identifier),
                  Surface_Type (Surface.New_Object (Surface_Version)));

--                 begin
--                 for Terrain_Resource of
--               Hera.Db.Terrain_Resource.Select_By_Terrain (Terrain (X, Y))
--                 loop
--                    if Hera.Random.Unit_Random < Terrain_Resource.Chance then
--                       declare
--                          use Hera.Quantities;
--                          Yield : constant Quantity_Type :=
--                            To_Quantity
--                              (Real (WL.Random.Random_Number (1, 20)));
--                          Total : constant Quantity_Type :=
--                            To_Quantity
--                              (Real (WL.Random.Random_Number (1, 1E6)));
--                       begin
--                          Hera.Db.Sector_Deposit.Create
--                            (Planet_Sector => Sector,
--                             Resource      => Terrain_Resource.Resource,
--                             Yield         => Yield,
--                             Total         => Total);
--                       end;
--                    end if;
--                 end loop;
--              end;
--           end loop;
--        end loop;

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
      for Sector of Surface.Vector loop
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
   -- Get_Sectors --
   -----------------

   function Get_Sectors
     (Planet : Root_Planet_Type'Class)
      return Hera.Sectors.Sector_Array
   is
      Surface : constant Surface_Type := Get_Surface (Planet);
   begin
      return Sectors : Hera.Sectors.Sector_Array
        (1 .. Surface.Vector.Last_Index)
      do
         for I in Sectors'Range loop
            Sectors (I) := Surface.Vector.Element (I);
         end loop;
      end return;
   end Get_Sectors;

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
               This_Chance : Non_Negative_Real := Chance_Record.Chance;
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
                           N : constant Terrain_Type :=
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

end Hera.Planets.Surfaces;
