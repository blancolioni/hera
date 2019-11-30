with Ada.Containers.Vectors;
with WL.String_Maps;

package body Hera.Terrain is

   package Terrain_Maps is
     new WL.String_Maps (Terrain_Type);

   package Terrain_Vectors is
     new Ada.Containers.Vectors (Positive, Terrain_Type);

   Map : Terrain_Maps.Map;
   Vector : Terrain_Vectors.Vector;

   -----------------
   -- All_Terrain --
   -----------------

   function All_Terrain return Terrain_Array is
   begin
      return Arr : Terrain_Array (1 .. Vector.Last_Index) do
         for I in Arr'Range loop
            Arr (I) := Vector.Element (I);
         end loop;
      end return;
   end All_Terrain;

   ------------
   -- Exists --
   ------------

   function Exists (Tag : String) return Boolean is
   begin
      return Map.Contains (Tag);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Tag : String) return Terrain_Type is
   begin
      return Map.Element (Tag);
   end Get;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Terrain : in out Root_Terrain_Type;
      From      : Tropos.Configuration)
   is
   begin
      Hera.Objects.Root_Localised_Object (Terrain).Load (From);
   end Load;

   -------------------
   -- New_Terrain --
   -------------------

   procedure New_Terrain
     (Terrain : Terrain_Type)
   is
   begin
      Map.Insert (Terrain.Tag, Terrain);
      while Vector.Last_Index < Terrain.Priority loop
         Vector.Append (null);
      end loop;
      Vector.Replace_Element (Terrain.Priority, Terrain);
   end New_Terrain;

   ---------------------
   -- Resource_Chance --
   ---------------------

   function Resource_Chance
     (Terrain  : Root_Terrain_Type'Class;
      Resource : Hera.Commodities.Resource_Type)
      return Unit_Real
   is
      use type Hera.Commodities.Commodity_Type;
   begin
      for Res of Terrain.Resource_Chance loop
         if Res.Resource = Resource then
            return Res.Chance;
         end if;
      end loop;
      return 0.0;
   end Resource_Chance;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Terrain : Root_Terrain_Type;
      To        : in out Tropos.Configuration)
   is
   begin
      Hera.Objects.Root_Localised_Object (Terrain).Save (To);
   end Save;

end Hera.Terrain;
