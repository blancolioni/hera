with Ada.Containers.Vectors;

with WL.Heaps;

with Hera.Planets.Surfaces;

package body Hera.Planets is

   package Planet_Heaps is
     new WL.Heaps (Non_Negative_Real, Planet_Type, "<");

   package Planet_Vectors is
     new Ada.Containers.Vectors (Positive, Planet_Type);

   Vector : Planet_Vectors.Vector;

   ----------
   -- Find --
   ----------

   function Find
     (Planet : Root_Planet_Type'Class;
      Score  : not null access
        function (Sector : Hera.Sectors.Sector_Type)
      return Non_Negative_Real)
      return Hera.Sectors.Sector_Array
   is
      Surface : constant Hera.Planets.Surfaces.Surface_Type :=
                  Surfaces.Get_Surface (Planet);
   begin
      return Surface.Find (Score);
   end Find;

   ----------
   -- Find --
   ----------

   function Find
     (Score : not null access
        function (Planet : Planet_Type)
      return Non_Negative_Real)
      return Planet_Array
   is
      Queue : Planet_Heaps.Heap;
   begin
      for Planet of Vector loop
         declare
            This_Score : constant Non_Negative_Real :=
              Score (Planet);
         begin
            if This_Score > 0.0 then
               Queue.Insert (This_Score, Planet);
            end if;
         end;
      end loop;

      return Result : Planet_Array (1 .. Queue.Length) do
         for Handle of Result loop
            Handle := Queue.First_Element;
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
   begin
      return Surfaces.Get_Sectors (Planet);
   end Get_Sectors;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Process : not null access
        procedure (Planet : Planet_Type))
   is
   begin
      for Planet of Vector loop
         Process (Planet);
      end loop;
   end Iterate;

   ---------------------
   -- Iterate_Economy --
   ---------------------

   procedure Iterate_Economy
     (Planet  : Root_Planet_Type'Class;
      Process : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Production : Hera.Commodities.Stock_Entry;
                   Consumption : Hera.Commodities.Stock_Entry))
   is
   begin
      Hera.Commodities.Scan_Stock_Join
        (Left        => Planet.Production.all,
         Right       => Planet.Consumption.all,
         Process     => Process);
   end Iterate_Economy;

   ----------------
   -- New_Planet --
   ----------------

   procedure New_Planet (Planet : Root_Planet_Type'Class) is
      New_Planet : constant Planet_Type :=
                          Planet_Type (Planet.Save_Object);
   begin
      Vector.Append (New_Planet);
      New_Planet.Primary.Add_Orbiting_Entity (New_Planet);
   end New_Planet;

   ------------------
   -- On_Colonized --
   ------------------

   overriding procedure On_Colonized (Planet : in out Root_Planet_Type) is
   begin
      Planet.Production :=
        Hera.Markets.New_Trade_Node ("Production - " & Planet.Name);
      Planet.Consumption :=
        Hera.Markets.New_Trade_Node ("Consumption - " & Planet.Name);
      Hera.Markets.New_Trade_Center
        (Planet.Name, Planet.Production, Planet.Consumption);
   end On_Colonized;

   --------------------
   -- On_Consumption --
   --------------------

   procedure On_Consumption
     (Planet    : Root_Planet_Type'Class;
      Commodity : Hera.Commodities.Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type;
      Cost      : Hera.Money.Money_Type)
   is
   begin
      Planet.Consumption.Add (Commodity, Quantity, Cost);
   end On_Consumption;

   -------------------
   -- On_Production --
   -------------------

   procedure On_Production
     (Planet    : Root_Planet_Type'Class;
      Commodity : Hera.Commodities.Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type;
      Cost      : Hera.Money.Money_Type)
   is
   begin
      Planet.Production.Add (Commodity, Quantity, Cost);
   end On_Production;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Object : Root_Planet_Type;
      Config : in out Tropos.Configuration)
   is
   begin
      Hera.Star_Systems.Root_Star_System_Entity (Object).Save (Config);
      Config.Add ("class", "planet");
      Config.Add ("composition", Object.Composition'Image);
      Config.Add ("climate", Object.Climate.Tag);
      Config.Add ("habitability", Object.Habitability);
      Config.Add ("ave-temp", Object.Average_Temperature);
      Config.Add ("hydrosphere", Object.Hydrosphere);
      Config.Add ("life", Object.Life_Complexity'Image);
      Config.Add ("smoothess", Object.Smoothness);
      Config.Add ("elevation-range", Object.Elevation_Range);
      Config.Add ("sea-level", Object.Sea_Level);
   end Save;

   --------------------
   -- Surface_Height --
   --------------------

   function Surface_Height
     (Planet : Root_Planet_Type'Class)
      return Natural
   is
   begin
      return Surfaces.Get_Surface (Planet).Height;
   end Surface_Height;

   -------------------
   -- Surface_Width --
   -------------------

   function Surface_Width
     (Planet : Root_Planet_Type'Class)
      return Natural
   is
   begin
      return Surfaces.Get_Surface (Planet).Width;
   end Surface_Width;

end Hera.Planets;
