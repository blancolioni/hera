with WL.Random;
with Hera.Random;

with Hera.Elementary_Functions;

package body Hera.Sectors.Configure is

   Sector_Version : constant Hera.Objects.Object_Version := "0.0.1";

   ----------------
   -- New_Sector --
   ----------------

   function New_Sector
     (Tile            : Hera.Surfaces.Surface_Tile_Index;
      Elevation       : Elevation_Range;
      Terrain         : Hera.Terrain.Terrain_Type;
      Ave_Temperature : Non_Negative_Real)
      return Sector_Type
   is
      Ideal_Tmp    : constant := 285.0;
      Std_Dev_Tmp  : constant := 25.0;
      Tmp_Factor   : constant Unit_Real :=
                       Unit_Clamp
                         (Hera.Elementary_Functions.Exp
                            (-(Ave_Temperature - Ideal_Tmp) ** 2
                             / (2.0 * Std_Dev_Tmp ** 2)));
      Habitability : constant Unit_Real := Tmp_Factor;
      Sector : Root_Sector_Type :=
                 Root_Sector_Type'
                   (Hera.Objects.Root_Hera_Object with
                    Tile            => Tile,
                    Elevation       => Elevation,
                    Terrain         => Terrain,
                    Ave_Temperature => Ave_Temperature,
                    Habitability    => Habitability,
                    Deposits        => <>);

   begin
      for Resource of
        Hera.Commodities.Resources
      loop
         if Hera.Random.Unit_Random < Terrain.Resource_Chance (Resource) then
            Sector.Deposits.Append
              (Sector_Deposit'
                 (Resource => Resource,
                  Quantity =>
                    Hera.Quantities.To_Quantity
                      (Real (WL.Random.Random_Number (1, 1E6))),
                  Yield    =>
                    Hera.Quantities.To_Quantity
                      (Real (WL.Random.Random_Number (1, 20)))));
         end if;
      end loop;

      return Sector_Type (Sector.New_Object (Sector_Version));

   end New_Sector;

end Hera.Sectors.Configure;
