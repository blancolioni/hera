with WL.Random;
with Hera.Random;

package body Hera.Sectors.Configure is

   Sector_Version : constant Hera.Objects.Object_Version := "0.0.1";

   ----------------
   -- New_Sector --
   ----------------

   function New_Sector
     (X, Y    : Positive;
      Terrain : Hera.Terrain.Terrain_Type)
      return Sector_Type
   is
      Sector : Root_Sector_Type :=
                 Root_Sector_Type'
                   (Hera.Objects.Root_Hera_Object with
                    X        => X,
                    Y        => Y,
                    Terrain  => Terrain,
                    Deposits => <>);

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
