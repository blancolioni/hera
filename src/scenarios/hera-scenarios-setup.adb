with Hera.Quantities;

with Hera.Colonies.Configure;
with Hera.Sectors;

package body Hera.Scenarios.Setup is

   --------------------------
   -- Initial_Planet_Setup --
   --------------------------

   procedure Initial_Planet_Setup
     (Planet           : Hera.Planets.Planet_Type;
      Corporations     : Hera.Corporations.Corporation_Array;
      Colony_Templates : Hera.String_Vectors.Vector)
   is
      function Score_Sector
        (Sector : Hera.Sectors.Sector_Type)
         return Non_Negative_Real;

      ------------------
      -- Score_Sector --
      ------------------

      function Score_Sector
        (Sector : Hera.Sectors.Sector_Type)
         return Non_Negative_Real
      is
         Score : Non_Negative_Real := 0.0;

      begin
         for Resource of Sector.Resources loop
            Score := Score
              + Hera.Quantities.To_Real (Sector.Yield (Resource));
         end loop;
         return Score;
      end Score_Sector;

      Sectors : constant Hera.Sectors.Sector_Array :=
                  Planet.Find (Score_Sector'Access);

      Colony_Count  : Natural := 0;

   begin

      for Sector of Sectors loop
         exit when Colony_Count = Colony_Templates.Last_Index;

         Colony_Count := Colony_Count + 1;
         Hera.Colonies.Configure.Create
           (Planet        => Planet,
            Sector        => Sector,
            Corporations  => Corporations,
            Template_Name => Colony_Templates.Element (Colony_Count));
      end loop;
   end Initial_Planet_Setup;

end Hera.Scenarios.Setup;
