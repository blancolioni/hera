package body Hera.Climate.Configure is

   --------------------
   -- Create_Climate --
   --------------------

   procedure Create_Climate
     (Config : Tropos.Configuration)
   is
      Hazard_Level : constant Unit_Real := Config.Get ("hazard", 0.0);
      Default_Terrain : constant Hera.Terrain.Terrain_Type :=
                          (if Config.Contains ("default-terrain")
                           then Hera.Terrain.Get
                             (Config.Get ("default-terrain"))
                           else null);
      Terrain_Chance  : Terrain_Chance_Lists.List;
   begin
      for Terrain_Config of Config.Child ("terrain") loop
         Terrain_Chance.Append
           (Terrain_Chance_Record'
              (Terrain =>
                   Hera.Terrain.Get
                 (Terrain_Config.Config_Name),
               Chance   => Terrain_Config.Value));
      end loop;

      New_Climate
        (Tag             => Config.Config_Name,
         Hazard_Level    => Hazard_Level,
         Default_Terrain => Default_Terrain,
         Terrain_Chance  => Terrain_Chance);
   end Create_Climate;

end Hera.Climate.Configure;
