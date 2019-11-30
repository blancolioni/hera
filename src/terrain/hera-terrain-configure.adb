package body Hera.Terrain.Configure is

   Terrain_Version : constant Hera.Objects.Object_Version := "0.0.1";

   --------------------
   -- Create_Terrain --
   --------------------

   procedure Create_Terrain
     (Config : Tropos.Configuration)
   is
      Terrain : Root_Terrain_Type :=
                  Root_Terrain_Type'
                    (Hera.Objects.Root_Localised_Object with
                     Priority        => Config.Get ("priority"),
                     Hazard_Level    => Config.Get ("hazard"),
                     Resource_Chance => <>);

   begin
      for Resource_Chance of Config.Child ("resource") loop
         Terrain.Resource_Chance.Append
           (Resource_Chance_Record'
              (Resource =>
                   Hera.Commodities.Get
                 (Resource_Chance.Config_Name),
               Chance   => Resource_Chance.Value));
      end loop;

      New_Terrain
        (Terrain_Type
           (Terrain.New_Localised_Object
                (Terrain_Version, Config.Config_Name)));
   end Create_Terrain;

end Hera.Terrain.Configure;
