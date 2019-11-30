package Hera.Scenarios.Converter is

   procedure Convert_Items_To_Commodities
     (Items_Path       : String;
      Commodities_Path : String);

   procedure Convert_Facilities
     (Properties_Path : String;
      Config_Path     : String);

   procedure Convert_Pop_Classes
     (Properties_Path : String;
      Config_Path     : String);

   procedure Convert_Terrain
     (Properties_Path : String;
      Config_Path     : String);

end Hera.Scenarios.Converter;
