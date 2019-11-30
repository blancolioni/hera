package Hera.Sectors.Configure is

   function New_Sector
     (X, Y    : Positive;
      Terrain : Hera.Terrain.Terrain_Type)
      return Sector_Type;

end Hera.Sectors.Configure;
