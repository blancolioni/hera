package Hera.Sectors.Configure is

   function New_Sector
     (Tile            : Hera.Surfaces.Surface_Tile_Index;
      Elevation       : Elevation_Range;
      Terrain         : Hera.Terrain.Terrain_Type;
      Ave_Temperature : Non_Negative_Real)
      return Sector_Type;

end Hera.Sectors.Configure;
