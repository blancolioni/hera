with Tropos;

package Hera.Colonies.Configure is

   procedure Configure_Template
     (Config : Tropos.Configuration);

   procedure Create
     (Planet        : Hera.Planets.Planet_Type;
      Sector        : Hera.Sectors.Sector_Type;
      Corporations  : Hera.Corporations.Corporation_Array;
      Template_Name : String);

end Hera.Colonies.Configure;
