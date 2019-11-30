with Hera.Stars;

package Hera.Planets.Configure is

   procedure Create_Planet
     (Star  : Hera.Stars.Star_Type;
      Index : Positive;
      Zone  : Planetary_Zone;
      Orbit : Non_Negative_Real);

end Hera.Planets.Configure;
