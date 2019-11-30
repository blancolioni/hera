with Hera.Calendar;

package Hera.Orbits is

   function Period
     (Large_Mass : Non_Negative_Real;
      Orbit      : Non_Negative_Real)
      return Non_Negative_Real;

   procedure Calculate_Position
     (Large_Mass : Non_Negative_Real;
      Orbit      : Non_Negative_Real;
      Elapsed    : Duration;
      Latitude   : out Real;
      Longitude  : out Real);

   function Calculate_Longitude
     (Large_Mass : Non_Negative_Real;
      Orbit      : Non_Negative_Real;
      Elapsed    : Duration)
      return Real;

   function Calculate_Current_Longitude
     (Large_Mass : Non_Negative_Real;
      Orbit      : Non_Negative_Real;
      Epoch      : Hera.Calendar.Time)
      return Real;

end Hera.Orbits;
