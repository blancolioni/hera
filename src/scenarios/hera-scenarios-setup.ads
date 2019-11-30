with Hera.Corporations;
with Hera.Planets;

with Hera.String_Vectors;

package Hera.Scenarios.Setup is

   procedure Initial_Planet_Setup
     (Planet           : Hera.Planets.Planet_Type;
      Corporations     : Hera.Corporations.Corporation_Array;
      Colony_Templates : Hera.String_Vectors.Vector);

end Hera.Scenarios.Setup;
