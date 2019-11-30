with Hera.Color;
with Hera.Star_Systems;

package Hera.Stars is

   type Root_Star_Type is
     new Hera.Star_Systems.Root_Star_System_Entity with private;

   function Age (Star : Root_Star_Type'Class) return Non_Negative_Real;
   --  age in years

   function Luminosity (Star : Root_Star_Type'Class) return Non_Negative_Real;
   --  luminosity relative to the Sun

   function Temperature (Star : Root_Star_Type'Class) return Non_Negative_Real;
   --  surface temperature in Kelvin

   function Color (Star : Root_Star_Type'Class) return Hera.Color.Hera_Color;

   type Star_Type is access constant Root_Star_Type'Class;

private

   type Root_Star_Type is
     new Hera.Star_Systems.Root_Star_System_Entity with
      record
         Age         : Non_Negative_Real;
         Luminosity  : Non_Negative_Real;
         Temperature : Non_Negative_Real;
         Ecosphere   : Non_Negative_Real;
         Color       : Hera.Color.Hera_Color;
      end record;

   function Save_Star
     (Star : Root_Star_Type'Class)
     return Star_Type;

   function Age (Star : Root_Star_Type'Class) return Non_Negative_Real
   is (Star.Age);

   function Luminosity (Star : Root_Star_Type'Class) return Non_Negative_Real
   is (Star.Luminosity);

   function Temperature (Star : Root_Star_Type'Class) return Non_Negative_Real
   is (Star.Temperature);

   function Color (Star : Root_Star_Type'Class) return Hera.Color.Hera_Color
   is (Star.Color);

end Hera.Stars;
