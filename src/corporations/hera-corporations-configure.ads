with Hera.Money;

package Hera.Corporations.Configure is

   function Create
     (Planet     : Hera.Planets.Planet_Type;
      Start_Cash : Hera.Money.Money_Type)
      return Corporation_Type;

end Hera.Corporations.Configure;
