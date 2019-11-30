with Hera.Quantities;

with Hera.Commodities;

package Hera.Planets.Updates is

   function Available_Quantity
     (Planet    : Hera.Handles.Planet.Planet_Class;
      Commodity : Hera.Commodities.Commodity_Class)
      return Hera.Quantities.Quantity_Type;

end Hera.Planets.Updates;
