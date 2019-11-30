with Tropos;

package Hera.Commodities.Configure is

   procedure Create_Commodity
     (Config : Tropos.Configuration);

   procedure Create_Components
     (Config : Tropos.Configuration);

   procedure Create_Prices;

   function New_Service_Commodity
     (Tag       : String;
      Charge    : Hera.Money.Price_Type;
      Quality   : Quality_Type;
      Happiness : Unit_Real;
      Class     : Service_Class)
      return Commodity_Type;

end Hera.Commodities.Configure;
