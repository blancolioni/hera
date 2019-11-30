with Hera.Colonies;

package body Hera.Pops.People.Updates is

   ---------------------
   -- Buy_Commodities --
   ---------------------

   procedure Buy_Commodities (Pop : Population_Type) is
      use Hera.Money;

      Budget   : constant Money_Type := Pop.Cash;

      procedure Buy (Commodity : Hera.Commodities.Commodity_Type);

      ---------
      -- Buy --
      ---------

      procedure Buy (Commodity : Hera.Commodities.Commodity_Type) is
      begin
         Pop.Market.Buy
           (Trader    => Pop,
            Stock     => Pop,
            Commodity => Commodity,
            Quantity  => Pop.Size,
            Cash      => Adjust (Budget, Commodity.Happiness));
      end Buy;

   begin
      Pop.Class.Iterate_Required_Goods (Buy'Access);
   end Buy_Commodities;

   -------------------------
   -- Consume_Commodities --
   -------------------------

   procedure Consume_Commodities
     (Pop : Population_Type)
   is
      use Hera.Quantities;

      Happiness : Unit_Real := 0.0;
      Size : constant Quantity_Type := Pop.Size;

      procedure Consume (Commodity : Hera.Commodities.Commodity_Type);

      -------------
      -- Consume --
      -------------

      procedure Consume (Commodity : Hera.Commodities.Commodity_Type) is
         Quantity : constant Quantity_Type :=
           Min (Pop.Quantity (Commodity), Size);
         Relative : constant Unit_Real := To_Real (Quantity) / To_Real (Size);
         Value    : Hera.Money.Money_Type;
      begin
         Happiness := Happiness + Relative * Commodity.Happiness;
         Pop.Remove (Commodity, Quantity, Value);
         Pop.Colony.Planet.On_Consumption
           (Commodity, Quantity, Value);
      end Consume;

   begin
      Pop.Class.Iterate_Required_Goods (Consume'Access);
      Pop.Update_Happiness (Happiness);
   end Consume_Commodities;

end Hera.Pops.People.Updates;
