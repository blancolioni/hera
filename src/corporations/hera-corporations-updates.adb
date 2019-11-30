with Hera.Money;
with Hera.Quantities;

with Hera.Commodities;
with Hera.Markets;
with Hera.Warehouses;

package body Hera.Corporations.Updates is

   function Minimum_Stock
     (Commodity : Hera.Commodities.Commodity_Type)
      return Hera.Quantities.Quantity_Type;

   -------------------
   -- Minimum_Stock --
   -------------------

   function Minimum_Stock
     (Commodity : Hera.Commodities.Commodity_Type)
      return Hera.Quantities.Quantity_Type
   is
      use all type Hera.Commodities.Commodity_Class;
   begin
      case Commodity.Class is
         when Resource =>
            return Hera.Quantities.To_Quantity (100.0);
         when Consumer_Good =>
            return Hera.Quantities.Zero;
         when Industrial_Good =>
            return Hera.Quantities.To_Quantity (100.0);
         when Building_Module =>
            return Hera.Quantities.Zero;
         when Starship_Component =>
            return Hera.Quantities.Zero;
         when Service_Token =>
            return Hera.Quantities.Zero;
         when Market_Token =>
            return Hera.Quantities.Zero;
      end case;
   end Minimum_Stock;

   ----------------------
   -- Sell_Commodities --
   ----------------------

   procedure Sell_Commodities
     (Corporation : Corporation_Type;
      Colony      : Hera.Colonies.Colony_Type)
   is
      Warehouse : constant Hera.Warehouses.Warehouse_Type :=
                    Colony.Warehouse (Corporation);

      procedure Sell_Stock
        (Commodity : Hera.Commodities.Commodity_Type;
         Stock     : Hera.Commodities.Stock_Entry);

      ----------------
      -- Sell_Stock --
      ----------------

      procedure Sell_Stock
        (Commodity : Hera.Commodities.Commodity_Type;
         Stock     : Hera.Commodities.Stock_Entry)
      is
         use Hera.Quantities;
         Available : constant Quantity_Type := Stock.Quantity;
         Minimum   : constant Quantity_Type :=
                       Minimum_Stock (Commodity);
      begin
         if Available > Minimum then
            Colony.Market.Sell
              (Trader    => Corporation,
               Stock     => Warehouse,
               Commodity => Commodity,
               Quantity  => Available - Minimum,
               Price     =>
                 Hera.Money.Price
                   (Stock.Value, Stock.Quantity));
         end if;
      end Sell_Stock;

   begin
      Warehouse.Scan_Stock (Sell_Stock'Access);
   end Sell_Commodities;

end Hera.Corporations.Updates;
