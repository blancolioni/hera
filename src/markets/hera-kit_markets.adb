with Hera.Calendar;
with Hera.Identifiers;
with Hera.Profiling;

with Hera.Accounts;
with Hera.Commodities;

with Hera.Handles.Commodity;

with Hera.Db.Commodity;
with Hera.Db.Commodity_Transaction;
with Hera.Db.Market;
with Hera.Db.Market_Offer;

package body Hera.Markets is

   Market_Version : constant := 1;

   procedure Execute_Offer
     (Buyer     : Hera.Handles.Trader.Trader_Class;
      Seller    : Hera.Handles.Trader.Trader_Class;
      Commodity : Hera.Db.Commodity_Reference;
      Quantity  : Hera.Quantities.Quantity_Type;
      Price     : Hera.Money.Price_Type);

   ---------
   -- Buy --
   ---------

   function Buy
     (From_Market : Market_Class;
      Trader      : Hera.Handles.Trader.Trader_Class;
      Commodity   : Hera.Db.Commodity_Reference;
      Quantity    : Hera.Quantities.Quantity_Type;
      Cash        : Hera.Money.Money_Type)
      return Transaction_Result
   is
      use Hera.Money, Hera.Quantities;
      Remaining_Cash     : Money_Type := Cash;
      Remaining_Quantity : Quantity_Type := Quantity;
   begin
      Hera.Profiling.Start ("market-buy");
      for Market_Offer of
        Hera.Db.Market_Offer.Select_Market_Offer_Bounded_By_Priority
          (Market          => From_Market.Reference_Market,
           Commodity       => Commodity,
           Start_Priority  => 0.0,
           Finish_Priority => Real'Last)
      loop
         if Market_Offer.Quantity > Zero then
            declare
               This_Quantity : constant Quantity_Type :=
                 Market_Offer.Quantity;
               This_Price    : constant Price_Type :=
                 Market_Offer.Price;
               Max_Quantity  : constant Quantity_Type :=
                 Min (Remaining_Quantity,
                      Get_Quantity (Remaining_Cash, This_Price));
               Traded_Quantity : constant Quantity_Type :=
                 Min (Max_Quantity, This_Quantity);
            begin
               exit when Traded_Quantity = Zero;

               Execute_Offer
                 (Buyer     => Trader,
                  Seller    => Hera.Handles.Trader.Get (Market_Offer.Trader),
                  Commodity => Commodity,
                  Quantity  => Traded_Quantity,
                  Price     => This_Price);

               Hera.Db.Market_Offer.Update_Market_Offer
                 (Market_Offer.Get_Market_Offer_Reference)
                 .Set_Quantity (This_Quantity - Traded_Quantity)
                 .Done;

               Remaining_Cash := Remaining_Cash
                 - Total (This_Price, Traded_Quantity);
               Remaining_Quantity := Remaining_Quantity
                 - Traded_Quantity;

            end;
         end if;
      end loop;

      Hera.Profiling.Stop ("market-buy");

      return Transaction_Result'
        (Quantity => Quantity - Remaining_Quantity,
         Cost     => Cash - Remaining_Cash);

   end Buy;

   ----------------------
   -- Clear_Transients --
   ----------------------

   procedure Clear_Transients
     (Market : Market_Class)
   is
   begin
      Hera.Profiling.Start ("clear-transients");
      for Market_Offer of
        Hera.Db.Market_Offer.Select_By_Market
          (Market.Reference_Market)
      loop
         if Hera.Db.Commodity.Get
           (Market_Offer.Commodity)
           .Transient
         then
            Hera.Db.Market_Offer.Update_Market_Offer
              (Market_Offer.Get_Market_Offer_Reference)
              .Set_Quantity (Hera.Quantities.Zero)
              .Done;
         end if;
      end loop;
      Hera.Profiling.Stop ("clear-transients");
   end Clear_Transients;

   -------------------
   -- Create_Market --
   -------------------

   function Create_Market return Market_Handle is
      Market : constant Hera.Db.Market_Reference :=
        Hera.Db.Market.Create
          (Version    => Market_Version,
           Identifier => Hera.Identifiers.Next_Identifier);
   begin
      return Hera.Handles.Market.Get (Market);
   end Create_Market;

   -------------------
   -- Execute_Offer --
   -------------------

   procedure Execute_Offer
     (Buyer     : Hera.Handles.Trader.Trader_Class;
      Seller    : Hera.Handles.Trader.Trader_Class;
      Commodity : Hera.Db.Commodity_Reference;
      Quantity  : Hera.Quantities.Quantity_Type;
      Price     : Hera.Money.Price_Type)
   is
      Cost : constant Hera.Money.Money_Type :=
        Hera.Money.Total (Price, Quantity);
   begin
      Hera.Profiling.Start ("execute-offer");
      Hera.Accounts.Earn (Seller.Account, Cost);
      Hera.Accounts.Spend (Buyer.Account, Cost);
      Hera.Profiling.Stop ("execute-offer");

      if False then
         Hera.Profiling.Start ("create-transaction");
         Hera.Db.Commodity_Transaction.Create
           (Time_Stamp => Hera.Calendar.Clock,
            Buyer      => Buyer.Reference_Has_Account,
            Seller     => Seller.Reference_Has_Account,
            Amount     => Cost,
            Commodity  => Commodity,
            Quantity   => Quantity);
         Hera.Profiling.Stop ("create-transaction");
      end if;
   end Execute_Offer;

   ----------
   -- Sell --
   ----------

   procedure Sell
     (To_Market : Market_Class;
      Trader    : Hera.Handles.Trader.Trader_Class;
      Stock     : Hera.Handles.Has_Stock.Has_Stock_Class;
      Commodity : Hera.Db.Commodity_Reference;
      Quantity  : Hera.Quantities.Quantity_Type;
      Price     : Hera.Money.Price_Type)
   is
      use Hera.Money, Hera.Quantities;
      Current_Offer : constant Hera.Db.Market_Offer.Market_Offer_Type :=
        Hera.Db.Market_Offer.Get_By_Sell_Offer
          (To_Market.Reference_Market, Trader.Reference_Trader, Commodity);
   begin
      Hera.Profiling.Start ("market-sell");
      if Current_Offer.Has_Element then
         Hera.Db.Market_Offer.Update_Market_Offer
           (Current_Offer.Get_Market_Offer_Reference)
           .Set_Quantity (Current_Offer.Quantity + Quantity)
           .Set_Price (Price)
           .Set_Priority (To_Real (Price))
           .Done;
      else
         Hera.Db.Market_Offer.Create
           (Market    => To_Market.Reference_Market,
            Trader    => Trader.Reference_Trader,
            Commodity => Commodity,
            Quantity  => Quantity,
            Price     => Price,
            Priority  => To_Real (Price));
      end if;

      Hera.Commodities.Remove
        (Has_Stock => Stock,
         Commodity => Hera.Handles.Commodity.Get (Commodity),
         Quantity  => Quantity);
      Hera.Profiling.Stop ("market-sell");
   end Sell;

end Hera.Markets;
