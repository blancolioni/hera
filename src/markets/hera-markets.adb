with Ada.Containers.Doubly_Linked_Lists;

with Hera.Markets.Transactions;

package body Hera.Markets is

   Market_Version : constant Hera.Objects.Object_Version := "0.0.1";
   Trade_Node_Version : constant Hera.Objects.Object_Version := "0.0.1";

   Log_Market : constant Boolean := True;

   package Trade_Center_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Trade_Center_Type);

   Trade_Centers : Trade_Center_Lists.List;

   type Stock_Access is access all
     Hera.Commodities.Has_Stock_Interface'Class;

   type Market_Offer_Update is abstract new Hera.Objects.Root_Update_Type with
      record
         Trader    : Trader_Type;
         Commodity : Hera.Commodities.Commodity_Type;
         Quantity  : Hera.Quantities.Quantity_Type;
      end record;

   type Add_Sell_Offer_Update is new Market_Offer_Update with
      record
         Price : Hera.Money.Price_Type;
      end record;

   overriding procedure Execute
     (Update : Add_Sell_Offer_Update;
      Target : in out Hera.Objects.Root_Hera_Object'Class);

   overriding function Description
     (Update : Add_Sell_Offer_Update)
      return String
   is ("offer "
       & Hera.Quantities.Show (Update.Quantity)
       & " " & Update.Commodity.Tag
       & " for " & Hera.Money.Show (Update.Price) & " ea");

   type Add_Buy_Offer_Update is new Market_Offer_Update with
      record
         Stock  : Stock_Access;
         Budget : Hera.Money.Money_Type;
      end record;

   overriding procedure Execute
     (Update : Add_Buy_Offer_Update;
      Target : in out Hera.Objects.Root_Hera_Object'Class);

   overriding function Description
     (Update : Add_Buy_Offer_Update)
      return String
   is ("want "
       & Hera.Quantities.Show (Update.Quantity)
       & " " & Update.Commodity.Tag
       & " for " & Hera.Money.Show (Update.Budget));

   ---------------
   -- Available --
   ---------------

   function Available
     (Market    : Root_Market_Type'Class;
      Commodity : Hera.Commodities.Commodity_Type;
      Budget    : Hera.Money.Money_Type)
      return Hera.Quantities.Quantity_Type
   is
      use Hera.Money;
      use Hera.Quantities;
      Remaining : Money_Type := Budget;
      Total     : Quantity_Type := Zero;

      procedure Add_Quantity
        (Price : Hera.Money.Price_Type;
         Item  : Sell_Offer_Record);

      ------------------
      -- Add_Quantity --
      ------------------

      procedure Add_Quantity
        (Price : Hera.Money.Price_Type;
         Item  : Sell_Offer_Record)
      is
         pragma Unreferenced (Price);
      begin
         if Remaining > Zero then
            declare
               This_Total : constant Money_Type :=
                              Hera.Money.Total (Item.Price, Item.Quantity);
            begin
               if This_Total <= Remaining then
                  Total := Total + Item.Quantity;
                  Remaining := Remaining - This_Total;
               else
                  Total := Total
                    + Scale (Item.Quantity,
                             To_Real (Remaining) / To_Real (This_Total));
                  Remaining := Zero;
               end if;
            end;
         end if;
      end Add_Quantity;

   begin
      if Market.Sell_Offers.Contains (Commodity) then
         Market.Sell_Offers.Element (Commodity).Iterate (Add_Quantity'Access);
      end if;
      return Total;
   end Available;

   ---------
   -- Buy --
   ---------

   procedure Buy
     (From_Market : Root_Market_Type'Class;
      Trader      : not null access constant Trader_Interface'Class;
      Stock       : not null access constant
        Hera.Commodities.Has_Stock_Interface'Class;
      Commodity   : Hera.Commodities.Commodity_Type;
      Quantity    : Hera.Quantities.Quantity_Type;
      Cash        : Hera.Money.Money_Type)
   is
   begin
      if Log_Market then
         From_Market.Log (Trader.Name & " wants "
                          & Hera.Quantities.Show (Quantity)
                          & " " & Commodity.Tag
                          & "; budget "
                          & Hera.Money.Show (Cash));
      end if;

      From_Market.Add_Update
        (Add_Buy_Offer_Update'
           (Hera.Objects.Root_Update_Type with
            Trader    => Trader_Type (Trader),
            Stock     => Stock_Access (Stock),
            Commodity => Commodity,
            Quantity  => Quantity,
            Budget    => Cash));
   end Buy;

   ----------------------
   -- Clear_Transients --
   ----------------------

   procedure Clear_Transients (Market : Root_Market_Type'Class) is
   begin
      null;
   end Clear_Transients;

   -------------------
   -- Create_Market --
   -------------------

   function Create_Market return Market_Type is
      Market : constant Root_Market_Type :=
        Root_Market_Type'
          (Hera.Objects.Root_Hera_Object with
           Sell_Offers => <>);
   begin
      return Market_Type
        (Market.New_Object (Market_Version));
   end Create_Market;

   ------------------
   -- Current_Cost --
   ------------------

   function Current_Cost
     (Market    : Root_Market_Type'Class;
      Commodity : Hera.Commodities.Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type)
      return Hera.Money.Money_Type
   is
      use Hera.Money;
      use Hera.Quantities;
      Remaining : Quantity_Type := Quantity;
      Total     : Money_Type := Zero;

      procedure Add_Cost
        (Price : Hera.Money.Price_Type;
         Item  : Sell_Offer_Record);

      procedure Add_Cost
        (Price : Hera.Money.Price_Type;
         Item  : Sell_Offer_Record)
      is
         pragma Unreferenced (Price);
      begin
         if Remaining > Zero then
            Total := Total +
              Hera.Money.Total (Item.Price, Min (Item.Quantity, Remaining));
            Remaining := Remaining - Min (Item.Quantity, Remaining);
         end if;
      end Add_Cost;

   begin
      if Market.Sell_Offers.Contains (Commodity) then
         Market.Sell_Offers.Element (Commodity).Iterate (Add_Cost'Access);
      end if;
      return Total;
   end Current_Cost;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Add_Sell_Offer_Update;
      Target : in out Hera.Objects.Root_Hera_Object'Class)
   is
      Market : Root_Market_Type'Class renames
        Root_Market_Type'Class (Target);
   begin
      if not Market.Sell_Offers.Contains (Update.Commodity) then
         Market.Sell_Offers.Insert
           (Update.Commodity, Sell_Offer_Queues.Empty_Heap);
      end if;

      Market.Sell_Offers.Reference (Update.Commodity).Insert
        (Key     => Update.Price,
         Element => Sell_Offer_Record'
           (Trader    => Update.Trader,
            Commodity => Update.Commodity,
            Quantity  => Update.Quantity,
            Price     => Update.Price));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Add_Buy_Offer_Update;
      Target : in out Hera.Objects.Root_Hera_Object'Class)
   is
      use Hera.Money, Hera.Quantities;
      Market : Root_Market_Type'Class renames
        Root_Market_Type'Class (Target);

      Budget : constant Money_Type := Update.Budget;
      Wanted : constant Quantity_Type := Update.Quantity;
      Total_Paid   : Money_Type := Zero;
      Total_Bought : Quantity_Type := Zero;

   begin

      if not Market.Sell_Offers.Contains (Update.Commodity) then
         return;
      end if;

      declare
         Sell_Queue   : Sell_Offer_Queues.Heap renames
           Market.Sell_Offers.Reference (Update.Commodity);
      begin
         while not Sell_Queue.Is_Empty
           and then Total_Paid < Budget
           and then Total_Bought < Wanted
         loop
            declare
               Next_Offer : Sell_Offer_Record := Sell_Queue.First_Element;
               Max_Paid   : constant Money_Type := Budget - Total_Paid;
               Max_Buy    : constant Quantity_Type :=
                 Min
                   (Min
                      (Wanted - Total_Bought,
                       Get_Quantity (Max_Paid, Next_Offer.Price)),
                    Next_Offer.Quantity);
               Total_Cost : constant Money_Type :=
                 Total (Next_Offer.Price, Max_Buy);
            begin
               exit when Max_Buy = Zero;

               declare
                  Transaction : constant Hera.Accounts.Transaction_Type :=
                    Hera.Markets.Transactions.Market_Transaction
                      (Buyer     => Update.Trader,
                       Seller    => Next_Offer.Trader,
                       Commodity => Next_Offer.Commodity,
                       Quantity  => Max_Buy,
                       Price     => Next_Offer.Price);
               begin
                  Next_Offer.Trader.Account.Earn
                    (Transaction => Transaction,
                     Amount      => Total_Cost);
                  Update.Trader.Account.Spend
                    (Transaction => Transaction,
                     Amount      => Total_Cost);

                  Total_Paid := Total_Paid + Total_Cost;
                  Total_Bought := Total_Bought + Max_Buy;

                  Next_Offer.Trader.On_Sell
                    (Commodity    => Update.Commodity,
                     Quantity     => Max_Buy,
                     Total_Earned => Total_Cost);

                  Next_Offer.Quantity := Next_Offer.Quantity - Max_Buy;

                  Sell_Queue.Delete_First;

                  if Next_Offer.Quantity > Zero then
                     Sell_Queue.Insert (Next_Offer.Price, Next_Offer);
                     exit;
                  end if;
               end;
            end;
         end loop;

         if Total_Bought > Zero then
            Market.Log (Update.Trader.Name & " buys "
                        & Hera.Quantities.Show (Total_Bought)
                        & " " & Update.Commodity.Tag
                        & " for "
                        & Hera.Money.Show (Total_Paid));
            Update.Stock.Add (Update.Commodity, Total_Bought, Total_Paid);
            Update.Trader.On_Buy
              (Commodity  => Update.Commodity,
               Quantity   => Total_Bought,
               Total_Paid => Total_Paid);
         end if;
      end;
   end Execute;

   ---------------
   -- Get_Stock --
   ---------------

   overriding function Get_Stock
     (From      : Root_Trade_Node_Type;
      Commodity : Hera.Commodities.Commodity_Type)
      return Hera.Commodities.Stock_Entry
   is
   begin
      return From.Stock.Get_Stock (Commodity);
   end Get_Stock;

   ---------------------------
   -- Iterate_Trade_Centers --
   ---------------------------

   procedure Iterate_Trade_Centers
     (Process : not null access
        procedure (Name : String;
                   Production : Trade_Node_Type;
                   Consumption : Trade_Node_Type))
   is
   begin
      for Center of Trade_Centers loop
         Process (Ada.Strings.Unbounded.To_String (Center.Name),
                  Center.Production, Center.Consumption);
      end loop;
   end Iterate_Trade_Centers;

   ----------------------
   -- New_Trade_Center --
   ----------------------

   procedure New_Trade_Center
     (Name        : String;
      Production  : Trade_Node_Type;
      Consumption : Trade_Node_Type)
   is
   begin
      Trade_Centers.Append
        (Trade_Center_Type'
           (Name        => Ada.Strings.Unbounded.To_Unbounded_String (Name),
            Production  => Production,
            Consumption => Consumption));
   end New_Trade_Center;

   --------------------
   -- New_Trade_Node --
   --------------------

   function New_Trade_Node
     (Name : String)
      return Trade_Node_Type
   is
      Trade_Node : Root_Trade_Node_Type :=
        Root_Trade_Node_Type'
          (Hera.Objects.Root_Named_Object with
           Stock => <>);
   begin
      Trade_Node.Initialize (Trade_Node_Version, Name);
      Trade_Node.Stock.Initialize_Stock_List (Trade_Node.Identifier);

      return Node : constant Trade_Node_Type :=
        Trade_Node_Type (Trade_Node.Save_Object)
      do
         null;
      end return;
   end New_Trade_Node;

   ----------------
   -- Scan_Stock --
   ----------------

   overriding procedure Scan_Stock
     (List      : Root_Trade_Node_Type;
      Process   : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Stock : Hera.Commodities.Stock_Entry))
   is
   begin
      List.Stock.Scan_Stock (Process);
   end Scan_Stock;

   ----------
   -- Sell --
   ----------

   procedure Sell
     (To_Market : Root_Market_Type'Class;
      Trader    : not null access constant Trader_Interface'Class;
      Stock : not null access constant Hera.Commodities.Has_Stock_Interface'
        Class;
      Commodity : Hera.Commodities.Commodity_Type;
      Quantity  : Hera.Quantities.Quantity_Type;
      Price     : Hera.Money.Price_Type)
   is
   begin
      if Log_Market then
         To_Market.Log (Trader.Name & " offers "
                        & Hera.Quantities.Show (Quantity)
                        & " " & Commodity.Tag
                        & " for "
                        & Hera.Money.Show (Price)
                        & " ea; total "
                        & Hera.Money.Show
                          (Hera.Money.Total (Price, Quantity)));
      end if;

      Stock.Remove (Commodity, Quantity);
      To_Market.Add_Update
        (Add_Sell_Offer_Update'
           (Hera.Objects.Root_Update_Type with
                Trader => Trader_Type (Trader),
                Commodity => Commodity,
                Quantity  => Quantity,
                Price     => Price));
   end Sell;

   ---------------
   -- Set_Stock --
   ---------------

   overriding procedure Set_Stock
     (To        : Root_Trade_Node_Type;
      Commodity : Hera.Commodities.Commodity_Type;
      Stock     : Hera.Commodities.Stock_Entry)
   is
   begin
      To.Stock.Set_Stock (Commodity, Stock);
   end Set_Stock;

   ---------------------
   -- Total_Available --
   ---------------------

   function Total_Available
     (Market    : Root_Market_Type'Class;
      Commodity : Hera.Commodities.Commodity_Type)
      return Hera.Quantities.Quantity_Type
   is
      use Hera.Quantities;
      Total : Quantity_Type := Zero;

      procedure Add_Quantity
        (Price : Hera.Money.Price_Type;
         Item  : Sell_Offer_Record);

      ------------------
      -- Add_Quantity --
      ------------------

      procedure Add_Quantity
        (Price : Hera.Money.Price_Type;
         Item  : Sell_Offer_Record)
      is
         pragma Unreferenced (Price);
      begin
         Total := Total + Item.Quantity;
      end Add_Quantity;

   begin
      if Market.Sell_Offers.Contains (Commodity) then
         Market.Sell_Offers.Element (Commodity).Iterate (Add_Quantity'Access);
      end if;
      return Total;
   end Total_Available;

   ------------------
   -- Update_Stock --
   ------------------

   overriding procedure Update_Stock
     (Trade_Node : in out Root_Trade_Node_Type;
      Commodity  : Hera.Commodities.Commodity_Type;
      Stock      : Hera.Commodities.Stock_Entry)
   is
   begin
      Trade_Node.Stock.Update_Stock (Commodity, Stock);
   end Update_Stock;

end Hera.Markets;
