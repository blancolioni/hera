with WL.Heaps;
with WL.Random.Weighted_Random_Choices;

with Hera.Money;
with Hera.Quantities;

with Hera.Commodities;
with Hera.Markets;
with Hera.Ships;

with Hera.Ships.Updates;

with Hera.Handles.Market;
with Hera.Handles.Star_System;

with Hera.Db.Commodity;
with Hera.Db.Ship;
with Hera.Db.Star_System_Distance;
with Hera.Db.Stock_Item;

package body Hera.Managers.Ships.Trade is

   type Root_Trade_Manager is
     new Root_Ship_Manager with
      record
         null;
      end record;

   type Manager_Access is access all Root_Trade_Manager'Class;

   overriding function Identifier
     (Manager : Root_Trade_Manager)
      return String
   is (Manager.Ship.Name);

   overriding procedure Activate
     (Manager : not null access Root_Trade_Manager);

   procedure Create_Orders
     (Manager : Root_Trade_Manager'Class);

   procedure Sell_Cargo
     (Manager : Root_Trade_Manager'Class);

   procedure Choose_Destination
     (Manager : Root_Trade_Manager'Class);

   procedure Buy_Cargo
     (Manager : Root_Trade_Manager'Class);

   procedure Depart
     (Manager : Root_Trade_Manager'Class);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Root_Trade_Manager)
   is
   begin
      Manager.Log ("activating");

      case Manager.Ship.Status is
         when Hera.Db.Idle =>
            Manager.Create_Orders;
         when others =>
            null;
      end case;
   end Activate;

   ---------------
   -- Buy_Cargo --
   ---------------

   procedure Buy_Cargo
     (Manager : Root_Trade_Manager'Class)
   is
      use Hera.Quantities;

      Local_Market : constant Hera.Handles.Market.Market_Handle :=
        Hera.Markets.Get (Manager.Ship.Star_System);

      function Score_Commodity
        (Commodity : Hera.Db.Commodity_Reference)
         return Non_Negative_Real;

      ---------------------
      -- Score_Commodity --
      ---------------------

      function Score_Commodity
        (Commodity : Hera.Db.Commodity_Reference)
         return Non_Negative_Real
      is
         use Hera.Money;
         Remote_Market    : constant Hera.Handles.Market.Market_Handle :=
           Hera.Markets.Get (Manager.Ship.Destination);
         Local_Available  : constant Quantity_Type :=
           Hera.Markets.Current_Available
             (Local_Market, Commodity);
         Local_Price      : constant Price_Type :=
           Hera.Markets.Current_Sell_Price
             (Local_Market, Commodity);
         Remote_Price     : constant Price_Type :=
           Hera.Markets.Current_Buy_Price
             (Remote_Market, Commodity);
      begin
         if Local_Available > Zero
           and then Remote_Price > Local_Price
         then
            return To_Real (Remote_Price) / To_Real (Local_Price);
         else
            return 0.0;
         end if;
      end Score_Commodity;

      package Cargo_Heaps is
        new WL.Heaps (Real, Hera.Db.Commodity_Reference, ">",
                      Hera.Db."=");
      Queue : Cargo_Heaps.Heap;
      Remaining : Quantity_Type := Hera.Ships.Maximum_Cargo (Manager.Ship);
   begin
      for Commodity of
        Hera.Db.Commodity.Scan_By_Top_Record
      loop
         declare
            Score : constant Non_Negative_Real :=
              Score_Commodity (Commodity.Get_Commodity_Reference);
         begin
            if Score > 0.0 then
               Queue.Insert (Score, Commodity.Get_Commodity_Reference);
            end if;
         end;
      end loop;

      while not Queue.Is_Empty loop
         declare
            Commodity : constant Hera.Db.Commodity_Reference :=
              Queue.First_Element;
            Quantity  : constant Quantity_Type :=
              Min (Remaining,
                   Hera.Markets.Current_Available (Local_Market, Commodity));
         begin
            Queue.Delete_First;
            Hera.Markets.Buy
              (From_Market => Local_Market,
               Agent       => Manager.Owner,
               Stock       => Manager.Has_Stock,
               Commodity   => Commodity,
               Quantity    => Quantity);
            Remaining := Remaining - Quantity;
            exit when Remaining = Zero;
         end;
      end loop;

   end Buy_Cargo;

   ------------------------
   -- Choose_Destination --
   ------------------------

   procedure Choose_Destination
     (Manager : Root_Trade_Manager'Class)
   is

      Local_Market : constant Hera.Handles.Market.Market_Handle :=
        Hera.Markets.Get (Manager.Ship.Star_System);

      Max_Cargo : constant Hera.Quantities.Quantity_Type :=
        Hera.Ships.Maximum_Cargo (Manager.Ship);

      function Score_Star_System
        (Star_System : Hera.Handles.Star_System.Star_System_Handle;
         Distance    : Non_Negative_Real)
         return Natural;

      -----------------------
      -- Score_Star_System --
      -----------------------

      function Score_Star_System
        (Star_System : Hera.Handles.Star_System.Star_System_Handle;
         Distance    : Non_Negative_Real)
         return Natural
      is
         Score : Non_Negative_Real := 0.0;
      begin
         if not Hera.Markets.Has_Market (Star_System) then
            return 0;
         end if;

         for Commodity of
           Hera.Db.Commodity.Scan_By_Top_Record
         loop
            declare
               use Hera.Money, Hera.Quantities;
               Reference     : constant Hera.Db.Commodity_Reference :=
                 Commodity.Get_Commodity_Reference;
               Remote_Market : constant Hera.Handles.Market.Market_Handle :=
                 Hera.Markets.Get (Star_System);
               Local_Available : constant Quantity_Type :=
                 Hera.Markets.Current_Available
                   (Local_Market, Reference);
               Local_Price     : constant Price_Type :=
                 Hera.Markets.Current_Sell_Price
                   (Local_Market, Reference);
               Remote_Price     : constant Price_Type :=
                 Hera.Markets.Current_Buy_Price
                   (Remote_Market, Reference);
            begin
               if Local_Available > Zero
                 and then Remote_Price > Local_Price
               then
                  Score := Score
                    + To_Real (Min (Local_Available, Max_Cargo))
                    * To_Real (Remote_Price - Local_Price);
               end if;
            end;
         end loop;

         return Natural (Score / Distance);

      end Score_Star_System;

      package Destination_Choices is
        new WL.Random.Weighted_Random_Choices
          (Hera.Db.Star_System_Reference);

      Choices : Destination_Choices.Weighted_Choice_Set;
   begin
      for Destination of
        Hera.Db.Star_System_Distance.Select_Distance_From_Bounded_By_Distance
          (Manager.Ship.Star_System.Reference_Star_System, 0.0,
           Hera.Ships.Maximum_Jump (Manager.Ship))
      loop
         Choices.Insert
           (Item  => Destination.To,
            Score =>
              Score_Star_System
                (Hera.Handles.Star_System.Get (Destination.To),
                 Destination.Distance));
      end loop;

      if not Choices.Is_Empty then
         declare
            Destination : constant Hera.Db.Star_System_Reference :=
              Choices.Choose;
         begin
            Manager.Log
              ("next destination: "
               & Hera.Handles.Star_System.Get (Destination).Name);
            Hera.Db.Ship.Update_Ship (Manager.Ship.Reference)
              .Set_Destination (Destination)
              .Done;
         end;
      else
         Manager.Log
           ("no destination found");
      end if;
   end Choose_Destination;

   -------------------
   -- Create_Orders --
   -------------------

   procedure Create_Orders
     (Manager : Root_Trade_Manager'Class)
   is
      use type Hera.Db.Star_System_Reference;
   begin
      Manager.Sell_Cargo;
      Manager.Choose_Destination;

      if Manager.Ship.Destination.Reference_Star_System
        /= Hera.Db.Null_Star_System_Reference
      then
         Manager.Buy_Cargo;
         Manager.Depart;
      end if;
   end Create_Orders;

   ------------
   -- Depart --
   ------------

   procedure Depart
     (Manager : Root_Trade_Manager'Class)
   is
   begin
      Hera.Db.Ship.Update_Ship (Manager.Ship.Reference_Ship)
        .Set_Status (Hera.Db.Leaving)
        .Done;
      Hera.Ships.Updates.Signal
        (Manager.Ship.Reference_Ship);
   end Depart;

   ----------------
   -- Sell_Cargo --
   ----------------

   procedure Sell_Cargo
     (Manager : Root_Trade_Manager'Class)
   is
      use Hera.Quantities;
      Market : constant Hera.Handles.Market.Market_Handle :=
        Hera.Markets.Get (Manager.Ship.Star_System);
      Stock  : Hera.Commodities.Stock_List;
   begin
      for Stock_Item of
        Hera.Db.Stock_Item.Select_By_Has_Stock
          (Manager.Has_Stock)
      loop
         if Stock_Item.Quantity > Zero then
            Hera.Commodities.Add
              (Stock, Stock_Item.Commodity, Stock_Item.Quantity);
         end if;
      end loop;

      declare
         procedure Sell
           (Commodity : Hera.Db.Commodity_Reference;
            Quantity  : Hera.Quantities.Quantity_Type);

         procedure Sell
           (Commodity : Hera.Db.Commodity_Reference;
            Quantity  : Hera.Quantities.Quantity_Type)
         is
         begin
            Hera.Markets.Sell
              (Market, Manager.Owner, Manager.Has_Stock,
               Commodity, Quantity);
         end Sell;

      begin
         Hera.Commodities.Iterate (Stock, Sell'Access);
      end;
   end Sell_Cargo;

   -------------------
   -- Trade_Manager --
   -------------------

   function Trade_Manager
     (Ship : Hera.Handles.Ship.Ship_Handle)
      return Manager_Type
   is
      S : constant Hera.Db.Ship.Ship_Type :=
        Hera.Db.Ship.Get (Ship.Reference);
      Manager : constant Manager_Access :=
        new Root_Trade_Manager'
          (Root_Manager_Type with
           Ship => Ship,
           Has_Stock => S.Get_Has_Stock_Reference,
           Owner     => S.Owner);
   begin
      return Manager_Type (Manager);
   end Trade_Manager;

end Hera.Managers.Ships.Trade;
