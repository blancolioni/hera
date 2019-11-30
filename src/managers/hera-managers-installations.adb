with WL.Random;
with WL.String_Maps;

with Hera.Money;
with Hera.Quantities;

with Hera.Commodities;
with Hera.Facilities;
with Hera.Sectors;
with Hera.Warehouses;

with Hera.Updates;

package body Hera.Managers.Installations is

   type Root_Installation_Manager is
     new Root_Manager_Type with
      record
         Installation : Hera.Installations.Installation_Type;
      end record;

   overriding procedure Execute
     (Manager : in out Root_Installation_Manager);

   function Manage_Factory_Queue
     (Installation : Hera.Installations.Installation_Type;
      Factory      : Hera.Facilities.Facility_Type)
      return Duration;

   procedure Queue_Building_Modules
     (Installation : Hera.Installations.Installation_Type);

   procedure Queue_Industrial_Goods
     (Installation : Hera.Installations.Installation_Type);

   procedure Queue_Consumer_Goods
     (Installation : Hera.Installations.Installation_Type);

   function Matching_Deposits
     (Sector    : Hera.Sectors.Sector_Type;
      Commodity : Hera.Commodities.Commodity_Type)
      return Boolean;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Manager : in out Root_Installation_Manager)
   is
      use all type Hera.Facilities.Facility_Class;
   begin
      case Manager.Installation.Facility.Class is
         when Colony_Hub =>
            null;
         when Orbital_Dock =>
            null;
         when Factory =>
            declare
               Next_Update_Delay : constant Duration :=
                 Manage_Factory_Queue
                   (Manager.Installation,
                    Manager.Installation.Facility);
            begin
               Manager.Set_Next_Update_Delay (Next_Update_Delay);
            end;

         when Extractor =>
            null;
         when Service =>
            null;
      end case;
   end Execute;

   --------------------------
   -- Installation_Manager --
   --------------------------

   function Installation_Manager
     (Installation : Hera.Installations.Installation_Type)
      return Manager_Type
   is
   begin
      return new Root_Installation_Manager'
        (Active       => True,
         Next_Update  => Hera.Calendar.Clock,
         Installation => Installation);
   end Installation_Manager;

   --------------------------
   -- Manage_Factory_Queue --
   --------------------------

   function Manage_Factory_Queue
     (Installation : Hera.Installations.Installation_Type;
      Factory      : Hera.Facilities.Facility_Type)
      return Duration
   is
      use Hera.Commodities;
      use Hera.Quantities;

      Production  : Stock_List;
      Consumption : Stock_List;

      Prod_Quantity : Quantity_Type := Zero;

      package Required_Lists is
        new WL.String_Maps (Quantity_Type);

      Requirements : Required_Lists.Map;

      procedure Add_Requirement
        (Commodity : Commodity_Type;
         Quantity  : Quantity_Type);

      procedure Save_Commodity
        (Commodity : Commodity_Type;
         Produce   : Stock_Entry;
         Consume   : Stock_Entry);

      procedure Manage_Possible_Production
        (Commodity : Commodity_Type;
         Demand    : Stock_Entry);

      ---------------------
      -- Add_Requirement --
      ---------------------

      procedure Add_Requirement
        (Commodity : Commodity_Type;
         Quantity  : Quantity_Type)
      is
      begin
         if not Requirements.Contains (Commodity.Tag) then
            Requirements.Insert (Commodity.Tag, Zero);
         end if;

         declare
            Q : Quantity_Type renames Requirements (Commodity.Tag);
         begin
            Q := Q + Quantity;
         end;
      end Add_Requirement;

      --------------------------------
      -- Manage_Possible_Production --
      --------------------------------

      procedure Manage_Possible_Production
        (Commodity : Commodity_Type;
         Demand    : Stock_Entry)
      is
         Max : Quantity_Type := Demand.Quantity;
      begin
         if not Factory.Produces_Class (Commodity.Class) then
            return;
         end if;

         Installation.Log
           (Commodity.Tag
            & ": demand " & Show (Demand.Quantity));

         for Component of Commodity.Components loop
            declare
               Available : constant Quantity_Type :=
                 Production.Quantity (Component);
            begin
               Max := Min (Max, Available / Commodity.Quantity (Component));
            end;
         end loop;

         if Max > Zero then
            Installation.Log
              ("adding " & Show (Max) & " " & Commodity.Tag
               & " to queue");
            Installation.Add_To_Queue
              (Commodity, Max,
               Priority => WL.Random.Random_Number (1, 100));

            for Component of Commodity.Components loop
               if Factory.Produces_Class (Component.Class) then
                  Add_Requirement
                    (Commodity => Component,
                     Quantity  => Commodity.Quantity (Component) * Max);
               end if;
            end loop;
         end if;

      end Manage_Possible_Production;

      --------------------
      -- Save_Commodity --
      --------------------

      procedure Save_Commodity
        (Commodity : Commodity_Type;
         Produce   : Stock_Entry;
         Consume   : Stock_Entry)
      is

         procedure Set
           (Target : in out Has_Stock_Interface'Class;
            Quantity : Quantity_Type;
            Original : Stock_Entry);

         ---------
         -- Set --
         ---------

         procedure Set
           (Target   : in out Has_Stock_Interface'Class;
            Quantity : Quantity_Type;
            Original : Stock_Entry)
         is
            New_Value : constant Hera.Money.Money_Type :=
              Hera.Money.Total
                (Hera.Money.Price (Original.Value, Original.Quantity),
                 Quantity);
         begin
            Target.Update_Stock
              (Commodity => Commodity,
               Stock     => (Quantity, New_Value));

            for Component of Commodity.Components loop
               Target.Update_Stock
                 (Component,
                  (Quantity * Commodity.Quantity (Component),
                   Hera.Money.Total
                     (Commodity.Base_Price,
                      Quantity * Commodity.Quantity (Component))));
            end loop;
         end Set;

      begin
         if Produce.Quantity > Consume.Quantity then
            Set (Production, Produce.Quantity - Consume.Quantity, Produce);
         elsif Consume.Quantity > Produce.Quantity
           and then Factory.Produces_Class (Commodity.Class)
         then
            Set (Consumption, Consume.Quantity - Produce.Quantity, Consume);
            Prod_Quantity := Prod_Quantity
              + Consume.Quantity - Produce.Quantity;
         end if;
      end Save_Commodity;

   begin

      Installation.Clear_Queue;
      Installation.Colony.Planet.Iterate_Economy
        (Save_Commodity'Access);

      if Prod_Quantity > Zero then
         Consumption.Scan_Stock (Manage_Possible_Production'Access);

         for Position in Requirements.Iterate loop
            declare
               Commodity  : constant Commodity_Type :=
                 Get (Required_Lists.Key (Position));
               Quantity   : constant Quantity_Type :=
                 Required_Lists.Element (Position);
               Have       : constant Quantity_Type :=
                 Installation.Quantity (Commodity);
               Warehouse  : constant Hera.Warehouses.Warehouse_Type :=
                 Installation.Colony.Warehouse (Installation.Owner);
               Warehoused : constant Quantity_Type :=
                 Warehouse.Quantity (Commodity);
               Remaining  : Quantity_Type := Quantity;
               Value      : Hera.Money.Money_Type;

            begin

               if Remaining > Have then
                  Remaining := Quantity - Have;
                  Warehouse.Remove
                    (Commodity, Min (Warehoused, Remaining), Value);
                  Installation.Add
                    (Commodity, Min (Warehoused, Remaining), Value);
                  Remaining := Remaining - Min (Warehoused, Remaining);
               end if;

               if Remaining > Zero then
                  Installation.Colony.Market.Buy
                    (Trader    => Installation.Owner,
                     Stock     => Installation,
                     Commodity => Commodity,
                     Quantity  => Remaining,
                     Cash      => Installation.Owner.Cash);
               end if;
            end;
         end loop;
      end if;

      if Installation.Queued_Capacity < Installation.Facility.Capacity then
         if Factory.Produces_Class (Hera.Commodities.Building_Module) then
            Queue_Building_Modules (Installation);
         end if;
         if Factory.Produces_Class (Hera.Commodities.Industrial_Good) then
            Queue_Industrial_Goods (Installation);
         end if;
         if Factory.Produces_Class (Hera.Commodities.Consumer_Good) then
            Queue_Consumer_Goods (Installation);
         end if;
      end if;

      if Prod_Quantity = Zero then
         return Hera.Calendar.Days (1);
      else
         return Hera.Calendar.Days (Hera.Updates.Update_Cycle_Days);
      end if;

   end Manage_Factory_Queue;

   -----------------------
   -- Matching_Deposits --
   -----------------------

   function Matching_Deposits
     (Sector    : Hera.Sectors.Sector_Type;
      Commodity : Hera.Commodities.Commodity_Type)
      return Boolean
   is
      use Hera.Commodities;
      Components : constant Commodity_Array := Commodity.Components;
      Available  : constant Commodity_Array := Sector.Resources;
   begin
      for Component of Components loop
         if Component.Class = Resource
           and then not (for some Item of Available => Item = Component)
         then
            return False;
         end if;
      end loop;
      return True;
   end Matching_Deposits;

   ----------------------------
   -- Queue_Building_Modules --
   ----------------------------

   procedure Queue_Building_Modules
     (Installation : Hera.Installations.Installation_Type)
   is
   begin
      for Module of Hera.Commodities.Building_Modules loop
         Installation.Add_To_Queue (Module, Hera.Quantities.Unit);
      end loop;
   end Queue_Building_Modules;

   --------------------------
   -- Queue_Consumer_Goods --
   --------------------------

   procedure Queue_Consumer_Goods
     (Installation : Hera.Installations.Installation_Type)
   is
      Goods : constant Hera.Commodities.Commodity_Array :=
        Hera.Commodities.Consumer_Goods;
   begin
      for Item of Goods loop
         if Matching_Deposits
           (Installation.Sector, Item)
         then
            Installation.Add_To_Queue (Item, Installation.Facility.Capacity);
         end if;
      end loop;
   end Queue_Consumer_Goods;

   ----------------------------
   -- Queue_Industrial_Goods --
   ----------------------------

   procedure Queue_Industrial_Goods
     (Installation : Hera.Installations.Installation_Type)
   is
   begin
      for Item of Hera.Commodities.Industrial_Goods loop
         if Matching_Deposits
           (Installation.Sector, Item)
         then
            Installation.Add_To_Queue (Item, Installation.Facility.Capacity);
         end if;
      end loop;
   end Queue_Industrial_Goods;

end Hera.Managers.Installations;
