with Hera.Logging;
with Hera.Money;
with Hera.Quantities;
with Hera.Random;
with Hera.Updates;

with Hera.Commodities;
with Hera.Planets;
with Hera.Sectors;

package body Hera.Facilities.Updates is

   procedure Update_Colony_Hub
     (Efficiency    : Unit_Real;
      On_Production : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Quantity  : Hera.Quantities.Quantity_Type;
                   Cost      : Hera.Money.Money_Type));

   procedure Extract_Resources
     (Sector        : Hera.Sectors.Sector_Type;
      Facility      : Facility_Type;
      Efficiency    : Unit_Real;
      On_Extraction : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Quantity  : Hera.Quantities.Quantity_Type;
                   Cost      : Hera.Money.Money_Type));

   procedure Execute_Production_Queue
     (Facility      : Facility_Type;
      Planet        : Hera.Planets.Planet_Type;
      Efficiency    : Unit_Real;
      Stock         : in out Hera.Commodities.Has_Stock_Interface'Class;
      Queue         : in out Production_Queue;
      On_Production : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Quantity  : Hera.Quantities.Quantity_Type;
                   Cost      : Hera.Money.Money_Type));

   procedure Execute_Service_Production
     (Facility      : Facility_Type;
      Efficiency    : Unit_Real;
      On_Production : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Quantity  : Hera.Quantities.Quantity_Type;
                   Cost      : Hera.Money.Money_Type));

   ------------------------
   -- Execute_Production --
   ------------------------

   procedure Execute_Production
     (Facility : Hera.Facilities.Facility_Type;
      Employer : not null access constant
        Hera.Colonies.Employer_Interface'Class;
      Stock    : Hera.Commodities.Has_Stock_Interface'Class;
      Colony   : Hera.Colonies.Colony_Type;
      Owner    : Hera.Corporations.Corporation_Type;
      Queue    : in out Production_Queue)
   is
      use type Hera.Money.Money_Type;

      Efficiency : Unit_Real := 1.0;
      Total_Cost : Hera.Money.Money_Type := Hera.Money.Zero;
      Total_Quantity : Hera.Quantities.Quantity_Type := Hera.Quantities.Zero;

      type Production_Record is
         record
            Commodity : Hera.Commodities.Commodity_Type;
            Quantity  : Hera.Quantities.Quantity_Type;
            Value     : Hera.Money.Money_Type;
         end record;

      package Production_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Production_Record);
      Production : Production_Lists.List;

   begin

      for Pop_Class of Facility.Workers loop
         declare
            use type Hera.Quantities.Quantity_Type;
            Required : constant Hera.Quantities.Quantity_Type :=
              Facility.Quantity (Pop_Class);
            Available : constant Hera.Quantities.Quantity_Type :=
              Colony.Employees (Employer, Pop_Class);
            Salary    : constant Hera.Money.Price_Type :=
              Hera.Money.Adjust_Price
                (Pop_Class.Salary,
                 1.0 / Hera.Updates.Update_Cycle_Days);
            This_Efficiency : constant Unit_Real :=
              (if Available >= Required
               then 1.0
               else Hera.Quantities.To_Real (Available)
                   / Hera.Quantities.To_Real (Required));
         begin
            Efficiency :=
              Real'Min (Efficiency, This_Efficiency);
--                / Hera.Updates.Update_Cycle_Days;
            Total_Cost := Total_Cost +
              Hera.Money.Total (Salary, Available);
         end;
      end loop;

      Hera.Logging.Log
        (Owner.Name,
         "execute production on "
         & Colony.Name
         & ": "
         & Facility.Tag
         & ": efficiency:"
         & Natural'Image
           (Natural
                (Efficiency * 100.0))
         & "%");

      declare
         procedure On_Production
           (Commodity : Hera.Commodities.Commodity_Type;
            Quantity  : Hera.Quantities.Quantity_Type;
            Cost      : Hera.Money.Money_Type);

         -------------------
         -- On_Production --
         -------------------

         procedure On_Production
           (Commodity : Hera.Commodities.Commodity_Type;
            Quantity  : Hera.Quantities.Quantity_Type;
            Cost      : Hera.Money.Money_Type)
         is
            use type Hera.Quantities.Quantity_Type;
         begin
            Owner.Log
              (Facility.Tag
               & " produces "
               & Hera.Quantities.Show (Quantity)
               & " "
               & Commodity.Tag
               & " complexity "
               & Hera.Quantities.Show (Commodity.Complexity));

            Production.Append ((Commodity, Quantity, Cost));
            Total_Quantity := Total_Quantity + Quantity;
         end On_Production;

      begin
         case Facility.Class is
            when Colony_Hub =>
               Update_Colony_Hub
                 (Efficiency    => Efficiency,
                  On_Production => On_Production'Access);

            when Orbital_Dock =>
               null;

            when Extractor =>
               Extract_Resources
                 (Sector        => Colony.Sector,
                  Facility      => Facility,
                  Efficiency    => Efficiency,
                  On_Extraction => On_Production'Access);

            when Factory =>
               declare
                  Local_Stock : Hera.Commodities.Stock_List;

                  procedure Set
                    (Commodity : Hera.Commodities.Commodity_Type;
                     Item      : Hera.Commodities.Stock_Entry);

                  ---------
                  -- Set --
                  ---------

                  procedure Set
                    (Commodity : Hera.Commodities.Commodity_Type;
                     Item      : Hera.Commodities.Stock_Entry)
                  is
                  begin
                     Stock.Set_Stock (Commodity, Item);
                  end Set;

               begin
                  Local_Stock.Replace (Stock);
                  Execute_Production_Queue
                    (Facility      => Facility,
                     Planet        => Colony.Planet,
                     Efficiency    => Efficiency,
                     Stock         => Local_Stock,
                     Queue         => Queue,
                     On_Production => On_Production'Access);
                  Stock.Scan_Stock (Set'Access);
               end;

            when Service =>
               Execute_Service_Production
                 (Facility      => Facility,
                  Efficiency    => Efficiency,
                  On_Production => On_Production'Access);

         end case;
      end;

      declare
         use Hera.Quantities;
      begin
         if Total_Quantity > Zero then
            for Item of Production loop
               declare
                  use Hera.Money;
                  This_Cost : constant Money_Type :=
                    Adjust (Total_Cost,
                            Hera.Quantities.To_Real (Item.Quantity)
                            / Hera.Quantities.To_Real (Total_Quantity))
                      + Item.Value;
               begin
                  Colony.Warehouse (Owner).Add
                    (Item.Commodity, Item.Quantity, This_Cost);
                  Colony.Planet.On_Production
                    (Item.Commodity, Item.Quantity, This_Cost);
               end;
            end loop;
         end if;
      end;

   end Execute_Production;

   ------------------------------
   -- Execute_Production_Queue --
   ------------------------------

   procedure Execute_Production_Queue
     (Facility      : Facility_Type;
      Planet        : Hera.Planets.Planet_Type;
      Efficiency    : Unit_Real;
      Stock         : in out Hera.Commodities.Has_Stock_Interface'Class;
      Queue         : in out Production_Queue;
      On_Production : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Quantity  : Hera.Quantities.Quantity_Type;
                   Cost      : Hera.Money.Money_Type))
   is
      use Hera.Quantities;
      Capacity    : Quantity_Type :=
        Scale (Facility.Capacity, Efficiency);

      function Consume_Components
        (Commodity : Hera.Commodities.Commodity_Type;
         Quantity  : Quantity_Type)
         return Quantity_Type;

      ------------------------
      -- Consume_Components --
      ------------------------

      function Consume_Components
        (Commodity : Hera.Commodities.Commodity_Type;
         Quantity  : Quantity_Type)
         return Quantity_Type
      is
         Max : Quantity_Type := Quantity;
      begin
         Facility.Log ("queued: " & Show (Quantity) & " " & Commodity.Tag);

         for Component of Commodity.Components loop
            declare
               Require : constant Quantity_Type :=
                 Commodity.Quantity (Component) * Quantity;
               Have    : constant Quantity_Type :=
                 Stock.Quantity (Component);
            begin
               Max :=
                 (if Have < Scale (Require, 0.01)
                  then Zero
                  else Min (Max, Quantity * Have / Require));
               if Max < To_Quantity (0.01) then
                  Max := Zero;
               end if;

               Facility.Log
                 (Component.Tag
                  & ": have " & Show (Have)
                  & "; require " & Show (Require)
                  & "; max now " & Show (Max));
            end;
         end loop;

         if Max > Zero then
            for Component of Commodity.Components loop
               declare
                  use Hera.Money;
                  Current   : constant Hera.Commodities.Stock_Entry :=
                    Stock.Get_Stock (Component);
                  Have      : constant Quantity_Type := Current.Quantity;
                  Require   : constant Quantity_Type :=
                    Min (Commodity.Quantity (Component) * Max, Have);
                  Old_Value : constant Hera.Money.Money_Type := Current.Value;
                  New_Value : constant Hera.Money.Money_Type :=
                    Hera.Money.Total (Hera.Money.Price (Old_Value, Have),
                                      Have - Require);
               begin
                  Facility.Log ("consume " & Show (Require) & " of "
                                & Show (Have) & " " & Component.Tag);

                  pragma Assert (Require <= Have);
                  Stock.Update_Stock (Component, (Have - Require, New_Value));
                  Planet.On_Consumption
                    (Component, Require, Old_Value - New_Value);
               end;
            end loop;
         end if;

         return Max;
      end Consume_Components;

      New_Queue : Production_Queue;
      Changed   : Boolean := False;

   begin
      while Queue.Has_Queued_Production
        and then Capacity > Zero
      loop
         if Queue.Next_Production.Quantity = Zero then
            Queue.Delete_First;
         else
            declare
               use Hera.Commodities;
               First_Item       : constant Production_Item :=
                 Queue.Next_Production;
               Commodity        : constant Hera.Commodities.Commodity_Type :=
                 First_Item.Commodity;
               Complexity       : constant Quantity_Type :=
                 Commodity.Complexity;
               Complexity_Limit : constant Quantity_Type :=
                 Min (First_Item.Quantity * Complexity,
                      Capacity);
               Max_Quantity     : constant Quantity_Type :=
                 Complexity_Limit / Complexity;
               Quantity         : constant Quantity_Type :=
                 (if Max_Quantity = Zero
                  then Zero
                  else Consume_Components
                    (First_Item.Commodity, Max_Quantity));
            begin
               if Quantity > Zero then
                  Changed := True;
                  On_Production (First_Item.Commodity,
                                 Quantity, Hera.Money.Zero);
                  Capacity := Capacity - Quantity * Complexity;
               end if;

               if Quantity < First_Item.Quantity then
                  New_Queue.Append
                    ((Commodity, First_Item.Quantity - Quantity));
               end if;

               Queue.Delete_First;
            end;
         end if;
      end loop;

      if Changed then
         for Item of Queue.Queue loop
            New_Queue.Queue.Append (Item);
         end loop;
         New_Queue.Changed := True;
         Queue := New_Queue;
      else
         Queue.Changed := False;
      end if;

   end Execute_Production_Queue;

   --------------------------------
   -- Execute_Service_Production --
   --------------------------------

   procedure Execute_Service_Production
     (Facility      : Facility_Type;
      Efficiency    : Unit_Real;
      On_Production : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Quantity  : Hera.Quantities.Quantity_Type;
                   Cost      : Hera.Money.Money_Type))
   is
      use type Hera.Quantities.Quantity_Type;
   begin
      On_Production
        (Facility.Service,
         Hera.Quantities.Scale (Facility.Capacity, Efficiency)
         / Facility.Service.Complexity,
         Hera.Money.Zero);
   end Execute_Service_Production;

   -----------------------
   -- Extract_Resources --
   -----------------------

   procedure Extract_Resources
     (Sector        : Hera.Sectors.Sector_Type;
      Facility      : Facility_Type;
      Efficiency    : Unit_Real;
      On_Extraction : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Quantity  : Hera.Quantities.Quantity_Type;
                   Cost      : Hera.Money.Money_Type))
   is
   begin
      for Resource of Sector.Resources loop
         if Facility.Resources (Resource.Get_Resource_Class) then
            declare
               Factor   : constant Signed_Unit_Real :=
                 Signed_Unit_Clamp
                   (Hera.Random.Normal_Random (0.1));
               Quantity : constant Hera.Quantities.Quantity_Type :=
                 Hera.Quantities.Min
                   (Sector.Remaining (Resource),
                    Hera.Quantities.Scale
                      (Sector.Yield (Resource),
                       Efficiency * (1.0 + Factor) * 10.0));
            begin
               Sector.Mine_Resources (Resource, Quantity);
               On_Extraction
                 (Resource, Quantity, Hera.Money.Zero);
            end;
         end if;
      end loop;
   end Extract_Resources;

   -----------------------
   -- Update_Colony_Hub --
   -----------------------

   procedure Update_Colony_Hub
     (Efficiency    : Unit_Real;
      On_Production : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Quantity  : Hera.Quantities.Quantity_Type;
                   Cost      : Hera.Money.Money_Type))
   is
   begin
      for Item of Hera.Commodities.Services loop
         declare
            use Hera.Quantities;
            Quality       : constant Quality_Type := Item.Quality;
            Base_Quantity : constant Quantity_Type := To_Quantity (160.0);
            Factor        : constant Unit_Real :=
              (case Quality is
                  when 1 => 1.0,
                  when 2 => 1.0 / 4.0,
                  when 3 => 1.0 / 16.0);
         begin
            On_Production
              (Item, Scale (Base_Quantity, Efficiency * Factor),
               Hera.Money.Zero);
         end;
      end loop;
   end Update_Colony_Hub;

end Hera.Facilities.Updates;
