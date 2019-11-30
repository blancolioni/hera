with Ada.Containers.Doubly_Linked_Lists;

with Hera.Logging;
with Hera.Profiling;

with Hera.Corporations.Updates;
with Hera.Installations.Updates;
with Hera.Markets;

with Hera.Handles.Employer;
with Hera.Handles.Installation.Selections;
with Hera.Handles.Population.Selections;
with Hera.Handles.Stock_Item.Selections;
with Hera.Handles.Warehouse.Selections;

package body Hera.Colonies.Updates is

   use Hera.Profiling;

   procedure Sell_Goods
     (Handle : Colony_Handle);

   procedure Check_Hires
     (Handle : Colony_Handle);

   procedure Check_Resignations
     (Handle : Colony_Handle);

   procedure Clear_Transients
     (Handle : Colony_Handle);

   procedure Execute_Production
     (Handle : Colony_Handle);

   procedure Pay_Workers
     (Handle : Colony_Handle);

   procedure Check_Needs
     (Handle : Colony_Handle);

   -----------------
   -- Check_Hires --
   -----------------

   procedure Check_Hires
     (Handle : Colony_Handle)
   is
      use Hera.Handles.Installation.Selections;
   begin
      Start ("colony-check-hires");
      for Installation of Select_Where (Colony (Handle)) loop
         Hera.Installations.Updates.Hire_Workers (Installation);
      end loop;
      Stop ("colony-check-hires");
   end Check_Hires;

   -----------------
   -- Check_Needs --
   -----------------

   procedure Check_Needs
     (Handle : Colony_Handle)
   is
      use Hera.Handles.Population.Selections;
   begin
      Start ("colony-check-needs");
      for Pop of Select_Where (Colony (Handle)) loop
         Hera.Pops.Buy_Needs (Pop);
      end loop;
      Stop ("colony-check-needs");
   end Check_Needs;

   ------------------------
   -- Check_Resignations --
   ------------------------

   procedure Check_Resignations
     (Handle : Colony_Handle)
   is

      use Hera.Handles.Population.Selections;

      type Resignation_Record is
         record
            Pop      : Hera.Pops.Population_Handle;
            Quantity : Hera.Quantities.Quantity_Type;
         end record;

      package Resignation_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Resignation_Record);

      Resignations : Resignation_Lists.List;

   begin

      Start ("colony-check-resignations");

      for Population of Select_Where
        (Colony (Handle)
         and Employer /= Hera.Handles.Employer.Empty_Handle)
      loop

         declare
            use Hera.Quantities;
            Total       : constant Quantity_Type :=
              Population.Quantity;
            Quit_Chance : constant Unit_Real :=
              1.0 - Population.Happiness;
            Quit_Total  : constant Quantity_Type :=
              Scale (Total, Quit_Chance);
         begin
            if Quit_Total > Zero then
               Resignations.Append
                 ((Population, Quit_Total));
            end if;
         end;

      end loop;

      for Resignation of Resignations loop
         declare
            Pop : constant Hera.Pops.Population_Handle := Resignation.Pop;
            Quantity : constant Hera.Quantities.Quantity_Type :=
                         Resignation.Quantity;
         begin
            Hera.Logging.Log
              (Pop,
               Hera.Quantities.Show (Quantity)
               & " " & Pop.Pop_Class.Tag
               & " quit employer"
               & " owned by "
               & Pop.Employer.Corporation.Name
               & " over happiness"
               & Natural'Image (Natural (Pop.Happiness * 100.0)));

            Hera.Pops.Quit (Pop, Quantity);
         end;
      end loop;

      Stop ("colony-check-resignations");

   end Check_Resignations;

   ----------------------
   -- Clear_Transients --
   ----------------------

   procedure Clear_Transients
     (Handle : Colony_Handle)
   is
      use Hera.Handles.Stock_Item.Selections;
      use Hera.Handles.Warehouse.Selections;
   begin

      Start ("colony-clear-transients");

      for Warehouse of Select_Where (Colony (Handle)) loop
         for Stock_Item of Select_Where (Has_Stock (Warehouse)) loop
            if Stock_Item.Commodity.Transient then
               Stock_Item.Update.Set_Quantity (Hera.Quantities.Zero).Done;
            end if;
         end loop;
      end loop;

      Stop ("colony-clear-transients");

   end Clear_Transients;

   ------------------------
   -- Execute_Production --
   ------------------------

   procedure Execute_Production
     (Handle : Colony_Handle)
   is
      use Hera.Handles.Installation.Selections;
   begin

      Start ("colony-execute-production");

      for Installation of Select_Where (Colony (Handle)) loop
         Hera.Installations.Updates.Execute_Production (Installation);
      end loop;

      Stop ("colony-execute-production");

   end Execute_Production;

   -----------------
   -- Pay_Workers --
   -----------------

   procedure Pay_Workers
     (Handle : Colony_Handle)
   is
      use Hera.Handles.Installation.Selections;
   begin

      Start ("colony-pay-workers");

      for Installation of Select_Where (Colony (Handle)) loop
         Hera.Installations.Updates.Pay_Workers (Installation);
      end loop;

      Stop ("colony-pay-workers");

   end Pay_Workers;

   ----------------
   -- Sell_Goods --
   ----------------

   procedure Sell_Goods
     (Handle : Colony_Handle)
   is
      use Hera.Handles.Warehouse.Selections;
   begin

      Start ("colony-sell-goods");

      for Warehouse of Select_Where (Colony (Handle)) loop
         Hera.Corporations.Updates.Sell_Commodities
           (Corporation => Warehouse.Corporation,
            Colony      => Handle,
            From        => Warehouse);
      end loop;

      Stop ("colony-sell-goods");

   end Sell_Goods;

   ------------
   -- Update --
   ------------

   procedure Update (Handle : Colony_Handle) is
   begin
      Sell_Goods (Handle);
      Check_Hires (Handle);
      Pay_Workers (Handle);
      Clear_Transients (Handle);
      Execute_Production (Handle);
      Check_Needs (Handle);
      Check_Resignations (Handle);
      Hera.Markets.Clear_Transients (Handle.Market);
   end Update;

end Hera.Colonies.Updates;
