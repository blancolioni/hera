with Hera.Quantities;

with Hera.Commodities;
with Hera.Installations;

with Hera.Db;

with Hera.Handles.Building_Module.Selections;
with Hera.Handles.Component.Selections;
with Hera.Handles.Consumer_Good.Selections;
with Hera.Handles.Resource.Selections;
with Hera.Handles.Sector_Deposit.Selections;
with Hera.Handles.Factory.Selections;
with Hera.Handles.Industrial_Good.Selections;
with Hera.Handles.Installation.Selections;
with Hera.Handles.Planet_Sector;

package body Hera.Corporations.Manager is

   procedure Manage_Factory_Queues
     (Corp : Corporation_Class);

   procedure Manage_Factory_Queue
     (Installation : Hera.Handles.Installation.Installation_Class;
      Factory      : Hera.Handles.Factory.Factory_Class);

   procedure Queue_Building_Modules
     (Installation : Hera.Handles.Installation.Installation_Class);

   procedure Queue_Industrial_Goods
     (Installation : Hera.Handles.Installation.Installation_Class);

   procedure Queue_Consumer_Goods
     (Installation : Hera.Handles.Installation.Installation_Class);

   procedure Queue_Commodity
     (Installation : Hera.Handles.Installation.Installation_Class;
      Commodity    : Hera.Commodities.Commodity_Class;
      Quantity     : Hera.Quantities.Quantity_Type);

   function Matching_Deposits
     (Sector    : Hera.Handles.Planet_Sector.Planet_Sector_Class;
      Commodity : Hera.Commodities.Commodity_Class)
      return Boolean;

   ------------
   -- Manage --
   ------------

   procedure Manage
     (Corporation : Corporation_Class)
   is
   begin
      Manage_Factory_Queues (Corporation);
   end Manage;

   --------------------------
   -- Manage_Factory_Queue --
   --------------------------

   procedure Manage_Factory_Queue
     (Installation : Hera.Handles.Installation.Installation_Class;
      Factory      : Hera.Handles.Factory.Factory_Class)
   is
   begin
      if Factory.Construction then
         Queue_Building_Modules (Installation);
      end if;
      if Factory.Industrial then
         Queue_Industrial_Goods (Installation);
      end if;
      if Factory.Consumer then
         Queue_Consumer_Goods (Installation);
      end if;
   end Manage_Factory_Queue;

   ---------------------------
   -- Manage_Factory_Queues --
   ---------------------------

   procedure Manage_Factory_Queues
     (Corp : Corporation_Class)
   is
      use Hera.Db;
      use Hera.Handles.Factory.Selections;
      use Hera.Handles.Installation.Selections;
   begin
      for Installation of Select_Where (Corporation (Corp)) loop
         if Installation.Facility.Top_Record = R_Factory then
            Manage_Factory_Queue
              (Installation,
               Hera.Handles.Factory.Selections.First_Where
                 (Tag (Installation.Facility.Tag)));
         end if;
      end loop;
   end Manage_Factory_Queues;

   -----------------------
   -- Matching_Deposits --
   -----------------------

   function Matching_Deposits
     (Sector    : Hera.Handles.Planet_Sector.Planet_Sector_Class;
      Commodity : Hera.Commodities.Commodity_Class)
      return Boolean
   is
      package Selections renames
        Hera.Handles.Component.Selections;
   begin
      for Commodity_Component of Selections.Select_Where
        (Selections.Commodity (Commodity))
      loop
         declare
            use Hera.Handles.Resource;
            use Hera.Handles.Resource.Selections;
            use Hera.Handles.Sector_Deposit;
            use Hera.Handles.Sector_Deposit.Selections;
            Resource : constant Resource_Handle :=
              First_Where (Tag (Commodity_Component.Component.Tag));
            Deposit : constant Sector_Deposit_Handle :=
              First_Where
                (Sector_Deposit (Sector, Resource));
         begin
            if not Resource.Has_Element
              or else not Deposit.Has_Element
            then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Matching_Deposits;

   ----------------------------
   -- Queue_Building_Modules --
   ----------------------------

   procedure Queue_Building_Modules
     (Installation : Hera.Handles.Installation.Installation_Class)
   is
      use Hera.Handles.Building_Module.Selections;
   begin
      for Item of Select_All loop
         Queue_Commodity (Installation, Item, Hera.Quantities.Unit);
      end loop;
   end Queue_Building_Modules;

   ---------------------
   -- Queue_Commodity --
   ---------------------

   procedure Queue_Commodity
     (Installation : Hera.Handles.Installation.Installation_Class;
      Commodity    : Hera.Commodities.Commodity_Class;
      Quantity     : Hera.Quantities.Quantity_Type)

   is
   begin
      Hera.Installations.Add_To_Queue
        (Installation => Installation,
         Commodity    => Commodity,
         Quantity     => Quantity);
   end Queue_Commodity;

   --------------------------
   -- Queue_Consumer_Goods --
   --------------------------

   procedure Queue_Consumer_Goods
     (Installation : Hera.Handles.Installation.Installation_Class)
   is
      use Hera.Handles.Consumer_Good.Selections;
   begin
      for Item of Select_All loop
         if Matching_Deposits
           (Installation.Colony.Planet_Sector, Item)
         then
            Queue_Commodity (Installation, Item,
                             Installation.Facility.Capacity);
         end if;
      end loop;
   end Queue_Consumer_Goods;

   ----------------------------
   -- Queue_Industrial_Goods --
   ----------------------------

   procedure Queue_Industrial_Goods
     (Installation : Hera.Handles.Installation.Installation_Class)
   is
      use Hera.Handles.Industrial_Good.Selections;
   begin
      for Item of Select_All loop
         if Matching_Deposits
           (Installation.Colony.Planet_Sector, Item)
         then
            Queue_Commodity
              (Installation, Item,
               Installation.Facility.Capacity);
         end if;
      end loop;
   end Queue_Industrial_Goods;

end Hera.Corporations.Manager;
