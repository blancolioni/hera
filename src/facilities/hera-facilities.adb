with WL.String_Maps;

package body Hera.Facilities is

   package Facility_Maps is
     new WL.String_Maps (Facility_Type);

   Map : Facility_Maps.Map;

   ------------
   -- Append --
   ------------

   procedure Append
     (Queue : in out Production_Queue;
      Item  : Production_Item)
   is
   begin
      Queue.Queue.Append ((Item, 0));
      Queue.Changed := True;
   end Append;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Queue : in out Production_Queue)
   is
   begin
      Queue.Queue.Clear;
      Queue.Changed := False;
   end Clear;

   -------------------
   -- Clear_Changed --
   -------------------

   procedure Clear_Changed
     (Queue : in out Production_Queue)
   is
   begin
      Queue.Changed := False;
   end Clear_Changed;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First
     (Queue : in out Production_Queue)
   is
   begin
      Queue.Queue.Delete_First;
      Queue.Changed := True;
   end Delete_First;

   ------------
   -- Exists --
   ------------

   function Exists (Tag : String) return Boolean is
   begin
      return Map.Contains (Tag);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Tag : String) return Facility_Type is
   begin
      return Map.Element (Tag);
   end Get;

   ---------------
   -- Get_Class --
   ---------------

   function Get_Class (Class : Facility_Class) return Facility_Array is
      Result : Facility_Array (1 .. Natural (Map.Length));
      Count  : Natural := 0;
   begin
      for Facility of Map loop
         if Facility.Class = Class then
            Count := Count + 1;
            Result (Count) := Facility;
         end if;
      end loop;
      return Result (1 .. Count);
   end Get_Class;

   --------------------
   -- Get_Extractors --
   --------------------

   function Get_Extractors
     (Resource : Hera.Commodities.Commodity_Type)
      return Facility_Array
   is
      Result : Facility_Array (1 .. 20);
      Count  : Natural := 0;
   begin
      for Facility of Map loop
         if Facility.Class = Extractor
           and then Facility.Resources (Resource.Get_Resource_Class)
         then
            Count := Count + 1;
            Result (Count) := Facility;
         end if;
      end loop;
      return Result (1 .. Count);
   end Get_Extractors;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Queue    : in out Production_Queue;
      Priority : Natural;
      Item     : Production_Item)
   is
      use Production_Queues;
      Position : Cursor := Queue.Queue.First;
   begin
      while Has_Element (Position)
        and then Element (Position).Priority >= Priority
      loop
         Next (Position);
      end loop;
      if Has_Element (Position) then
         Queue.Queue.Insert (Position, (Item, Priority));
      else
         Queue.Queue.Append ((Item, Priority));
      end if;
      Queue.Changed := True;
   end Insert;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Queue   : Production_Queue;
      Process : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Quantity  : Hera.Quantities.Quantity_Type))
   is
   begin
      for Item of Queue.Queue loop
         Process (Item.Item.Commodity, Item.Item.Quantity);
      end loop;
   end Iterate;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Facility : in out Root_Facility_Type; From : Tropos.Configuration)
   is
   begin
      Hera.Objects.Root_Localised_Object (Facility).Load (From);
   end Load;

   -------------
   -- Modules --
   -------------

   function Modules
     (Facility : Root_Facility_Type)
      return Hera.Commodities.Commodity_Array
   is
      use Facility_Module_Lists;
      Position : Cursor := Facility.Modules.First;
   begin
      return Arr : Hera.Commodities.Commodity_Array
        (1 .. Natural (Facility.Modules.Length))
      do
         for I in Arr'Range loop
            Arr (I) := Element (Position).Module;
            Next (Position);
         end loop;
      end return;
   end Modules;

   ------------------
   -- New_Facility --
   ------------------

   procedure New_Facility (Facility : Facility_Type) is
   begin
      Map.Insert (Facility.Tag, Facility);
   end New_Facility;

   --------------------
   -- Produces_Class --
   --------------------

   function Produces_Class
     (Facility : Root_Facility_Type;
      Class    : Hera.Commodities.Commodity_Class)
      return Boolean
   is
      use all type Hera.Commodities.Commodity_Class;
   begin
      case Facility.Class is
         when Colony_Hub =>
            return Class = Market_Token;
         when Orbital_Dock =>
            return Class = Market_Token;
         when Factory =>
            return Facility.Production (Class);
         when Extractor =>
            return Class = Resource;
         when Service =>
            return Class = Service_Token;
      end case;
   end Produces_Class;

   --------------
   -- Quantity --
   --------------

   function Quantity
     (Facility  : Root_Facility_Type;
      Module    : Hera.Commodities.Commodity_Type)
      return Hera.Quantities.Quantity_Type
   is
      use type Hera.Commodities.Commodity_Type;
   begin
      for Item of Facility.Modules loop
         if Item.Module = Module then
            return Item.Quantity;
         end if;
      end loop;
      return Hera.Quantities.Zero;
   end Quantity;

   --------------
   -- Quantity --
   --------------

   function Quantity
     (Facility : Root_Facility_Type;
      Worker   : Hera.Pops.Classes.Pop_Class_Type)
      return Hera.Quantities.Quantity_Type
   is
      use type Hera.Pops.Classes.Pop_Class_Type;
   begin
      for Item of Facility.Employees loop
         if Item.Pop_Class = Worker then
            return Item.Quantity;
         end if;
      end loop;
      return Hera.Quantities.Zero;
   end Quantity;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Facility : Root_Facility_Type; To : in out Tropos.Configuration)
   is
   begin
      Hera.Objects.Root_Localised_Object (Facility).Save (To);
   end Save;

   -------------
   -- Workers --
   -------------

   function Workers
     (Facility : Root_Facility_Type)
      return Hera.Pops.Classes.Pop_Class_Array
   is
      use Facility_Worker_Lists;
      Position : Cursor := Facility.Employees.First;
   begin
      return Arr : Hera.Pops.Classes.Pop_Class_Array
        (1 .. Natural (Facility.Employees.Length))
      do
         for I in Arr'Range loop
            Arr (I) := Element (Position).Pop_Class;
            Next (Position);
         end loop;
      end return;
   end Workers;

end Hera.Facilities;
