with Ada.Containers.Vectors;

with Hera.Calendar;
with Hera.Random;

with Hera.Updates.Events;

with Hera.Installations.Updates;

package body Hera.Installations is

   Installation_Version : constant Hera.Objects.Object_Version := "0.0.1";

   package Installation_Vectors is
     new Ada.Containers.Vectors (Positive, Installation_Type);

   Vector : Installation_Vectors.Vector;

   type Clear_Queue_Update is
     new Hera.Objects.Root_Update_Type with null record;

   overriding procedure Execute
     (Update : Clear_Queue_Update;
      Target : in out Hera.Objects.Root_Hera_Object'Class);

   overriding function Description
     (Update : Clear_Queue_Update)
      return String
   is ("clear queue");

   type Add_To_Queue_Update is
     new Hera.Objects.Root_Update_Type with
      record
         Item     : Hera.Facilities.Production_Item;
         Priority : Natural;
      end record;

   overriding procedure Execute
     (Update : Add_To_Queue_Update;
      Target : in out Hera.Objects.Root_Hera_Object'Class);

   overriding function Description
     (Update : Add_To_Queue_Update)
      return String
   is ("add " & Hera.Quantities.Show (Update.Item.Quantity)
       & " " & Update.Item.Commodity.Tag & " to queue");

   type Installation_Update_Type is
     new Hera.Updates.Update_Interface with
      record
         Installation : Installation_Type;
      end record;

   overriding procedure Activate
     (Update : Installation_Update_Type);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Update : Installation_Update_Type)
   is
   begin
      Update.Installation.Log
        (Update.Installation.Facility.Tag
         & " activating");

      Hera.Installations.Updates.Hire_Workers (Update.Installation);
      Hera.Installations.Updates.Pay_Workers (Update.Installation);
      Hera.Installations.Updates.Execute_Production (Update.Installation);

      Hera.Updates.Events.Update_With_Delay
        (Hera.Calendar.Days (1), Update);

   end Activate;

   ------------------
   -- Add_To_Queue --
   ------------------

   procedure Add_To_Queue
     (Installation : Root_Installation_Type'Class;
      Commodity    : Hera.Commodities.Commodity_Type;
      Quantity     : Hera.Quantities.Quantity_Type;
      Priority     : Natural := 0)
   is
   begin
      Installation.Add_Update
        (Add_To_Queue_Update'
           (Hera.Objects.Root_Update_Type with
                (Commodity, Quantity), Priority));
   end Add_To_Queue;

   -----------------
   -- Clear_Queue --
   -----------------

   procedure Clear_Queue (Installation : Root_Installation_Type'Class) is
   begin
      Installation.Add_Update
        (Clear_Queue_Update'(Hera.Objects.Root_Update_Type with null record));
   end Clear_Queue;

   ------------
   -- Create --
   ------------

   procedure Create
     (Owner    : Hera.Corporations.Corporation_Type;
      Colony   : Hera.Colonies.Colony_Type;
      Facility : Hera.Facilities.Facility_Type)
   is
      New_Item : Root_Installation_Type := Root_Installation_Type'
        (Hera.Objects.Root_Hera_Object with
         Owner    => Owner,
         Colony   => Colony,
         Sector   => Colony.Sector,
         Stock    => <>,
         Facility => Facility,
         Queue    => <>);
   begin
      New_Item.Initialize (Installation_Version);
      New_Item.Stock.Initialize_Stock_List (New_Item.Identifier);

      declare
         Item : constant Installation_Type :=
           Installation_Type (New_Item.Save_Object);
      begin
         Vector.Append (Item);
         Hera.Updates.Events.Update_At
           (Hera.Calendar.Delay_Days
              (Hera.Random.Unit_Random),
            Installation_Update_Type'
              (Installation => Item));
      end;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Add_To_Queue_Update;
      Target : in out Hera.Objects.Root_Hera_Object'Class)
   is
   begin
      Root_Installation_Type (Target).Queue.Insert
        (Update.Priority, Update.Item);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Clear_Queue_Update;
      Target : in out Hera.Objects.Root_Hera_Object'Class)
   is
      pragma Unreferenced (Update);
   begin
      Root_Installation_Type (Target).Queue.Clear;
   end Execute;

   ---------------
   -- Get_Stock --
   ---------------

   overriding function Get_Stock
     (From      : Root_Installation_Type;
      Commodity : Hera.Commodities.Commodity_Type)
      return Hera.Commodities.Stock_Entry
   is
   begin
      return From.Stock.Get_Stock (Commodity);
   end Get_Stock;

   ---------------------
   -- Queued_Capacity --
   ---------------------

   function Queued_Capacity
     (Installation : Root_Installation_Type'Class)
      return Hera.Quantities.Quantity_Type
   is
      use Hera.Quantities;
   begin
      return Capacity : Quantity_Type := Zero do
         declare
            procedure Update
              (Commodity : Hera.Commodities.Commodity_Type;
               Quantity  : Hera.Quantities.Quantity_Type);

            ------------
            -- Update --
            ------------

            procedure Update
              (Commodity : Hera.Commodities.Commodity_Type;
               Quantity  : Hera.Quantities.Quantity_Type)
            is
            begin
               Capacity := Capacity
                 + Commodity.Complexity * Quantity;
            end Update;

         begin
            Installation.Queue.Iterate (Update'Access);
         end;
      end return;
   end Queued_Capacity;

   -------------------
   -- Scan_By_Owner --
   -------------------

   procedure Scan_By_Owner
     (Owner   : Hera.Corporations.Corporation_Type;
      Process : not null access
        procedure (Installation : Installation_Type))
   is
      use type Hera.Corporations.Corporation_Type;
   begin
      for Item of Vector loop
         if Item.Owner = Owner then
            Process (Item);
         end if;
      end loop;
   end Scan_By_Owner;

   ----------------
   -- Scan_Stock --
   ----------------

   overriding procedure Scan_Stock
     (List      : Root_Installation_Type;
      Process   : not null access
        procedure (Commodity : Hera.Commodities.Commodity_Type;
                   Stock : Hera.Commodities.Stock_Entry))
   is
   begin
      List.Stock.Scan_Stock (Process);
   end Scan_Stock;

   ---------------
   -- Set_Stock --
   ---------------

   overriding procedure Set_Stock
     (To        : Root_Installation_Type;
      Commodity : Hera.Commodities.Commodity_Type;
      Stock     : Hera.Commodities.Stock_Entry)
   is
   begin
      To.Stock.Set_Stock (Commodity, Stock);
   end Set_Stock;

   ------------------
   -- Update_Stock --
   ------------------

   overriding procedure Update_Stock
     (Installation : in out Root_Installation_Type;
      Commodity    : Hera.Commodities.Commodity_Type;
      Stock        : Hera.Commodities.Stock_Entry)
   is
   begin
      Installation.Stock.Update_Stock (Commodity, Stock);
   end Update_Stock;

end Hera.Installations;
